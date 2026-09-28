;; Conformance tests: drive a running cl-nrepl server with the official Clojure
;; nREPL client (nrepl.core), checking that responses have the shapes real
;; clients rely on.
;;
;; Usage: java -cp <clojure + nrepl jars> clojure.main conformance.clj PORT
;; (run.sh takes care of all of that.)

(ns conformance
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [nrepl.core :as nrepl]
            [nrepl.transport :as transport]))

(def port (Integer/parseInt (first *command-line-args*)))
(def timeout 10000)

(defn- with-client* [f]
  (with-open [conn (nrepl/connect :port port)]
    (f conn (nrepl/client conn timeout))))

(defmacro with-client [[conn client] & body]
  `(with-client* (fn [~conn ~client] ~@body)))

(defn- new-session [client]
  (-> (nrepl/message client {:op "clone"}) first :new-session))

(defn- session-message [client session msg]
  (doall (nrepl/message client (assoc msg :session session))))

(defn- statuses [responses]
  (set (mapcat :status responses)))

(defn- recv-until
  "Read from transport `t` until a message for `id` has `status`, returning
  every message read."
  [t id status]
  (loop [acc []]
    (let [m (transport/recv t timeout)]
      (assert m (str "timed out waiting for " status " on " id))
      (let [acc (conj acc m)]
        (if (and (= id (:id m)) (some #{status} (:status m)))
          acc
          (recur acc))))))

(defn- recv-until-done
  "Read from transport `t` until every id in `ids` has sent \"done\", returning
  every message read."
  [t ids]
  (loop [acc [] pending (set ids)]
    (if (empty? pending)
      acc
      (let [m (transport/recv t timeout)]
        (assert m (str "timed out waiting for " pending))
        (recur (conj acc m)
               (if (some #{"done"} (:status m)) (disj pending (:id m)) pending))))))

(defn- for-id [id msgs] (filter #(= id (:id %)) msgs))


(deftest describe-test
  (with-client [_ client]
    (let [d (first (nrepl/message client {:op "describe"}))]
      (is (= #{"done"} (set (:status d))))
      (doseq [op [:eval :load-file :interrupt :stdin :clone :close :ls-sessions
                  :describe :completions :lookup]]
        (is (contains? (:ops d) op) (str "missing op " op)))
      (is (string? (get-in d [:versions :cl-nrepl :version-string]))))))

(deftest eval-test
  (with-client [_ client]
    (let [r (nrepl/combine-responses (nrepl/message client {:op "eval" :code "(+ 1 2)"}))]
      (is (= ["3"] (:value r)))
      (is (= "COMMON-LISP-USER" (:ns r)))
      (is (= #{"done"} (:status r))))
    (testing "response-values reads each form's value"
      ;; (Common Lisp prints the keyword upcased.)
      (is (= [1 2 :THREE]
             (nrepl/response-values
               (nrepl/message client {:op "eval" :code "1 2 :three"})))))
    (testing "output"
      (let [r (nrepl/combine-responses
                (nrepl/message client {:op "eval" :code "(progn (princ \"hi\") (terpri) (princ \"err\" *error-output*) 42)"}))]
        (is (= "hi\n" (:out r)))
        (is (= "err" (:err r)))
        (is (= ["42"] (:value r)))))
    (testing "ns param"
      (is (= "NREPL" (:ns (nrepl/combine-responses
                            (nrepl/message client {:op "eval" :code "1" :ns "nrepl"}))))))))

(deftest eval-error-test
  (with-client [_ client]
    (let [responses (nrepl/message client {:op "eval" :code "(/ 1 0)"})
          r (nrepl/combine-responses responses)]
      (is (contains? (:status r) "eval-error"))
      (is (contains? (:status r) "done"))
      (is (= "COMMON-LISP:DIVISION-BY-ZERO" (:ex r)))
      (is (= "COMMON-LISP:DIVISION-BY-ZERO" (:root-ex r)))
      (is (seq (:err r))))
    (testing "missing code"
      (is (= #{"done" "no-code" "error"}
             (statuses (nrepl/message client {:op "eval"})))))))

(deftest session-test
  (with-client [_ client]
    (let [s (new-session client)]
      (is (string? s))
      (session-message client s {:op "eval" :code "(in-package :nrepl)"})
      (is (= "NREPL" (:ns (nrepl/combine-responses
                            (session-message client s {:op "eval" :code "1"})))))
      (is (some #{s} (-> (nrepl/message client {:op "ls-sessions"}) first :sessions)))
      (is (= #{"done" "session-closed"}
             (statuses (session-message client s {:op "close"}))))
      (is (= #{"done" "unknown-session" "error"}
             (statuses (session-message client s {:op "eval" :code "1"})))))))

(deftest client-session-test
  ;; nrepl.core's own session helper: clones, then routes messages through it.
  (with-client [_ client]
    (let [session (nrepl/client-session client)]
      ;; Responses are lazy and filtered by session, so consume this one fully
      ;; or its value turns up in the next call's.
      (doall (session {:op "eval" :code "(defparameter *x* 41)"}))
      (is (= [42] (nrepl/response-values (session {:op "eval" :code "(1+ *x*)"})))))))

(deftest load-file-test
  (with-client [_ client]
    (let [r (nrepl/combine-responses
              (nrepl/message client {:op "load-file"
                                     :file "(defun conformance-fn () :loaded) (conformance-fn)"
                                     :file-name "conformance.lisp"
                                     :file-path "conformance.lisp"}))]
      (is (= [":LOADED"] (:value r)))
      (is (= #{"done"} (:status r))))))

(deftest interrupt-test
  (with-open [t (nrepl/connect :port port)]
    (transport/send t {:op "clone" :id "clone"})
    (let [s (-> (recv-until t "clone" "done") last :new-session)]
      (transport/send t {:op "eval" :id "e1" :session s
                         :code "(progn (print :started) (finish-output) (loop (sleep 0.01)))"})
      ;; Wait for output so the eval is known to be running.
      (loop [] (let [m (transport/recv t timeout)] (when-not (:out m) (recur))))
      (transport/send t {:op "interrupt" :id "i1" :session s :interrupt-id "e1"})
      (let [msgs (recv-until-done t ["i1" "e1"])]
        (is (some #(some #{"interrupted"} (:status %)) (for-id "i1" msgs)))
        (is (some #(= #{"done" "interrupted"} (set (:status %))) (for-id "e1" msgs))))
      (testing "idle"
        (transport/send t {:op "interrupt" :id "i2" :session s})
        (is (= ["done" "session-idle"] (:status (last (recv-until t "i2" "done")))))))))

(deftest stdin-test
  (with-open [t (nrepl/connect :port port)]
    (transport/send t {:op "clone" :id "clone"})
    (let [s (-> (recv-until t "clone" "done") last :new-session)]
      (transport/send t {:op "eval" :id "e1" :session s :code "(read-line)"})
      (is (some #(some #{"need-input"} (:status %)) (recv-until t "e1" "need-input")))
      (transport/send t {:op "stdin" :id "s1" :session s :stdin "typed line\n"})
      (let [msgs (filter #(= "e1" (:id %)) (recv-until t "e1" "done"))]
        (is (some #(= "\"typed line\"\nNIL" (:value %)) msgs))))))

(deftest completions-test
  (with-client [_ client]
    (let [cs (-> (nrepl/message client {:op "completions" :prefix "mapc"}) first :completions)
          mapcar (first (filter #(= "mapcar" (:candidate %)) cs))]
      (is (= "function" (:type mapcar)))
      (is (= "COMMON-LISP" (:ns mapcar))))
    (is (= [] (-> (nrepl/message client {:op "completions" :prefix "zzqq-nothing"}) first :completions)))))

(deftest lookup-test
  (with-client [_ client]
    (let [info (-> (nrepl/message client {:op "lookup" :sym "mapcar"}) first :info)]
      (is (= "mapcar" (:name info)))
      (is (= "COMMON-LISP" (:ns info)))
      (is (string? (:arglists-str info)))
      (is (string? (:doc info))))))

(deftest unknown-op-test
  (with-client [_ client]
    (is (= #{"done" "unknown-op" "error"}
           (statuses (nrepl/message client {:op "frobnicate"}))))))

(let [{:keys [fail error]} (run-tests 'conformance)]
  (shutdown-agents)
  (System/exit (if (zero? (+ fail error)) 0 1)))
