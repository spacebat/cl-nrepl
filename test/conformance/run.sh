#!/bin/sh
# Run the conformance tests: start a cl-nrepl server in SBCL and drive it with
# the official Clojure nREPL client.  Needs java, sbcl and Quicklisp (with this
# repository where Quicklisp can find it).  The Clojure and nREPL jars are
# downloaded to $CACHE on first run.
set -e

HERE=$(cd "$(dirname "$0")" && pwd)
QUICKLISP_SETUP=${QUICKLISP_SETUP:-"$HOME/quicklisp/setup.lisp"}
CACHE=${CACHE:-"$HOME/.cache/cl-nrepl-conformance"}
CLOJURE_VERSION=1.12.0
NREPL_VERSION=1.7.0

mkdir -p "$CACHE"
fetch() {
    [ -f "$CACHE/$(basename "$1")" ] || curl -sSfL --retry 5 --retry-delay 3 -o "$CACHE/$(basename "$1")" "$1"
}
fetch https://repo1.maven.org/maven2/org/clojure/clojure/$CLOJURE_VERSION/clojure-$CLOJURE_VERSION.jar
fetch https://repo1.maven.org/maven2/org/clojure/spec.alpha/0.5.238/spec.alpha-0.5.238.jar
fetch https://repo1.maven.org/maven2/org/clojure/core.specs.alpha/0.4.74/core.specs.alpha-0.4.74.jar
fetch https://repo.clojars.org/nrepl/nrepl/$NREPL_VERSION/nrepl-$NREPL_VERSION.jar
CLASSPATH=$(echo "$CACHE"/*.jar | tr ' ' :)

# Start the server on a free port and wait for it to tell us which.
PORT_FILE=$(mktemp)
trap 'kill $SERVER 2>/dev/null; rm -f "$PORT_FILE"' EXIT
sbcl --noinform --non-interactive \
     --load "$QUICKLISP_SETUP" \
     --eval '(ql:quickload :nrepl :silent t)' \
     --eval "(with-open-file (f \"$PORT_FILE\" :direction :output :if-exists :supersede)
               (princ (nth-value 1 (nrepl:start-server :port 0)) f))" \
     --eval '(loop (sleep 60))' >/dev/null 2>&1 &
SERVER=$!
for _ in $(seq 1 300); do
    [ -s "$PORT_FILE" ] && break
    sleep 0.2
done
[ -s "$PORT_FILE" ] || { echo "Server didn't start" >&2; exit 1; }

java -cp "$CLASSPATH" clojure.main "$HERE/conformance.clj" "$(cat "$PORT_FILE")"
