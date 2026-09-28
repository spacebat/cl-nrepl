```
  ______  __          .__   __. .______       _______ .______    __
 /      ||  |         |  \ |  | |   _  \     |   ____||   _  \  |  |
|  ,----'|  |   ______|   \|  | |  |_)  |    |  |__   |  |_)  | |  |
|  |     |  |  |______|  . `  | |      /     |   __|  |   ___/  |  |
|  `----.|  `----.    |  |\   | |  |\  \----.|  |____ |  |      |  `----.
 \______||_______|    |__| \__| | _| `._____||_______|| _|      |_______|
```

A bare-bones [NREPL][] server for Common Lisp.

**Not ready yet, don't use unless you want to help me build it.**

## Usage

Put this repository somewhere Quicklisp/ASDF can find it (e.g.
`~/quicklisp/local-projects/`), then:

    (ql:quickload :nrepl)
    (nrepl:start-server)              ; listens on 127.0.0.1:8675
    (nrepl:start-server :port 0)      ; or pick a free port (returned as 2nd value)
    (nrepl:stop-server)

`sender.py` is a small interactive client for poking at a running server by
hand: `python3 sender.py 8675`.

## Protocol support

The server speaks the standard [nREPL protocol][ops] and is tested against the
official Clojure client.  Supported ops:

* `eval`, `load-file`: evaluate code in the session's package (or the one named
  by `ns`), streaming `out`/`err` and replying with a `value` and `ns` for each
  form.  Errors reply with `ex`/`root-ex` (the condition's type) and an
  `eval-error` status.
* `interrupt`, `stdin`: interrupt a running evaluation; supply input when an
  evaluation reads from `*standard-input*` (signalled by a `need-input` status).
* `clone`, `close`, `ls-sessions`: sessions keep their own `*package*` and REPL
  history variables (`*`, `**`, `+`, `/`...).  Each runs its evaluations in
  order on a thread of its own.
* `describe`, `completions`, `lookup`.
* `documentation`, `arglist`, `macroexpand`: specific to this server.

Some things differ from Clojure because Common Lisp does:

* Package names are reported as they are in Lisp, e.g. `"ns": "COMMON-LISP-USER"`.
  Any case is accepted from clients.
* A form returning several values gets them all in its `value`, one per line;
  a form returning none gets `; No values`.
* `ex`/`root-ex` are package-qualified condition types, like
  `COMMON-LISP:DIVISION-BY-ZERO`.
* An empty `stdin` message signals end of file.

[ops]: https://nrepl.org/nrepl/ops.html

## Tests

    (ql:quickload :nrepl/test)
    (asdf:test-system :nrepl)

The tests start a server on a free port and talk to it over a real socket.

`test/conformance/run.sh` drives a server with the official Clojure nREPL
client instead (it needs `java` too, and downloads the Clojure jars it uses).

[NREPL]: https://github.com/clojure/tools.nrepl/
