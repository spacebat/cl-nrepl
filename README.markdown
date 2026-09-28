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

## Tests

    (ql:quickload :nrepl/test)
    (asdf:test-system :nrepl)

The tests start a server on a free port and talk to it over a real socket.

[NREPL]: https://github.com/clojure/tools.nrepl/
