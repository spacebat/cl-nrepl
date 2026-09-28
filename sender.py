"""A tiny interactive client for poking at an nREPL server by hand.

Usage: python3 sender.py PORT

Type Lisp code to eval it, `\\d SYMBOL` to look up documentation, `\\{...}`
to send a raw message (a Python dict literal), or `quit` to exit.
"""
import ast
import socket
import sys
from pprint import pformat

try:
    from pygments import highlight
    from pygments.lexers import PythonLexer
    from pygments.formatters import TerminalFormatter
except ImportError:
    highlight = None


ADDRESS = 'localhost'
PORT = int(sys.argv[1])


# Bencode ---------------------------------------------------------------------
def bencode(obj):
    if isinstance(obj, int):
        return b'i%de' % obj
    if isinstance(obj, str):
        obj = obj.encode('utf-8')
    if isinstance(obj, bytes):
        return b'%d:%s' % (len(obj), obj)
    if isinstance(obj, (list, tuple)):
        return b'l' + b''.join(bencode(x) for x in obj) + b'e'
    if isinstance(obj, dict):
        return (b'd'
                + b''.join(bencode(k) + bencode(v)
                           for k, v in sorted(obj.items()))
                + b'e')
    raise TypeError('Cannot bencode %r' % (obj,))


def bdecode(data, i=0):
    """Decode one value from `data` at `i`, returning (value, next_i).

    Raises IndexError/ValueError if `data` holds an incomplete value.
    """
    c = data[i:i+1]
    if c == b'i':
        end = data.index(b'e', i)
        return int(data[i+1:end]), end + 1
    if c == b'l':
        i, items = i + 1, []
        while data[i:i+1] != b'e':
            if i >= len(data):
                raise IndexError('incomplete list')
            item, i = bdecode(data, i)
            items.append(item)
        return items, i + 1
    if c == b'd':
        i, d = i + 1, {}
        while data[i:i+1] != b'e':
            if i >= len(data):
                raise IndexError('incomplete dict')
            k, i = bdecode(data, i)
            v, i = bdecode(data, i)
            d[k] = v
        return d, i + 1
    if c.isdigit():
        colon = data.index(b':', i)
        start = colon + 1
        end = start + int(data[i:colon])
        if end > len(data):
            raise IndexError('incomplete string')
        return data[start:end].decode('utf-8'), end
    raise ValueError('bad bencode data at %d' % i)


def decode_all(buffer):
    """Decode every complete message in `buffer`, returning (messages, rest)."""
    messages, i = [], 0
    while i < len(buffer):
        try:
            message, i = bdecode(buffer, i)
        except (IndexError, ValueError):
            break
        messages.append(message)
    return messages, buffer[i:]


# REPL ------------------------------------------------------------------------
def build_eval(data):
    return {"op": "eval", "code": data.strip()}

def build_doc(data):
    return {"op": "documentation", "symbol": data.strip()}

def pprint(obj):
    # ...........………………_„-,-~''~''':::'':::':::::''::::''~
    # ………._,-'':::::::::::::::::::::::::::::::::::::::::::''-„
    # ………..,-':::::::::::::::::::::::::::::::::::::::::::::::
    # ………,-'::::::::::::::::::::::::::„:„„-~-~--'~-'~--~-~--~-
    # ……..,'::::::::::,~'': : : : : : : : : : : : : : : : '-|
    # ……..|::::::::,-': : : : : : : : - -~''''¯¯''-„: : : : :\
    # ……..|:::::::: : : : : : : : : _„„--~'''''~-„: : : : '|
    # ……..'|:::::::,': : : : : : :_„„-: : : : : : : : ~--„_: |'
    # ………|:::::: : : „--~~'''~~''''''''-„…_..„~''''''''''''¯|
    # ………|:::::,':_„„-|: : :_„---~: : ''¯¯''''|: ~---„_:   ||
    # ……..,~-,_/'': : : |: _ o__): : |: :: : : : _o__): \..|
    # ……../,'-,: : : : : ''-,_______,-'': : : : ''-„_____|
    # ……..\: : : : : : : : : : : : : : :„: : : : :-,: : :\
    # ………',:': : : : : : : : : : : : :,-'__: : : :_', ;: ,'
    # ……….'-,-': : : : : :___„-: : :'': : ¯''~~'': ': : ~--|'
    # ………….|: ,: : : : : : : : : : : : : : : : : : : :: :
    # ………….'|: \: : : : : : : : -,„_„„-~~--~--„_: :: : : |
    # …………..|: \: : : : : : : : : : : :-------~: : : : : |
    # …………..|: :''-,: : : : : : : : : : : : : : : : : :
    # …………..',: : :''-, : : : : : : : : : : : :  : :: ,'
    # ……………| : : : : : : : : :_ : : : : : : : : : : ,-'
    # ……………|: : : : : : : : : : '''~----------~''
    # …………._|: : : : : : : : : : : : : : : : : : :
    # ……….„-''. '-,_: : : : : : : : : : : : : : : : : ,'
    # ……,-''. . . . . '''~-„_: : : : : : : : : : : : :,-'''-„
    #              █▀█░█▀█░█▀█░█░█▄░█░▀█▀░
    #              █▀▀░█▀▀░█▀▄░█░█▀██░░█░░
    #              ▀░░░▀░░░▀░▀░▀░▀░░▀░░▀░░
    text = pformat(obj)
    if highlight:
        text = highlight(text, PythonLexer(), TerminalFormatter())
    print(text)

def repl():
    sock = socket.socket()
    sock.connect((ADDRESS, PORT))
    sock.settimeout(0.5)
    buffer = b''

    while True:
        data = input("> ")
        if data.strip():
            if data == 'quit':
                return

            if data.startswith('\\d'):
                sock.sendall(bencode(build_doc(data[2:])))
            elif data.startswith('\\'):
                sock.sendall(bencode(ast.literal_eval(data[1:])))
            else:
                sock.sendall(bencode(build_eval(data)))

        try:
            while True:
                incoming = sock.recv(1024*1024)
                if not incoming:
                    print("Server closed the connection.")
                    return
                buffer += incoming
                messages, buffer = decode_all(buffer)
                for m in messages:
                    pprint(m)
        except socket.timeout:
            pass


if __name__ == '__main__':
    repl()
