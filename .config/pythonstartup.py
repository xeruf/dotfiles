# https://unix.stackexchange.com/questions/630642/change-location-of-python-history
import os
import atexit
import readline

history = os.path.join(os.path.expanduser('~'), '.local/state/python_history')
try:
    readline.read_history_file(history)
except OSError:
    pass

def write_history():
    try:
        readline.write_history_file(history)
    except OSError:
        pass

atexit.register(write_history)
