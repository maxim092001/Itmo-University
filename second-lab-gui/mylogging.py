import sys

_ENABLE = False


def log(func):
    def wrapped(*args, **kwargs):
        print("Call in", func.__name__, args, kwargs, file=sys.stderr)
        func(*args, **kwargs)
        print("Call out", func.__name__, args, kwargs, file=sys.stderr)

    return wrapped if _ENABLE else func
