"""Cooperative interruption shared by local and remote engines."""


class MoveCancelled(Exception):
    pass


def check_cancel(cancel):
    if cancel is not None and cancel.is_set():
        raise MoveCancelled("Move rejected")
