"""ac_heartbeat — tiny progress heartbeats for the Slab menubar.

Stdlib-only Python port of pop/lib/render-progress.mjs. Writes a JSON
heartbeat into ~/.ac-pop-renders/<id>.json that the Swift menubar polls
(every 2s) and renders as a temporary progress bar — then sweeps the
instant the file is gone or the pid dies.

Heartbeat shape (must match StateSnapshot.readPopRenders):
  { id, type, label, pct, done, total, pid, startedAt, updatedAt }
  type  — free string; the menubar shows it as a 6-char column label
  pct   — 0..100, or None for an indeterminate "·····" bar

Usage:
  hb = Heartbeat(type="gemma", label="e2b · describe")
  hb.begin()
  hb.update(42)                 # pct
  hb.update(60, done=12, total=20)
  hb.end()                      # or use as a context manager
"""
import atexit, json, os, signal, time
from pathlib import Path

RENDERS_DIR = Path.home() / ".ac-pop-renders"


class Heartbeat:
    def __init__(self, type="render", label=None):
        self.type = type
        self.label = label or type
        self.id = None
        self.file = None
        self.started = 0
        self._last = 0.0

    def begin(self, pct=0):
        RENDERS_DIR.mkdir(parents=True, exist_ok=True)
        self.started = int(time.time() * 1000)
        self.id = f"{self.type}-{os.getpid()}-{self.started:x}"
        self.file = RENDERS_DIR / f"{self.id}.json"
        self._write(pct)
        atexit.register(self.end)
        for sig in (signal.SIGINT, signal.SIGTERM):
            try:
                signal.signal(sig, self._on_signal)
            except (ValueError, OSError):
                pass  # not on main thread, etc.
        return self.id

    def _on_signal(self, signum, frame):
        self.end()
        os._exit(130 if signum == signal.SIGINT else 143)

    def update(self, pct=None, done=None, total=None):
        if not self.file:
            return
        # throttle to ~3 writes/s (menubar polls every 2s)
        if pct is not None and pct < 100 and time.time() - self._last < 0.3:
            return
        self._write(pct, done, total)

    def end(self):
        if not self.file:
            return
        try:
            self.file.unlink()
        except OSError:
            pass
        self.file = self.id = None

    def _write(self, pct=None, done=None, total=None):
        if not self.file:
            return
        self._last = time.time()
        rec = {
            "id": self.id, "type": self.type, "label": self.label,
            "pct": None if pct is None else max(0, min(100, round(pct))),
            "done": None if done is None else max(0, int(done)),
            "total": None if total is None else max(0, int(total)),
            "pid": os.getpid(),
            "startedAt": self.started,
            "updatedAt": int(self._last * 1000),
        }
        try:
            self.file.write_text(json.dumps(rec))
        except OSError:
            pass

    def __enter__(self):
        self.begin()
        return self

    def __exit__(self, *exc):
        self.end()
        return False
