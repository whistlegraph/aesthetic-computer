"""Append-only painting tracks. Fresh/Done start a new track, never delete one."""
import datetime
import json
from pathlib import Path
import uuid


class PaintingTracks:
    def __init__(self, folder, legacy=()):
        self.folder = Path(folder) / "tracks"
        self.folder.mkdir(exist_ok=True)
        self.index_path = self.folder / "index.json"
        self.index = json.loads(self.index_path.read_text()) if self.index_path.exists() else {"version": 1, "paintings": []}
        if not self.index_path.exists():
            for event in legacy:
                self.record(event, legacy=True)

    @property
    def current(self):
        return self.index["paintings"][-1] if self.index["paintings"] else None

    def write_index(self):
        temporary = self.index_path.with_suffix(".tmp")
        temporary.write_text(json.dumps(self.index, indent=2) + "\n")
        temporary.replace(self.index_path)

    def append(self, painting, event):
        path = self.folder / painting["id"] / "events.jsonl"
        path.parent.mkdir(exist_ok=True)
        with path.open("a") as stream:
            stream.write(json.dumps({"painting": painting["id"], **event}, separators=(",", ":")) + "\n")

    def record(self, event, legacy=False):
        at = event.get("at") or datetime.datetime.now(datetime.timezone.utc).isoformat()
        action = event.get("action")
        if self.current is None or action in ("restart", "done"):
            if self.current:
                self.current["ended_at"] = at
                if action == "done":
                    self.current["publication"] = event.get("publication")
                self.append(self.current, {"kind": "end", "at": at, "reason": action})
            self.index["paintings"].append({"id": uuid.uuid4().hex, "started_at": at,
                "ended_at": None, "initial": event.get("accepted"), "final": event.get("accepted"), "paints": 0})
        painting = self.current
        self.append(painting, {"kind": "decision", "legacy": legacy, **event, "at": at})
        if event.get("accepted"):
            painting["final"] = event["accepted"]
        if action == "paint":
            painting["paints"] += 1
        self.write_index()

    def frame(self, generation, frame):
        if self.current:
            self.append(self.current, {"kind": "state", "at": datetime.datetime.now(datetime.timezone.utc).isoformat(),
                "generation": generation, "frame": frame})

    def list(self):
        return [{**painting, "current": painting is self.current} for painting in reversed(self.index["paintings"])]

    def read(self, identifier):
        painting = next((p for p in self.index["paintings"] if p["id"] == identifier), None)
        if not painting:
            raise ValueError("Unknown painting track")
        events = []
        for line in (self.folder / identifier / "events.jsonl").read_text().splitlines():
            try:
                events.append(json.loads(line))
            except json.JSONDecodeError:
                continue  # A process stopped during the last append.
        return {"version": 1, "painting": painting, "events": events}
