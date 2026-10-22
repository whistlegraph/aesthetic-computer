#!/usr/bin/env python3
"""Read bounded local Menu Band evidence; never controls the app or audio."""
import json
from pathlib import Path


def report(home=None):
    home = Path.home() if home is None else Path(home)
    directory = home / "Library/Logs/MenuBand/Session"
    result = {"logs": [], "lastDiagnosticSession": None, "crashReports": []}
    for name in ("session-diagnostics.previous.jsonl", "session-diagnostics.jsonl"):
        path = directory / name
        if path.is_file():
            result["logs"].append({"path": str(path), "bytes": path.stat().st_size})
    marker = directory / "session-diagnostics-state.json"
    try:
        # A damaged file cannot turn this into an unbounded log reader.
        with marker.open("rb") as stream:
            state = json.loads(stream.read(1024))
        if (isinstance(state.get("clean"), bool)
                and isinstance(state.get("timestamp"), (int, float))):
            result["lastDiagnosticSession"] = {
                "clean": state["clean"], "startedAt": state["timestamp"]}
    except (OSError, ValueError, AttributeError):
        pass
    # macOS owns crash handling. Inspect only MenuBand report metadata, never
    # report contents (which can include paths, process arguments and stacks).
    reports = home / "Library/Logs/DiagnosticReports"
    try:
        for path in reports.iterdir():
            if path.name.startswith("MenuBand-") and path.suffix in (".ips", ".crash"):
                try:
                    result["crashReports"].append({"modifiedAt": path.stat().st_mtime,
                                                   "format": path.suffix[1:]})
                    result["crashReports"] = sorted(result["crashReports"],
                        key=lambda row: row["modifiedAt"], reverse=True)[:10]
                except OSError:
                    pass
    except OSError:
        pass
    return result


if __name__ == "__main__":
    print(json.dumps(report(), indent=2))
