#!/usr/bin/env python3
"""Pin the installed app without replacing other Dock entries."""
import argparse
import datetime
from pathlib import Path
import plistlib
import subprocess

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("app", type=Path)
parser.add_argument("--refresh", action="store_true", help="Replace this app's stale Dock bookmark after an update")
args = parser.parse_args()
app = args.app.expanduser().resolve(strict=True)
info = plistlib.loads((app / "Contents/Info.plist").read_bytes())
identifier = info["CFBundleIdentifier"]
preferences = plistlib.loads(subprocess.check_output(["defaults", "export", "com.apple.dock", "-"]))
entries = preferences.get("persistent-apps", [])
uri = app.as_uri() + "/"
existing = next((index for index, item in enumerate(entries)
                 if item.get("tile-data", {}).get("file-data", {}).get("_CFURLString") == uri), None)
if existing is not None and not args.refresh:
    print("No Paint is already pinned in the Dock.")
else:
    backup = Path.home() / "Library/Application Support/No Paint/install-backups"
    backup.mkdir(parents=True, exist_ok=True)
    (backup / ("dock-" + datetime.datetime.now().strftime("%Y%m%d-%H%M%S-%f") + ".plist")).write_bytes(plistlib.dumps(preferences))
    tile = {"tile-data": {"file-data": {"_CFURLString": uri, "_CFURLStringType": 15},
                          "file-label": "No Paint", "bundle-identifier": identifier, "file-type": 41},
            "tile-type": "file-tile"}
    if existing is None:
        subprocess.run(["defaults", "write", "com.apple.dock", "persistent-apps", "-array-add",
                        plistlib.dumps(tile).decode()], check=True)
    else:
        entries[existing] = tile
        subprocess.run(["defaults", "write", "com.apple.dock", "persistent-apps", "-array",
                        *[plistlib.dumps(item).decode() for item in entries]], check=True)
    subprocess.run(["killall", "Dock"], check=False)
    print("Refreshed No Paint in the Dock." if existing is not None else "Added No Paint to the Dock.")
