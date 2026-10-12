#!/usr/bin/env python3
"""Pin this native app, preserving Dock order and a restorable preferences copy."""
import datetime
import pathlib
import plistlib
import subprocess
import sys

app = pathlib.Path(sys.argv[1]).resolve()
with (app / "Contents/Info.plist").open("rb") as stream:
    bundle = plistlib.load(stream)["CFBundleIdentifier"]
raw = subprocess.check_output(["defaults", "export", "com.apple.dock", "-"])
settings = plistlib.loads(raw)
apps = settings.get("persistent-apps", [])
url = app.as_uri() + "/"
tile = {
    "tile-type": "file-tile",
    "tile-data": {
        "bundle-identifier": bundle,
        "file-label": "Aesthetic Computer",
        "file-type": 41,
        "file-data": {"_CFURLString": url, "_CFURLStringType": 15},
    },
}
matches = [i for i, entry in enumerate(apps)
           if entry.get("tile-data", {}).get("file-data", {}).get("_CFURLString", "").rstrip("/") == url.rstrip("/")
           or entry.get("tile-data", {}).get("bundle-identifier") == bundle]
if matches and apps[matches[0]].get("tile-data", {}).get("file-data") == tile["tile-data"]["file-data"] and apps[matches[0]].get("tile-data", {}).get("bundle-identifier") == bundle:
    print("Aesthetic Computer is already pinned.")
    sys.exit(0)
if matches:
    apps[matches[0]] = tile
    for index in reversed(matches[1:]):
        del apps[index]
else:
    apps.append(tile)
backup_dir = pathlib.Path.home() / "Library/Application Support/Aesthetic Computer/Previous Builds"
backup_dir.mkdir(parents=True, exist_ok=True)
backup = backup_dir / ("Dock-" + datetime.datetime.now().strftime("%Y%m%d-%H%M%S") + ".plist")
backup.write_bytes(raw)

def xml_value(value):
    return plistlib.dumps(value).decode().split('<plist version="1.0">', 1)[1].split("</plist>", 1)[0].strip()

subprocess.run(["defaults", "write", "com.apple.dock", "persistent-apps", "-array", *map(xml_value, apps)], check=True)
subprocess.run(["killall", "Dock"], check=False, stdout=subprocess.DEVNULL)
print("Pinned Aesthetic Computer. Dock backup:", backup)
