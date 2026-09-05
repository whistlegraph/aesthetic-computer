#!/usr/bin/env bash
# deepen.sh — second-layer facts for a band member: OS lineage, sleep/wake,
# devices it has known, inventories, footprints. Prints JSON to stdout;
# redirect into members/<name>/deep.json. Local: `bin/deepen.sh`.
# Remote: `bin/deepen.sh blueberry` (lands in bash even though login shell
# is fish, same pattern as harvest.sh / profile.sh).
set -euo pipefail
if [ -n "${1:-}" ] && [ "$1" != "local" ]; then
  exec ssh "$1" 'bash -s' -- local < "$0"
fi

python3 <<'PY'
import json, subprocess, os, glob

def sh(cmd, timeout=180):
    try:
        r = subprocess.run(cmd, shell=True, capture_output=True, text=True, timeout=timeout)
        return r.stdout.strip()
    except Exception:
        return ""

def profiler(dt):
    raw = sh(f"system_profiler {dt} -json 2>/dev/null")
    try:
        return json.loads(raw).get(dt, [])
    except Exception:
        return []

out = {"hostname": sh("hostname -s"),
       "computer_name": sh("scutil --get ComputerName 2>/dev/null")}

# --- OS lineage: every install the machine has lived through -----------------
hist = profiler("SPInstallHistoryDataType")
out["installs_total"] = len(hist)
macos = [h for h in hist if "macOS" in (h.get("_name") or "")]
out["macos_updates"] = [{"name": h.get("_name"), "version": h.get("install_version"),
                         "date": h.get("install_date")} for h in macos]
by_src = {}
for h in hist:
    s = h.get("package_source", "?")
    by_src[s] = by_src.get(s, 0) + 1
out["installs_by_source"] = by_src

# --- sleep / wake: how many naps (current power-log window, not lifetime) ---
log = sh("pmset -g log", timeout=300)
out["sleeps"] = sum(1 for l in log.splitlines() if "Entering Sleep state" in l)
out["wakes"] = sum(1 for l in log.splitlines() if "Wake from" in l)
out["menuband_wake_assertions"] = sum(
    1 for l in log.splitlines()
    if "(MenuBand)" in l and "PreventUserIdleSystemSleep" in l)

# --- devices it has known ---------------------------------------------------
audio = []
for card in profiler("SPAudioDataType"):
    for d in card.get("_items", []):
        name = d.get("_name")
        if name:
            audio.append({"name": name,
                          "input": d.get("coreaudio_device_input"),
                          "output": d.get("coreaudio_device_output"),
                          "transport": d.get("coreaudio_device_transport")})
out["audio_devices"] = audio

bt = profiler("SPBluetoothDataType")
paired = []
for node in bt:
    for key in ("device_connected", "device_not_connected"):
        for entry in node.get(key, []):
            for name, info in entry.items():
                paired.append({"name": name,
                               "type": (info or {}).get("device_minorType"),
                               "connected": key == "device_connected"})
out["bluetooth_paired"] = paired

disp = []
for gpu in profiler("SPDisplaysDataType"):
    for d in gpu.get("spdisplays_ndrvs", []):
        disp.append({"name": d.get("_name"),
                     "resolution": d.get("_spdisplays_pixels"),
                     "main": d.get("spdisplays_main")})
out["displays"] = disp

usb = []
def walk_usb(items):
    for i in items:
        n = i.get("_name")
        if n and "Hub" not in n and "hub" not in n:
            usb.append(n)
        walk_usb(i.get("_items", []))
walk_usb(profiler("SPUSBDataType"))
out["usb_now"] = usb

# --- inventories ------------------------------------------------------------
home = os.path.expanduser("~")
out["apps_system"] = len(glob.glob("/Applications/*.app"))
out["apps_user"] = len(glob.glob(f"{home}/Applications/*.app"))
house = ["MenuBand", "TrackDrum", "BlueberryWallpaper", "PhysMidi", "GlyphWizard",
         "Fluoddity", "Menuband", "Aesthetic Computer", "Slab"]
present = []
for base in ("/Applications", f"{home}/Applications"):
    for a in glob.glob(f"{base}/*.app"):
        stem = os.path.basename(a)[:-4]
        if any(hn.lower() in stem.lower() for hn in house):
            present.append(stem)
out["house_apps"] = sorted(set(present))
out["brew_formulae"] = int(sh("brew list --formula 2>/dev/null | wc -l") or 0)
out["fonts_user"] = len(glob.glob(f"{home}/Library/Fonts/*"))
out["fonts_local"] = len(glob.glob("/Library/Fonts/*"))
out["say_voices"] = int(sh("say -v '?' 2>/dev/null | wc -l") or 0)
svc = sh("launchctl list 2>/dev/null | tail -n +2 | awk '{print $3}'")
labels = [l for l in svc.splitlines() if l]
out["launchd_services"] = len(labels)
out["launchd_non_apple"] = len([l for l in labels if not l.startswith("com.apple.")])

# --- acquaintances ----------------------------------------------------------
kh = f"{home}/.ssh/known_hosts"
out["ssh_known_hosts"] = sum(1 for _ in open(kh)) if os.path.exists(kh) else 0
wifi = sh("networksetup -listpreferredwirelessnetworks en0 2>/dev/null | tail -n +2 | wc -l")
out["wifi_known_networks"] = int(wifi or 0)  # count only; SSIDs stay private

# --- footprints -------------------------------------------------------------
def du_gb(path):
    v = sh(f"du -sk '{path}' 2>/dev/null | cut -f1")
    return round(int(v) / 1048576, 1) if v.isdigit() else None
out["repo_gb"] = du_gb(f"{home}/aesthetic-computer")
rc = sh(f"git -C '{home}/aesthetic-computer' rev-list --count HEAD 2>/dev/null")
out["repo_commits_local"] = int(rc) if rc.isdigit() else None
libs = glob.glob(f"{home}/Pictures/*.photoslibrary")
out["photos_library_gb"] = du_gb(libs[0]) if libs else None
out["timezone"] = os.path.basename(os.readlink("/etc/localtime")) if os.path.islink("/etc/localtime") else sh("date +%Z")
out["locale"] = sh("defaults read -g AppleLocale 2>/dev/null")

print(json.dumps(out, indent=2))
PY
