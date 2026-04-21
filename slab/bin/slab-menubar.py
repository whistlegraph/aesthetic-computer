#!/usr/bin/env python3
"""Menu bar status item for the slab daemon.

Polls lid/sleep/state every 2s and reflects status in the menu bar title.

Menu bar icon legend:
    ◦   idle — no Claude prompts or subagents in flight
    ●   N work items in flight, lid open
    ◉   ambient playing (lid closed + active work + sleep disabled)
"""
import json
import os
import shutil
import subprocess
from pathlib import Path

import rumps
from AppKit import NSApplication, NSApplicationActivationPolicyAccessory

TAILSCALE_BIN = shutil.which("tailscale") or "/opt/homebrew/bin/tailscale"

SLAB_HOME = Path(os.environ.get("SLAB_HOME", os.path.expanduser("~/.local/share/slab")))
SLAB_BIN = Path(os.environ.get("SLAB_BIN", os.path.expanduser("~/.local/bin")))

ACTIVE_DIR = SLAB_HOME / "state" / "active-prompts"
SUBAGENT_DIR = SLAB_HOME / "state" / "active-subagents"
AMBIENT_FLAG = Path("/tmp/slab-ambient-active")
LID_LOG = SLAB_HOME / "logs" / "lidalive.log"
DAEMON_PLIST = Path.home() / "Library/LaunchAgents/computer.slab.daemon.plist"
MENUBAR_PLIST = Path.home() / "Library/LaunchAgents/computer.slab.menubar.plist"


def count_files(path: Path) -> int:
    try:
        return sum(1 for _ in path.iterdir())
    except FileNotFoundError:
        return 0


def lid_closed() -> bool:
    try:
        out = subprocess.check_output(
            ["ioreg", "-r", "-k", "AppleClamshellState", "-d", "4"],
            text=True, stderr=subprocess.DEVNULL,
        )
    except subprocess.CalledProcessError:
        return False
    for line in out.splitlines():
        if "AppleClamshellState" in line:
            return line.strip().split()[-1] == "Yes"
    return False


def sleep_disabled() -> bool:
    try:
        out = subprocess.check_output(["pmset", "-g"], text=True, stderr=subprocess.DEVNULL)
    except subprocess.CalledProcessError:
        return False
    for line in out.splitlines():
        if "SleepDisabled" in line:
            parts = line.strip().split()
            if len(parts) >= 2:
                return parts[1] == "1"
    return False


def ambient_running() -> bool:
    return AMBIENT_FLAG.exists()


def tailscale_peers():
    """Return [{hostname, os, online, active}, ...] sorted online-first, or None if tailscale unavailable."""
    try:
        out = subprocess.check_output(
            [TAILSCALE_BIN, "status", "--json"],
            text=True, stderr=subprocess.DEVNULL, timeout=2,
        )
    except (subprocess.CalledProcessError, FileNotFoundError,
            subprocess.TimeoutExpired, OSError):
        return None
    try:
        data = json.loads(out)
    except json.JSONDecodeError:
        return None
    peers = []
    for peer in (data.get("Peer") or {}).values():
        peers.append({
            "hostname": peer.get("HostName", ""),
            "os": peer.get("OS", ""),
            "online": bool(peer.get("Online")),
            "active": bool(peer.get("Active")),
        })
    peers.sort(key=lambda p: (not p["online"], p["hostname"]))
    return peers


def open_ssh(host: str):
    """Open a Terminal.app window and run `ssh <host>`."""
    subprocess.run([
        "osascript",
        "-e", f'tell application "Terminal" to do script "ssh {host}"',
        "-e", 'tell application "Terminal" to activate',
    ], check=False)


class SlabApp(rumps.App):
    def __init__(self):
        super().__init__("slab", title="◦", quit_button=None)
        self.status_item = rumps.MenuItem("Status: —")
        self.prompts_item = rumps.MenuItem("Prompts in flight: 0")
        self.subs_item = rumps.MenuItem("Subagents in flight: 0")
        self.tailnet_item = rumps.MenuItem("Tailnet: —")
        # Seed a child so rumps creates the underlying NSMenu; refresh_tailnet() will replace it.
        self.tailnet_item.add(rumps.MenuItem("…"))
        self.awake_item = rumps.MenuItem(
            "Stay awake (lid closed)", callback=self.toggle_awake
        )

        self.menu = [
            self.status_item,
            None,
            self.prompts_item,
            self.subs_item,
            None,
            self.tailnet_item,
            None,
            self.awake_item,
            rumps.MenuItem("Sleep now", callback=self.sleep_now),
            None,
            rumps.MenuItem("Open daemon log", callback=self.open_log),
            rumps.MenuItem("Open sounds folder", callback=self.open_sounds),
            None,
            rumps.MenuItem("Reload daemon", callback=self.reload_daemon),
            rumps.MenuItem("Quit menu bar", callback=self.quit_app),
        ]
        self.refresh(None)

    @rumps.timer(2)
    def refresh(self, _):
        prompts = count_files(ACTIVE_DIR)
        subs = count_files(SUBAGENT_DIR)
        total = prompts + subs
        lid = lid_closed()
        sd = sleep_disabled()
        amb = ambient_running()

        if amb:
            icon = f"◉ {total}" if total else "◉"
            status = f"ambient — {total} active" if total else "ambient"
        elif total > 0:
            icon = f"● {total}"
            if lid and not sd:
                status = f"{total} active · sleep not disabled"
            elif lid:
                status = f"{total} active · lid closed"
            else:
                status = f"{total} active"
        else:
            icon = "◦"
            status = "idle"

        self.title = icon
        self.status_item.title = f"Status: {status}"
        self.prompts_item.title = f"Prompts in flight: {prompts}"
        self.subs_item.title = f"Subagents in flight: {subs}"
        self.awake_item.state = 1 if sd else 0
        self.refresh_tailnet()

    def refresh_tailnet(self):
        peers = tailscale_peers()
        self.tailnet_item.clear()
        if peers is None:
            self.tailnet_item.title = "Tailnet: unavailable"
            self.tailnet_item.add(rumps.MenuItem("(tailscale not responding)"))
            return
        online = sum(1 for p in peers if p["online"])
        self.tailnet_item.title = f"Tailnet: {online}/{len(peers)} online"
        if not peers:
            self.tailnet_item.add(rumps.MenuItem("(no peers)"))
            return
        for p in peers:
            if p["active"]:
                dot = "●"
            elif p["online"]:
                dot = "○"
            else:
                dot = "·"
            label = f"{dot}  {p['hostname']}  ({p['os']})"
            host = p["hostname"]
            self.tailnet_item.add(
                rumps.MenuItem(label, callback=lambda _, h=host: open_ssh(h))
            )

    def toggle_awake(self, sender):
        cmd = "auto" if sender.state else "awake"
        subprocess.run([str(SLAB_BIN / "claude-sleep"), cmd], check=False)

    def sleep_now(self, _):
        subprocess.run([str(SLAB_BIN / "claude-sleep"), "now"], check=False)

    def open_log(self, _):
        subprocess.run(["open", "-a", "Console", str(LID_LOG)], check=False)

    def open_sounds(self, _):
        subprocess.run(["open", str(SLAB_HOME / "sounds")], check=False)

    def reload_daemon(self, _):
        if DAEMON_PLIST.exists():
            subprocess.run(["launchctl", "unload", str(DAEMON_PLIST)], check=False)
            subprocess.run(["launchctl", "load", str(DAEMON_PLIST)], check=False)

    def quit_app(self, _):
        if MENUBAR_PLIST.exists():
            subprocess.run(["launchctl", "unload", str(MENUBAR_PLIST)], check=False)
        rumps.quit_application()


if __name__ == "__main__":
    app = SlabApp()
    # Hide from Dock and Cmd-Tab — menu bar item only.
    # sharedApplication() instantiates the singleton; NSApp wrapper is None until then.
    NSApplication.sharedApplication().setActivationPolicy_(
        NSApplicationActivationPolicyAccessory
    )
    app.run()
