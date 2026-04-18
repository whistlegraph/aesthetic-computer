#!/usr/bin/env python3
"""Menu bar + transparent fullscreen HUD for the slab daemon.

Polls lid/sleep/state every 2s and reflects status both in the menu bar
title and in a click-through full-screen overlay that hugs the top-right
corner of the screen (the rest of the overlay is fully transparent so the
desktop shows through).

Menu bar icon legend:
    ◦   idle — no Claude prompts or subagents in flight
    ●   N work items in flight, lid open
    ◉   ambient playing (lid closed + active work + sleep disabled)
"""
import os
import subprocess
from pathlib import Path

import rumps
from AppKit import (
    NSBackingStoreBuffered,
    NSColor,
    NSFont,
    NSScreen,
    NSTextField,
    NSWindow,
    NSWindowCollectionBehaviorCanJoinAllSpaces,
    NSWindowCollectionBehaviorStationary,
    NSWindowStyleMaskBorderless,
    NSStatusWindowLevel,
    NSTextAlignmentRight,
)
from Foundation import NSMakeRect

SLAB_HOME = Path(os.environ.get("SLAB_HOME", os.path.expanduser("~/.local/share/slab")))
SLAB_BIN = Path(os.environ.get("SLAB_BIN", os.path.expanduser("~/.local/bin")))

ACTIVE_DIR = SLAB_HOME / "state" / "active-prompts"
SUBAGENT_DIR = SLAB_HOME / "state" / "active-subagents"
AMBIENT_PID_FILE = Path("/tmp/lidambient.pid")
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
    if not AMBIENT_PID_FILE.exists():
        return False
    try:
        pid = int(AMBIENT_PID_FILE.read_text().strip())
        os.kill(pid, 0)
        return True
    except (ValueError, ProcessLookupError, PermissionError, OSError):
        return False


class Overlay:
    """Full-screen, chromeless, click-through HUD.

    Background is fully transparent — only the small status label in the
    top-right corner is drawn. Sits at status-window level so it stays
    above normal app windows but out of the way of full-screen apps.
    """
    def __init__(self):
        screen = NSScreen.mainScreen()
        frame = screen.frame()

        self.window = NSWindow.alloc().initWithContentRect_styleMask_backing_defer_(
            frame, NSWindowStyleMaskBorderless, NSBackingStoreBuffered, False
        )
        self.window.setBackgroundColor_(NSColor.clearColor())
        self.window.setOpaque_(False)
        self.window.setHasShadow_(False)
        self.window.setLevel_(NSStatusWindowLevel)
        self.window.setIgnoresMouseEvents_(True)
        self.window.setCollectionBehavior_(
            NSWindowCollectionBehaviorCanJoinAllSpaces
            | NSWindowCollectionBehaviorStationary
        )

        label = NSTextField.alloc().init()
        label.setBezeled_(False)
        label.setDrawsBackground_(False)
        label.setEditable_(False)
        label.setSelectable_(False)
        label.setFont_(NSFont.monospacedSystemFontOfSize_weight_(14, 0))
        label.setAlignment_(NSTextAlignmentRight)
        label.setTextColor_(NSColor.colorWithWhite_alpha_(1.0, 0.45))
        label.setStringValue_("slab")

        w, h = 320, 22
        margin = 18
        label.setFrame_(NSMakeRect(
            frame.size.width - w - margin,
            frame.size.height - h - margin - 24,  # extra margin for notch/menu bar
            w, h,
        ))
        self.window.contentView().addSubview_(label)
        self.label = label

        self.window.orderFrontRegardless()

    def update(self, text: str, accent: bool = False):
        self.label.setStringValue_(text)
        # Brighten the label when ambient is playing for gentle emphasis.
        alpha = 0.85 if accent else 0.45
        self.label.setTextColor_(NSColor.colorWithWhite_alpha_(1.0, alpha))


class SlabApp(rumps.App):
    def __init__(self):
        super().__init__("slab", title="◦", quit_button=None)
        self.status_item = rumps.MenuItem("Status: —")
        self.prompts_item = rumps.MenuItem("Prompts in flight: 0")
        self.subs_item = rumps.MenuItem("Subagents in flight: 0")
        self.awake_item = rumps.MenuItem(
            "Stay awake (lid closed)", callback=self.toggle_awake
        )
        self.hud_item = rumps.MenuItem("Show desktop HUD", callback=self.toggle_hud)
        self.hud_item.state = 1

        self.menu = [
            self.status_item,
            None,
            self.prompts_item,
            self.subs_item,
            None,
            self.awake_item,
            rumps.MenuItem("Sleep now", callback=self.sleep_now),
            None,
            self.hud_item,
            rumps.MenuItem("Open daemon log", callback=self.open_log),
            rumps.MenuItem("Open sounds folder", callback=self.open_sounds),
            None,
            rumps.MenuItem("Reload daemon", callback=self.reload_daemon),
            rumps.MenuItem("Quit menu bar", callback=self.quit_app),
        ]
        self.overlay = Overlay()
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

        hud_text = f"slab  {icon}  {status}"
        self.overlay.update(hud_text, accent=amb)

    def toggle_awake(self, sender):
        cmd = "auto" if sender.state else "awake"
        subprocess.run([str(SLAB_BIN / "claude-sleep"), cmd], check=False)

    def sleep_now(self, _):
        subprocess.run([str(SLAB_BIN / "claude-sleep"), "now"], check=False)

    def toggle_hud(self, sender):
        sender.state = 0 if sender.state else 1
        if sender.state:
            self.overlay.window.orderFrontRegardless()
        else:
            self.overlay.window.orderOut_(None)

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
    SlabApp().run()
