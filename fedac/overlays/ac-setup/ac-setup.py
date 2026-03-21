#!/usr/bin/env python3
"""
FedAC Setup TUI
A terminal UI for WiFi configuration, piece selection, and kiosk management.
Adapted from the FFOS ac-setup.py for Fedora.
Features purple/pink matrix rain animation.

Run: ac-setup [--force]
Access: Ctrl+Alt+F2 from kiosk mode
"""

import curses
import subprocess
import os
import json
import time
import sys
import random

STATE_DIR = os.path.expanduser("~/.state")
CONFIG_FILE = os.path.join(STATE_DIR, "ac-config.json")
SETUP_DONE_FILE = os.path.join(STATE_DIR, "setup-done")
APPIMAGE_PATH = "/opt/ac/bin/aesthetic-computer.AppImage"
KIOSK_SERVICE = "ac-electron-kiosk"

# AC pieces (online)
PIECES = [
    ("prompt", "Prompt — conversational AI canvas"),
    ("notepat", "Notepat — musical notepad"),
    ("wand", "Wand — magical drawing tool"),
    ("painting", "Painting — digital canvas"),
    ("whistlegraph", "Whistlegraph — collaborative drawing"),
    ("metronome", "Metronome — tempo keeper"),
    ("starfield", "Starfield — hypnotic stars"),
    ("sage", "Sage — wisdom interface"),
    ("bleep", "Bleep — sound toy"),
    ("freaky-flowers", "Freaky Flowers — generative art"),
]

# Offline pieces (bundled)
OFFLINE_PIECES_DIR = "/opt/ac/offline-pieces"
OFFLINE_PIECES = [
    ("notepat", "Notepat — musical notepad"),
    ("roz", "$roz — KidLisp demo"),
    ("starfield", "Starfield — hypnotic stars"),
]

# Matrix characters
MATRIX_CHARS = "0123456789ABCDEFabcdef@#$%&*+=<>[]{}|~"
HAS_256_COLORS = False


class MatrixRain:
    """Matrix rain effect with purple/pink colors."""

    def __init__(self, height, width):
        self.height = height
        self.width = width
        self.drops = []
        self.grid = [[None for _ in range(width)] for _ in range(height)]
        self._init_drops()

    def _init_drops(self):
        num_drops = max(5, self.width // 4)
        for _ in range(num_drops):
            self.drops.append(self._create_drop())

    def _create_drop(self, col=None):
        char = random.choice(MATRIX_CHARS)
        return {
            "col": col
            if col is not None
            else random.randint(0, max(0, self.width - 1)),
            "row": random.uniform(-15, -1),
            "speed": random.uniform(0.2, 0.6),
            "char": char,
            "brightness": random.randint(0, 4),
            "length": random.randint(4, 12),
        }

    def update(self):
        for y in range(self.height):
            for x in range(self.width):
                self.grid[y][x] = None

        for drop in self.drops:
            drop["row"] += drop["speed"]
            if random.random() < 0.02:
                drop["char"] = random.choice(MATRIX_CHARS)
            if drop["row"] - drop["length"] > self.height:
                col = drop["col"]
                new_drop = self._create_drop(col)
                drop.update(new_drop)
            head_row = int(drop["row"])
            for i in range(drop["length"]):
                y = head_row - i
                if 0 <= y < self.height and 0 <= drop["col"] < self.width:
                    brightness = max(0, 4 - (i * 4 // drop["length"]))
                    self.grid[y][drop["col"]] = (drop["char"], brightness)

    def resize(self, height, width):
        self.height = height
        self.width = width
        self.grid = [[None for _ in range(width)] for _ in range(height)]
        target_drops = max(5, width // 4)
        while len(self.drops) < target_drops:
            self.drops.append(self._create_drop())
        while len(self.drops) > target_drops:
            self.drops.pop()


# ── WiFi (nmcli — same API on Fedora) ──


def get_wifi_networks():
    try:
        subprocess.run(["nmcli", "dev", "wifi", "rescan"], capture_output=True, timeout=5)
        time.sleep(2)
        result = subprocess.run(
            ["nmcli", "-t", "-f", "SSID,SIGNAL,SECURITY", "dev", "wifi", "list"],
            capture_output=True,
            text=True,
            timeout=10,
        )
        networks = []
        seen = set()
        for line in result.stdout.strip().split("\n"):
            if line:
                parts = line.split(":")
                ssid = parts[0] if parts else ""
                if ssid and ssid not in seen:
                    seen.add(ssid)
                    signal = parts[1] if len(parts) > 1 else "?"
                    security = parts[2] if len(parts) > 2 else ""
                    networks.append(
                        {"ssid": ssid, "signal": signal, "security": security}
                    )
        return sorted(networks, key=lambda x: int(x["signal"] or 0), reverse=True)
    except Exception:
        return []


def get_current_wifi():
    try:
        result = subprocess.run(
            ["nmcli", "-t", "-f", "ACTIVE,SSID", "dev", "wifi"],
            capture_output=True,
            text=True,
            timeout=5,
        )
        for line in result.stdout.strip().split("\n"):
            if line.startswith("yes:"):
                return line.split(":", 1)[1]
    except Exception:
        pass
    return None


def connect_wifi(ssid, pw):
    debug_lines = []
    try:
        debug_lines.append(f"SSID: {ssid}")
        con_name = f"ac-{ssid}"
        subprocess.run(
            ["nmcli", "con", "delete", ssid], capture_output=True, text=True, timeout=10
        )
        subprocess.run(
            ["nmcli", "con", "delete", con_name],
            capture_output=True,
            text=True,
            timeout=10,
        )
        add_cmd = [
            "nmcli",
            "con",
            "add",
            "type",
            "wifi",
            "con-name",
            con_name,
            "ssid",
            ssid,
            "wifi-sec.key-mgmt",
            "wpa-psk",
            "wifi-sec.psk",
            pw,
        ]
        add_result = subprocess.run(add_cmd, capture_output=True, text=True, timeout=15)
        debug_lines.append(f"Add rc={add_result.returncode}")

        if add_result.returncode != 0:
            cmd = ["nmcli", "dev", "wifi", "connect", ssid, "password", pw]
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=30)
            output = result.stderr or result.stdout or "No output"
            return result.returncode == 0, output.strip(), "\n".join(debug_lines)

        up_result = subprocess.run(
            ["nmcli", "con", "up", con_name],
            capture_output=True,
            text=True,
            timeout=30,
        )
        output = up_result.stderr or up_result.stdout or "No output"
        return up_result.returncode == 0, output.strip(), "\n".join(debug_lines)
    except subprocess.TimeoutExpired:
        return False, "Connection timed out", "\n".join(debug_lines)
    except Exception as e:
        return False, str(e), "\n".join(debug_lines)


# ── Config ──


def load_config():
    try:
        with open(CONFIG_FILE, "r") as f:
            return json.load(f)
    except Exception:
        return {"piece": "prompt"}


def save_config(config):
    os.makedirs(STATE_DIR, exist_ok=True)
    with open(CONFIG_FILE, "w") as f:
        json.dump(config, f)


def apply_config():
    """Apply config: restart Electron kiosk with the selected piece."""
    config = load_config()
    piece = config.get("piece", "prompt")
    # The Electron app reads AC_PIECE env or navigates via --piece flag
    override_dir = os.path.expanduser(
        f"~/.config/systemd/user/{KIOSK_SERVICE}.service.d"
    )
    os.makedirs(override_dir, exist_ok=True)
    with open(os.path.join(override_dir, "override.conf"), "w") as f:
        f.write(
            f"[Service]\n"
            f"Environment=AC_PIECE={piece}\n"
            f"ExecStart=\n"
            f"ExecStart={APPIMAGE_PATH} --no-sandbox --kiosk --piece={piece}\n"
        )
    subprocess.run(["systemctl", "--user", "daemon-reload"], capture_output=True)
    subprocess.run(
        ["systemctl", "--user", "restart", KIOSK_SERVICE], capture_output=True
    )


def mark_setup_done():
    os.makedirs(STATE_DIR, exist_ok=True)
    with open(SETUP_DONE_FILE, "w") as f:
        f.write("done")


def is_setup_done():
    return os.path.exists(SETUP_DONE_FILE)


# ── UI helpers ──

PURPLE_BG = 53


def init_colors():
    global HAS_256_COLORS
    curses.start_color()
    curses.use_default_colors()
    HAS_256_COLORS = curses.COLORS >= 256

    if HAS_256_COLORS:
        try:
            curses.init_pair(10, 53, PURPLE_BG)
            curses.init_pair(11, 91, PURPLE_BG)
            curses.init_pair(12, 129, PURPLE_BG)
            curses.init_pair(13, 177, PURPLE_BG)
            curses.init_pair(14, 219, PURPLE_BG)
            curses.init_pair(20, 255, PURPLE_BG)
        except Exception:
            HAS_256_COLORS = False

    if not HAS_256_COLORS:
        curses.init_pair(10, curses.COLOR_BLACK, curses.COLOR_MAGENTA)
        curses.init_pair(11, curses.COLOR_MAGENTA, curses.COLOR_MAGENTA)
        curses.init_pair(12, curses.COLOR_MAGENTA, curses.COLOR_MAGENTA)
        curses.init_pair(13, curses.COLOR_WHITE, curses.COLOR_MAGENTA)
        curses.init_pair(14, curses.COLOR_WHITE, curses.COLOR_MAGENTA)
        curses.init_pair(20, curses.COLOR_WHITE, curses.COLOR_MAGENTA)

    if HAS_256_COLORS:
        curses.init_pair(1, 255, PURPLE_BG)
        curses.init_pair(2, 87, PURPLE_BG)
        curses.init_pair(3, 201, PURPLE_BG)
    else:
        curses.init_pair(1, curses.COLOR_WHITE, curses.COLOR_MAGENTA)
        curses.init_pair(2, curses.COLOR_CYAN, curses.COLOR_MAGENTA)
        curses.init_pair(3, curses.COLOR_MAGENTA, curses.COLOR_MAGENTA)


def draw_matrix(win, matrix):
    h, w = win.getmaxyx()
    for y in range(min(h - 1, matrix.height)):
        for x in range(min(w - 1, matrix.width)):
            cell = matrix.grid[y][x]
            if cell:
                char, brightness = cell
                color_pair = curses.color_pair(10 + brightness)
                try:
                    win.addch(y, x, char, color_pair)
                except curses.error:
                    pass


def draw_box(win, y, x, h, w, title="", fill=True):
    if fill:
        for row in range(y + 1, y + h - 1):
            try:
                win.addstr(row, x + 1, " " * (w - 2))
            except Exception:
                pass
    try:
        win.addch(y, x, curses.ACS_ULCORNER)
        win.addch(y, x + w - 1, curses.ACS_URCORNER)
        win.addch(y + h - 1, x, curses.ACS_LLCORNER)
        win.addch(y + h - 1, x + w - 1, curses.ACS_LRCORNER)
        for i in range(1, w - 1):
            win.addch(y, x + i, curses.ACS_HLINE)
            win.addch(y + h - 1, x + i, curses.ACS_HLINE)
        for i in range(1, h - 1):
            win.addch(y + i, x, curses.ACS_VLINE)
            win.addch(y + i, x + w - 1, curses.ACS_VLINE)
        if title:
            win.addstr(y, x + 2, f" {title} ", curses.A_BOLD)
    except curses.error:
        pass


def center_text(win, y, text, attr=0):
    try:
        h, w = win.getmaxyx()
        x = max(0, (w - len(text)) // 2)
        win.addstr(y, x, text, attr)
    except curses.error:
        pass


def menu_select(stdscr, title, items, selected=0, matrix=None):
    curses.curs_set(0)
    h, w = stdscr.getmaxyx()
    menu_w = min(60, w - 4)
    menu_h = min(len(items) + 4, h - 4)
    menu_y = (h - menu_h) // 2
    menu_x = (w - menu_w) // 2
    visible_items = menu_h - 4
    scroll_offset = 0
    last_update = time.time()
    frame_time = 0.1

    stdscr.nodelay(True)
    stdscr.timeout(50)

    while True:
        now = time.time()
        if matrix and (now - last_update) >= frame_time:
            matrix.update()
            last_update = now

        stdscr.erase()
        if matrix:
            draw_matrix(stdscr, matrix)

        center_text(
            stdscr,
            1,
            "* AESTHETIC COMPUTER *",
            curses.A_BOLD | curses.color_pair(14),
        )
        draw_box(stdscr, menu_y, menu_x, menu_h, menu_w, title)

        if selected < scroll_offset:
            scroll_offset = selected
        elif selected >= scroll_offset + visible_items:
            scroll_offset = selected - visible_items + 1

        for i in range(min(visible_items, len(items))):
            idx = scroll_offset + i
            if idx >= len(items):
                break
            item = items[idx]
            y = menu_y + 2 + i
            x = menu_x + 2
            try:
                if idx == selected:
                    stdscr.attron(curses.A_REVERSE)
                    stdscr.addstr(y, x, " " * (menu_w - 4))
                    stdscr.addstr(y, x + 1, item[: menu_w - 6])
                    stdscr.attroff(curses.A_REVERSE)
                else:
                    stdscr.addstr(y, x + 1, item[: menu_w - 6])
            except curses.error:
                pass

        try:
            if scroll_offset > 0:
                stdscr.addstr(menu_y + 1, menu_x + menu_w - 3, "^")
            if scroll_offset + visible_items < len(items):
                stdscr.addstr(menu_y + menu_h - 2, menu_x + menu_w - 3, "v")
        except curses.error:
            pass

        try:
            stdscr.addstr(
                h - 2, 2, "Up/Down Navigate  Enter Select  q Quit", curses.A_DIM
            )
        except curses.error:
            pass

        stdscr.refresh()

        key = stdscr.getch()
        if key == -1:
            continue
        elif key == curses.KEY_UP and selected > 0:
            selected -= 1
        elif key == curses.KEY_DOWN and selected < len(items) - 1:
            selected += 1
        elif key == curses.KEY_HOME:
            selected = 0
        elif key == curses.KEY_END:
            selected = len(items) - 1
        elif key in (curses.KEY_ENTER, 10, 13):
            stdscr.nodelay(False)
            stdscr.timeout(-1)
            return selected
        elif key in (ord("q"), ord("Q"), 27):
            stdscr.nodelay(False)
            stdscr.timeout(-1)
            return -1
        elif key == curses.KEY_RESIZE:
            h, w = stdscr.getmaxyx()
            if matrix:
                matrix.resize(h, w)
            menu_w = min(60, w - 4)
            menu_h = min(len(items) + 4, h - 4)
            menu_y = (h - menu_h) // 2
            menu_x = (w - menu_w) // 2


def text_input(stdscr, title, prompt, hidden=False, matrix=None):
    curses.curs_set(1)
    h, w = stdscr.getmaxyx()
    box_w = min(50, w - 4)
    box_h = 7
    box_y = (h - box_h) // 2
    box_x = (w - box_w) // 2
    text = ""
    last_update = time.time()
    frame_time = 0.1

    stdscr.nodelay(True)
    stdscr.timeout(50)

    while True:
        now = time.time()
        if matrix and (now - last_update) >= frame_time:
            matrix.update()
            last_update = now

        stdscr.erase()
        if matrix:
            draw_matrix(stdscr, matrix)

        center_text(
            stdscr,
            1,
            "* AESTHETIC COMPUTER *",
            curses.A_BOLD | curses.color_pair(14),
        )
        draw_box(stdscr, box_y, box_x, box_h, box_w, title)
        try:
            stdscr.addstr(box_y + 2, box_x + 2, prompt[: box_w - 4])
        except curses.error:
            pass

        input_y = box_y + 3
        input_x = box_x + 2
        input_w = box_w - 4
        display = "*" * len(text) if hidden else text
        try:
            stdscr.addstr(input_y, input_x, "_" * input_w, curses.A_DIM)
            stdscr.addstr(input_y, input_x, display[:input_w])
            stdscr.addstr(h - 2, 2, "Enter Confirm  Esc Cancel", curses.A_DIM)
            stdscr.move(input_y, input_x + min(len(text), input_w - 1))
        except curses.error:
            pass

        stdscr.refresh()

        key = stdscr.getch()
        if key == -1:
            continue
        elif key in (curses.KEY_ENTER, 10, 13):
            curses.curs_set(0)
            stdscr.nodelay(False)
            stdscr.timeout(-1)
            return text
        elif key == 27:
            curses.curs_set(0)
            stdscr.nodelay(False)
            stdscr.timeout(-1)
            return None
        elif key in (curses.KEY_BACKSPACE, 127, 8):
            text = text[:-1]
        elif 32 <= key < 127:
            if len(text) < input_w - 1:
                text += chr(key)


def show_message(stdscr, title, message, wait=True, matrix=None):
    curses.curs_set(0)
    h, w = stdscr.getmaxyx()
    box_w = min(50, w - 4)
    box_h = 6
    box_y = (h - box_h) // 2
    box_x = (w - box_w) // 2
    last_update = time.time()
    show_start = time.time()
    frame_time = 0.1

    stdscr.nodelay(True)
    stdscr.timeout(50)

    while True:
        now = time.time()
        if matrix and (now - last_update) >= frame_time:
            matrix.update()
            last_update = now

        stdscr.erase()
        if matrix:
            draw_matrix(stdscr, matrix)

        center_text(
            stdscr,
            1,
            "* AESTHETIC COMPUTER *",
            curses.A_BOLD | curses.color_pair(14),
        )
        draw_box(stdscr, box_y, box_x, box_h, box_w, title)

        words = message.split()
        lines = []
        line = ""
        for word in words:
            if len(line) + len(word) + 1 <= box_w - 4:
                line = line + " " + word if line else word
            else:
                lines.append(line)
                line = word
        if line:
            lines.append(line)

        for i, ln in enumerate(lines[: box_h - 4]):
            try:
                stdscr.addstr(box_y + 2 + i, box_x + 2, ln)
            except curses.error:
                pass

        if wait:
            try:
                stdscr.addstr(h - 2, 2, "Press any key to continue", curses.A_DIM)
            except curses.error:
                pass

        stdscr.refresh()

        key = stdscr.getch()
        if wait:
            if key != -1:
                stdscr.nodelay(False)
                stdscr.timeout(-1)
                return
        else:
            if now - show_start > 1.5:
                stdscr.nodelay(False)
                stdscr.timeout(-1)
                return


# ── Screens ──


def wifi_setup(stdscr, matrix=None):
    show_message(stdscr, "WiFi Setup", "Scanning for networks...", wait=False, matrix=matrix)
    networks = get_wifi_networks()
    current = get_current_wifi()

    if not networks:
        show_message(stdscr, "WiFi Setup", "No WiFi networks found.", matrix=matrix)
        return False

    items = []
    for net in networks:
        ssid = net["ssid"]
        signal = net["signal"]
        lock = "[*]" if net["security"] else "   "
        connected = " <" if ssid == current else ""
        items.append(f"{ssid} {lock} ({signal}%){connected}")
    items.append("-" * 30)
    items.append("Skip WiFi setup")

    selected = menu_select(stdscr, "Select WiFi Network", items, matrix=matrix)
    if selected == -1 or selected >= len(networks):
        return current is not None

    ssid = networks[selected]["ssid"]
    needs_password = bool(networks[selected]["security"])

    password = ""
    if needs_password:
        password = text_input(
            stdscr, "WiFi Password", f"Password for '{ssid}':", hidden=False, matrix=matrix
        )
        if password is None:
            return False

    show_message(stdscr, "Connecting", f"Connecting to {ssid}...", wait=False, matrix=matrix)
    success, msg, debug = connect_wifi(ssid, password)

    if success:
        show_message(stdscr, "Connected", f"Connected to {ssid}", matrix=matrix)
        return True
    else:
        show_message(stdscr, "Failed", f"Could not connect: {msg}", matrix=matrix)
        return False


def piece_setup(stdscr, matrix=None):
    config = load_config()
    current_piece = config.get("piece", "prompt")

    items = []
    selected_idx = 0
    for i, (code, desc) in enumerate(PIECES):
        marker = " <" if code == current_piece else ""
        items.append(f"{desc}{marker}")
        if code == current_piece:
            selected_idx = i

    selected = menu_select(
        stdscr, "Select Default Piece", items, selected_idx, matrix=matrix
    )
    if selected == -1:
        return

    piece_code = PIECES[selected][0]
    config["piece"] = piece_code
    save_config(config)
    show_message(stdscr, "Saved", f"Default piece: {piece_code}", matrix=matrix)


def system_info(stdscr, matrix=None):
    """Show system information."""
    info_lines = []
    # Hostname
    try:
        hostname = subprocess.run(
            ["hostname"], capture_output=True, text=True, timeout=3
        ).stdout.strip()
        info_lines.append(f"Hostname: {hostname}")
    except Exception:
        info_lines.append("Hostname: unknown")

    # IP address
    try:
        ip = subprocess.run(
            ["hostname", "-I"], capture_output=True, text=True, timeout=3
        ).stdout.strip().split()[0]
        info_lines.append(f"IP: {ip}")
    except Exception:
        info_lines.append("IP: not connected")

    # WiFi
    wifi = get_current_wifi()
    info_lines.append(f"WiFi: {wifi or 'not connected'}")

    # Electron
    if os.path.isfile(APPIMAGE_PATH):
        info_lines.append(f"Electron: installed")
    else:
        info_lines.append(f"Electron: NOT FOUND")

    # Kiosk service
    try:
        result = subprocess.run(
            ["systemctl", "--user", "is-active", KIOSK_SERVICE],
            capture_output=True,
            text=True,
            timeout=3,
        )
        status = result.stdout.strip()
        info_lines.append(f"Kiosk service: {status}")
    except Exception:
        info_lines.append("Kiosk service: unknown")

    # Config
    config = load_config()
    info_lines.append(f"Current piece: {config.get('piece', 'prompt')}")

    show_message(
        stdscr, "System Info", "\n".join(info_lines), matrix=matrix
    )


def main_menu(stdscr, matrix=None):
    while True:
        current_wifi = get_current_wifi()
        config = load_config()
        current_piece = config.get("piece", "prompt")

        items = [
            f"WiFi Setup        [{current_wifi or 'Not connected'}]",
            f"Select Piece      [{current_piece}]",
            "System Info",
            "-" * 40,
            "Start Aesthetic Computer",
            "-" * 40,
            "Exit to Shell",
        ]

        selected = menu_select(stdscr, "FedAC Setup", items, matrix=matrix)

        if selected == 0:
            wifi_setup(stdscr, matrix)
        elif selected == 1:
            piece_setup(stdscr, matrix)
        elif selected == 2:
            system_info(stdscr, matrix)
        elif selected == 4:
            mark_setup_done()
            apply_config()
            show_message(
                stdscr,
                "Starting",
                "Launching Aesthetic Computer...",
                wait=False,
                matrix=matrix,
            )
            return True
        elif selected == 6 or selected == -1:
            return False


def show_welcome_screen(stdscr, matrix):
    h, w = stdscr.getmaxyx()
    last_update = time.time()
    frame_time = 0.1

    stdscr.nodelay(True)
    stdscr.timeout(50)

    while True:
        now = time.time()
        if (now - last_update) >= frame_time:
            matrix.update()
            last_update = now

        stdscr.erase()
        draw_matrix(stdscr, matrix)

        center_text(
            stdscr,
            h // 2 - 3,
            "* AESTHETIC COMPUTER *",
            curses.A_BOLD | curses.color_pair(14),
        )
        center_text(stdscr, h // 2 - 1, "FedAC Setup", curses.color_pair(12))
        center_text(stdscr, h // 2 + 2, "Press any key to begin...", curses.A_DIM)

        stdscr.refresh()

        key = stdscr.getch()
        if key != -1:
            stdscr.nodelay(False)
            stdscr.timeout(-1)
            return


def run_setup(stdscr):
    init_colors()
    stdscr.bkgd(" ", curses.color_pair(20))
    h, w = stdscr.getmaxyx()
    matrix = MatrixRain(h, w)
    show_welcome_screen(stdscr, matrix)
    return main_menu(stdscr, matrix)


def main():
    force = "--force" in sys.argv or "-f" in sys.argv

    if is_setup_done() and not force:
        print("Setup already completed. Use --force to run again.")
        print("Starting kiosk...")
        apply_config()
        return

    os.environ.setdefault("TERM", "xterm-256color")

    try:
        result = curses.wrapper(run_setup)
        if result:
            print("\nSetup complete! Aesthetic Computer is starting...")
        else:
            print("\nSetup cancelled. Run 'ac-setup --force' to configure later.")
    except KeyboardInterrupt:
        print("\nSetup interrupted.")


if __name__ == "__main__":
    main()
