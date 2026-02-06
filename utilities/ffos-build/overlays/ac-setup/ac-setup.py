#!/usr/bin/env python3
"""
Aesthetic Computer Boot Setup TUI
A simple terminal UI for WiFi and piece configuration.
Runs on first boot or when called manually.
"""

import curses
import subprocess
import os
import json
import time
import sys

STATE_DIR = os.path.expanduser("~/.state")
CONFIG_FILE = os.path.join(STATE_DIR, "ac-config.json")
SETUP_DONE_FILE = os.path.join(STATE_DIR, "setup-done")

# Popular AC pieces
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

def get_wifi_networks():
    """Scan for WiFi networks."""
    try:
        # Trigger a fresh scan
        subprocess.run(["nmcli", "dev", "wifi", "rescan"], 
                      capture_output=True, timeout=5)
        time.sleep(2)
        
        result = subprocess.run(
            ["nmcli", "-t", "-f", "SSID,SIGNAL,SECURITY", "dev", "wifi", "list"],
            capture_output=True, text=True, timeout=10
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
                    networks.append({
                        "ssid": ssid,
                        "signal": signal,
                        "security": security
                    })
        return sorted(networks, key=lambda x: int(x["signal"] or 0), reverse=True)
    except Exception as e:
        return []

def get_current_wifi():
    """Get currently connected WiFi."""
    try:
        result = subprocess.run(
            ["nmcli", "-t", "-f", "ACTIVE,SSID", "dev", "wifi"],
            capture_output=True, text=True, timeout=5
        )
        for line in result.stdout.strip().split("\n"):
            if line.startswith("yes:"):
                return line.split(":", 1)[1]
    except:
        pass
    return None

def connect_wifi(ssid, password):
    """Connect to WiFi network."""
    try:
        cmd = ["nmcli", "dev", "wifi", "connect", ssid]
        if password:
            cmd.extend(["password", password])
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=30)
        return result.returncode == 0, result.stderr or result.stdout
    except Exception as e:
        return False, str(e)

def load_config():
    """Load saved config."""
    try:
        with open(CONFIG_FILE, "r") as f:
            return json.load(f)
    except:
        return {"piece": "prompt"}

def save_config(config):
    """Save config."""
    os.makedirs(STATE_DIR, exist_ok=True)
    with open(CONFIG_FILE, "w") as f:
        json.dump(config, f)

def apply_config():
    """Apply config and restart kiosk."""
    config = load_config()
    piece = config.get("piece", "prompt")
    url = f"https://aesthetic.computer/{piece}?tv=true&nogap=true&nolabel=true"
    
    # Create systemd override
    override_dir = os.path.expanduser("~/.config/systemd/user/aesthetic-kiosk.service.d")
    os.makedirs(override_dir, exist_ok=True)
    with open(os.path.join(override_dir, "override.conf"), "w") as f:
        f.write(f"[Service]\nEnvironment=AC_URL={url}\n")
    
    # Reload and restart
    subprocess.run(["systemctl", "--user", "daemon-reload"], capture_output=True)
    subprocess.run(["systemctl", "--user", "restart", "aesthetic-kiosk"], capture_output=True)

def mark_setup_done():
    """Mark setup as complete."""
    os.makedirs(STATE_DIR, exist_ok=True)
    with open(SETUP_DONE_FILE, "w") as f:
        f.write("done")

def is_setup_done():
    """Check if setup was already completed."""
    return os.path.exists(SETUP_DONE_FILE)

def draw_box(win, y, x, h, w, title=""):
    """Draw a box with optional title."""
    # Corners and edges
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

def center_text(win, y, text, attr=0):
    """Draw centered text."""
    h, w = win.getmaxyx()
    x = max(0, (w - len(text)) // 2)
    win.addstr(y, x, text, attr)

def menu_select(stdscr, title, items, selected=0):
    """Show a menu and return selected index, or -1 if cancelled."""
    curses.curs_set(0)
    h, w = stdscr.getmaxyx()
    
    # Calculate menu dimensions
    menu_w = min(60, w - 4)
    menu_h = min(len(items) + 4, h - 4)
    menu_y = (h - menu_h) // 2
    menu_x = (w - menu_w) // 2
    
    visible_items = menu_h - 4
    scroll_offset = 0
    
    while True:
        stdscr.clear()
        
        # Header
        center_text(stdscr, 1, "⬡ AESTHETIC COMPUTER", curses.A_BOLD)
        
        # Draw menu box
        draw_box(stdscr, menu_y, menu_x, menu_h, menu_w, title)
        
        # Adjust scroll to keep selection visible
        if selected < scroll_offset:
            scroll_offset = selected
        elif selected >= scroll_offset + visible_items:
            scroll_offset = selected - visible_items + 1
        
        # Draw items
        for i in range(min(visible_items, len(items))):
            idx = scroll_offset + i
            if idx >= len(items):
                break
            
            item = items[idx]
            y = menu_y + 2 + i
            x = menu_x + 2
            
            if idx == selected:
                stdscr.attron(curses.A_REVERSE)
                stdscr.addstr(y, x, " " * (menu_w - 4))
                stdscr.addstr(y, x + 1, item[:menu_w - 6])
                stdscr.attroff(curses.A_REVERSE)
            else:
                stdscr.addstr(y, x + 1, item[:menu_w - 6])
        
        # Scroll indicators
        if scroll_offset > 0:
            stdscr.addstr(menu_y + 1, menu_x + menu_w - 3, "↑")
        if scroll_offset + visible_items < len(items):
            stdscr.addstr(menu_y + menu_h - 2, menu_x + menu_w - 3, "↓")
        
        # Footer
        stdscr.addstr(h - 2, 2, "↑↓ Navigate  Enter Select  q Quit", curses.A_DIM)
        
        stdscr.refresh()
        
        key = stdscr.getch()
        if key == curses.KEY_UP and selected > 0:
            selected -= 1
        elif key == curses.KEY_DOWN and selected < len(items) - 1:
            selected += 1
        elif key == curses.KEY_HOME:
            selected = 0
        elif key == curses.KEY_END:
            selected = len(items) - 1
        elif key in (curses.KEY_ENTER, 10, 13):
            return selected
        elif key in (ord('q'), ord('Q'), 27):  # q or Escape
            return -1

def text_input(stdscr, title, prompt, hidden=False):
    """Get text input from user. Returns None if cancelled."""
    curses.curs_set(1)
    h, w = stdscr.getmaxyx()
    
    box_w = min(50, w - 4)
    box_h = 7
    box_y = (h - box_h) // 2
    box_x = (w - box_w) // 2
    
    text = ""
    
    while True:
        stdscr.clear()
        center_text(stdscr, 1, "⬡ AESTHETIC COMPUTER", curses.A_BOLD)
        
        draw_box(stdscr, box_y, box_x, box_h, box_w, title)
        stdscr.addstr(box_y + 2, box_x + 2, prompt)
        
        # Input field
        input_y = box_y + 3
        input_x = box_x + 2
        input_w = box_w - 4
        
        display = "*" * len(text) if hidden else text
        stdscr.addstr(input_y, input_x, "_" * input_w, curses.A_DIM)
        stdscr.addstr(input_y, input_x, display[:input_w])
        
        stdscr.addstr(h - 2, 2, "Enter Confirm  Esc Cancel", curses.A_DIM)
        
        stdscr.move(input_y, input_x + min(len(text), input_w - 1))
        stdscr.refresh()
        
        key = stdscr.getch()
        if key in (curses.KEY_ENTER, 10, 13):
            curses.curs_set(0)
            return text
        elif key == 27:  # Escape
            curses.curs_set(0)
            return None
        elif key in (curses.KEY_BACKSPACE, 127, 8):
            text = text[:-1]
        elif key >= 32 and key < 127:
            if len(text) < input_w - 1:
                text += chr(key)

def show_message(stdscr, title, message, wait=True):
    """Show a message box."""
    curses.curs_set(0)
    h, w = stdscr.getmaxyx()
    
    box_w = min(50, w - 4)
    box_h = 6
    box_y = (h - box_h) // 2
    box_x = (w - box_w) // 2
    
    stdscr.clear()
    center_text(stdscr, 1, "⬡ AESTHETIC COMPUTER", curses.A_BOLD)
    draw_box(stdscr, box_y, box_x, box_h, box_w, title)
    
    # Word wrap message
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
    
    for i, line in enumerate(lines[:box_h - 4]):
        stdscr.addstr(box_y + 2 + i, box_x + 2, line)
    
    if wait:
        stdscr.addstr(h - 2, 2, "Press any key to continue", curses.A_DIM)
        stdscr.refresh()
        stdscr.getch()
    else:
        stdscr.refresh()
        time.sleep(1.5)

def wifi_setup(stdscr):
    """WiFi selection and connection."""
    show_message(stdscr, "WiFi Setup", "Scanning for networks...", wait=False)
    
    networks = get_wifi_networks()
    current = get_current_wifi()
    
    if not networks:
        show_message(stdscr, "WiFi Setup", "No WiFi networks found. Check adapter.")
        return False
    
    # Build menu items
    items = []
    for net in networks:
        ssid = net["ssid"]
        signal = net["signal"]
        lock = "🔒" if net["security"] else "  "
        connected = " ✓" if ssid == current else ""
        items.append(f"{ssid} {lock} ({signal}%){connected}")
    
    items.append("─" * 30)
    items.append("Skip WiFi setup")
    
    selected = menu_select(stdscr, "Select WiFi Network", items)
    
    if selected == -1 or selected >= len(networks):
        return current is not None  # Return True if already connected
    
    ssid = networks[selected]["ssid"]
    needs_password = bool(networks[selected]["security"])
    
    password = ""
    if needs_password:
        password = text_input(stdscr, "WiFi Password", f"Password for '{ssid}':", hidden=True)
        if password is None:
            return False
    
    show_message(stdscr, "Connecting", f"Connecting to {ssid}...", wait=False)
    success, msg = connect_wifi(ssid, password)
    
    if success:
        show_message(stdscr, "Success!", f"Connected to {ssid}")
        return True
    else:
        show_message(stdscr, "Failed", f"Could not connect: {msg[:40]}")
        return False

def piece_setup(stdscr):
    """Piece selection."""
    config = load_config()
    current_piece = config.get("piece", "prompt")
    
    items = []
    selected_idx = 0
    for i, (code, desc) in enumerate(PIECES):
        marker = " ✓" if code == current_piece else ""
        items.append(f"{desc}{marker}")
        if code == current_piece:
            selected_idx = i
    
    selected = menu_select(stdscr, "Select Default Piece", items, selected_idx)
    
    if selected == -1:
        return
    
    piece_code = PIECES[selected][0]
    config["piece"] = piece_code
    save_config(config)
    
    show_message(stdscr, "Saved", f"Default piece set to: {piece_code}")

def main_menu(stdscr):
    """Main setup menu."""
    while True:
        current_wifi = get_current_wifi()
        config = load_config()
        current_piece = config.get("piece", "prompt")
        
        items = [
            f"WiFi Setup        [{current_wifi or 'Not connected'}]",
            f"Select Piece      [{current_piece}]",
            "─" * 40,
            "Start Aesthetic Computer",
            "─" * 40,
            "Exit to Shell",
        ]
        
        selected = menu_select(stdscr, "Setup Menu", items)
        
        if selected == 0:
            wifi_setup(stdscr)
        elif selected == 1:
            piece_setup(stdscr)
        elif selected == 3:
            # Start AC
            mark_setup_done()
            apply_config()
            show_message(stdscr, "Starting", "Launching Aesthetic Computer...", wait=False)
            return True
        elif selected == 5 or selected == -1:
            return False

def run_setup(stdscr):
    """Run the setup wizard."""
    # Setup curses
    curses.start_color()
    curses.use_default_colors()
    curses.init_pair(1, curses.COLOR_WHITE, curses.COLOR_BLUE)
    curses.init_pair(2, curses.COLOR_BLACK, curses.COLOR_CYAN)
    
    stdscr.clear()
    
    # Welcome screen
    h, w = stdscr.getmaxyx()
    center_text(stdscr, h // 2 - 3, "⬡ AESTHETIC COMPUTER", curses.A_BOLD)
    center_text(stdscr, h // 2 - 1, "Boot Setup", curses.A_DIM)
    center_text(stdscr, h // 2 + 1, "Press any key to begin...", curses.A_DIM)
    stdscr.refresh()
    stdscr.getch()
    
    return main_menu(stdscr)

def main():
    # Check if we should run setup
    force = "--force" in sys.argv or "-f" in sys.argv
    
    if is_setup_done() and not force:
        print("Setup already completed. Use --force to run again.")
        print("Starting kiosk...")
        apply_config()
        return
    
    # Run the TUI
    try:
        result = curses.wrapper(run_setup)
        if result:
            print("\n✓ Setup complete! Aesthetic Computer is starting...")
        else:
            print("\nSetup cancelled. Run 'ac-setup' to configure later.")
    except KeyboardInterrupt:
        print("\nSetup interrupted.")

if __name__ == "__main__":
    main()
