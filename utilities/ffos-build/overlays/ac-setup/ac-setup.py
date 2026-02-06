#!/usr/bin/env python3
"""
Aesthetic Computer Boot Setup TUI
A simple terminal UI for WiFi and piece configuration.
Runs on first boot or when called manually.
Features a purple/pink matrix rain animation.
"""

import curses
import subprocess
import os
import json
import time
import sys
import random
import threading

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

# Matrix characters (mix of katakana-like and symbols)
MATRIX_CHARS = "ｦｧｨｩｪｫｬｭｮｯｰｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜﾝ0123456789⬡◇◈♦✧∴∵"

# Purple/Pink color palette (256 color mode indices, darkest to brightest)
PURPLE_PINK_PALETTE = [
    53,   # Dark purple
    54,   # Purple
    55,   # Medium purple
    91,   # Magenta-purple
    127,  # Pink-purple
    128,  # Medium pink
    129,  # Pink
    135,  # Light pink
    164,  # Bright magenta
    170,  # Light magenta
    171,  # Very light pink
    177,  # Pale pink
    213,  # Bright pink
    219,  # Lightest pink
]

class MatrixRain:
    """Matrix rain effect with purple/pink colors."""
    
    def __init__(self, height, width):
        self.height = height
        self.width = width
        self.drops = []
        self.grid = [[None for _ in range(width)] for _ in range(height)]
        self._init_drops()
    
    def _init_drops(self):
        """Initialize matrix rain drops."""
        num_drops = self.width // 2
        for _ in range(num_drops):
            self.drops.append(self._create_drop())
    
    def _create_drop(self, col=None):
        """Create a new drop."""
        return {
            'col': col if col is not None else random.randint(0, self.width - 1),
            'row': random.uniform(-20, 0),
            'speed': random.uniform(0.3, 1.2),
            'char_idx': random.randint(0, len(MATRIX_CHARS) - 1),
            'brightness': random.randint(8, 13),  # Index into palette
            'length': random.randint(5, 15),
        }
    
    def update(self):
        """Update drop positions."""
        # Clear grid
        for y in range(self.height):
            for x in range(self.width):
                self.grid[y][x] = None
        
        for drop in self.drops:
            # Move drop down
            drop['row'] += drop['speed']
            
            # Change character occasionally
            if random.random() < 0.1:
                drop['char_idx'] = (drop['char_idx'] + 1) % len(MATRIX_CHARS)
            
            # Reset if off screen
            if drop['row'] - drop['length'] > self.height:
                col = drop['col']
                drop.update(self._create_drop(col))
                drop['row'] = random.uniform(-10, 0)
            
            # Draw drop trail
            head_row = int(drop['row'])
            for i in range(drop['length']):
                y = head_row - i
                if 0 <= y < self.height and 0 <= drop['col'] < self.width:
                    # Fade brightness along trail
                    brightness = max(0, drop['brightness'] - i)
                    char = MATRIX_CHARS[(drop['char_idx'] + i) % len(MATRIX_CHARS)]
                    self.grid[y][drop['col']] = (char, brightness)
    
    def resize(self, height, width):
        """Resize the matrix."""
        self.height = height
        self.width = width
        self.grid = [[None for _ in range(width)] for _ in range(height)]
        # Adjust number of drops
        target_drops = width // 2
        while len(self.drops) < target_drops:
            self.drops.append(self._create_drop())
        while len(self.drops) > target_drops:
            self.drops.pop()

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

def draw_box(win, y, x, h, w, title="", fill=True):
    """Draw a box with optional title and background fill."""
    # Fill background first
    if fill:
        for row in range(y + 1, y + h - 1):
            try:
                win.addstr(row, x + 1, " " * (w - 2))
            except:
                pass
    
    # Corners and edges
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
    """Draw centered text."""
    try:
        h, w = win.getmaxyx()
        x = max(0, (w - len(text)) // 2)
        win.addstr(y, x, text, attr)
    except curses.error:
        pass

def init_colors():
    """Initialize color pairs for matrix rain."""
    curses.start_color()
    curses.use_default_colors()
    
    # Create color pairs for the purple/pink palette
    for i, color in enumerate(PURPLE_PINK_PALETTE):
        try:
            curses.init_pair(i + 10, color, -1)  # Start at pair 10 to avoid conflicts
        except:
            pass
    
    # UI color pairs
    curses.init_pair(1, curses.COLOR_WHITE, curses.COLOR_BLACK)
    curses.init_pair(2, curses.COLOR_CYAN, curses.COLOR_BLACK)
    curses.init_pair(3, curses.COLOR_MAGENTA, curses.COLOR_BLACK)

def draw_matrix(win, matrix):
    """Draw the matrix rain background."""
    h, w = win.getmaxyx()
    
    for y in range(min(h - 1, matrix.height)):
        for x in range(min(w - 1, matrix.width)):
            cell = matrix.grid[y][x]
            if cell:
                char, brightness = cell
                color_pair = curses.color_pair(brightness + 10)
                try:
                    # Use ASCII fallback for half-width katakana
                    display_char = char if ord(char) < 128 else random.choice("0123456789ABCDEF@#$%&*")
                    win.addstr(y, x, display_char, color_pair)
                except curses.error:
                    pass

def menu_select(stdscr, title, items, selected=0, matrix=None):
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
    last_update = time.time()
    
    stdscr.nodelay(True)  # Non-blocking input for animation
    
    while True:
        # Update and draw matrix background
        if matrix:
            now = time.time()
            if now - last_update > 0.05:  # 20 FPS
                matrix.update()
                last_update = now
            stdscr.erase()
            draw_matrix(stdscr, matrix)
        else:
            stdscr.clear()
        
        # Header (with bright color)
        center_text(stdscr, 1, "⬡ AESTHETIC COMPUTER", curses.A_BOLD | curses.color_pair(3))
        
        # Draw menu box (solid background)
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
            
            try:
                if idx == selected:
                    stdscr.attron(curses.A_REVERSE)
                    stdscr.addstr(y, x, " " * (menu_w - 4))
                    stdscr.addstr(y, x + 1, item[:menu_w - 6])
                    stdscr.attroff(curses.A_REVERSE)
                else:
                    stdscr.addstr(y, x + 1, item[:menu_w - 6])
            except curses.error:
                pass
        
        # Scroll indicators
        try:
            if scroll_offset > 0:
                stdscr.addstr(menu_y + 1, menu_x + menu_w - 3, "↑")
            if scroll_offset + visible_items < len(items):
                stdscr.addstr(menu_y + menu_h - 2, menu_x + menu_w - 3, "↓")
        except curses.error:
            pass
        
        # Footer
        try:
            stdscr.addstr(h - 2, 2, "↑↓ Navigate  Enter Select  q Quit", curses.A_DIM)
        except curses.error:
            pass
        
        stdscr.refresh()
        
        key = stdscr.getch()
        if key == -1:  # No input, continue animation
            time.sleep(0.016)
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
            return selected
        elif key in (ord('q'), ord('Q'), 27):  # q or Escape
            stdscr.nodelay(False)
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
    """Get text input from user. Returns None if cancelled."""
    curses.curs_set(1)
    h, w = stdscr.getmaxyx()
    
    box_w = min(50, w - 4)
    box_h = 7
    box_y = (h - box_h) // 2
    box_x = (w - box_w) // 2
    
    text = ""
    last_update = time.time()
    stdscr.nodelay(True)
    
    while True:
        # Update and draw matrix background
        if matrix:
            now = time.time()
            if now - last_update > 0.05:
                matrix.update()
                last_update = now
            stdscr.erase()
            draw_matrix(stdscr, matrix)
        else:
            stdscr.clear()
        
        center_text(stdscr, 1, "⬡ AESTHETIC COMPUTER", curses.A_BOLD | curses.color_pair(3))
        
        draw_box(stdscr, box_y, box_x, box_h, box_w, title)
        try:
            stdscr.addstr(box_y + 2, box_x + 2, prompt)
        except curses.error:
            pass
        
        # Input field
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
            time.sleep(0.016)
            continue
        elif key in (curses.KEY_ENTER, 10, 13):
            curses.curs_set(0)
            stdscr.nodelay(False)
            return text
        elif key == 27:  # Escape
            curses.curs_set(0)
            stdscr.nodelay(False)
            return None
        elif key in (curses.KEY_BACKSPACE, 127, 8):
            text = text[:-1]
        elif key >= 32 and key < 127:
            if len(text) < input_w - 1:
                text += chr(key)

def show_message(stdscr, title, message, wait=True, matrix=None):
    """Show a message box."""
    curses.curs_set(0)
    h, w = stdscr.getmaxyx()
    
    box_w = min(50, w - 4)
    box_h = 6
    box_y = (h - box_h) // 2
    box_x = (w - box_w) // 2
    
    last_update = time.time()
    show_start = time.time()
    
    if not wait:
        stdscr.nodelay(True)
    
    while True:
        # Update and draw matrix background
        if matrix:
            now = time.time()
            if now - last_update > 0.05:
                matrix.update()
                last_update = now
            stdscr.erase()
            draw_matrix(stdscr, matrix)
        else:
            stdscr.clear()
        
        center_text(stdscr, 1, "⬡ AESTHETIC COMPUTER", curses.A_BOLD | curses.color_pair(3))
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
            try:
                stdscr.addstr(box_y + 2 + i, box_x + 2, line)
            except curses.error:
                pass
        
        if wait:
            try:
                stdscr.addstr(h - 2, 2, "Press any key to continue", curses.A_DIM)
            except curses.error:
                pass
        
        stdscr.refresh()
        
        if wait:
            key = stdscr.getch()
            if key != -1:
                return
            time.sleep(0.016)
        else:
            # Non-waiting: show for 1.5 seconds with animation
            if time.time() - show_start > 1.5:
                stdscr.nodelay(False)
                return
            time.sleep(0.016)

def wifi_setup(stdscr, matrix=None):
    """WiFi selection and connection."""
    show_message(stdscr, "WiFi Setup", "Scanning for networks...", wait=False, matrix=matrix)
    
    networks = get_wifi_networks()
    current = get_current_wifi()
    
    if not networks:
        show_message(stdscr, "WiFi Setup", "No WiFi networks found. Check adapter.", matrix=matrix)
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
    
    selected = menu_select(stdscr, "Select WiFi Network", items, matrix=matrix)
    
    if selected == -1 or selected >= len(networks):
        return current is not None  # Return True if already connected
    
    ssid = networks[selected]["ssid"]
    needs_password = bool(networks[selected]["security"])
    
    password = ""
    if needs_password:
        password = text_input(stdscr, "WiFi Password", f"Password for '{ssid}':", hidden=True, matrix=matrix)
        if password is None:
            return False
    
    show_message(stdscr, "Connecting", f"Connecting to {ssid}...", wait=False, matrix=matrix)
    success, msg = connect_wifi(ssid, password)
    
    if success:
        show_message(stdscr, "Success!", f"Connected to {ssid}", matrix=matrix)
        return True
    else:
        show_message(stdscr, "Failed", f"Could not connect: {msg[:40]}", matrix=matrix)
        return False

def piece_setup(stdscr, matrix=None):
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
    
    selected = menu_select(stdscr, "Select Default Piece", items, selected_idx, matrix=matrix)
    
    if selected == -1:
        return
    
    piece_code = PIECES[selected][0]
    config["piece"] = piece_code
    save_config(config)
    
    show_message(stdscr, "Saved", f"Default piece set to: {piece_code}", matrix=matrix)

def main_menu(stdscr, matrix=None):
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
        
        selected = menu_select(stdscr, "Setup Menu", items, matrix=matrix)
        
        if selected == 0:
            wifi_setup(stdscr, matrix)
        elif selected == 1:
            piece_setup(stdscr, matrix)
        elif selected == 3:
            # Start AC
            mark_setup_done()
            apply_config()
            show_message(stdscr, "Starting", "Launching Aesthetic Computer...", wait=False, matrix=matrix)
            return True
        elif selected == 5 or selected == -1:
            return False

def show_welcome_screen(stdscr, matrix):
    """Show animated welcome screen with matrix rain."""
    h, w = stdscr.getmaxyx()
    last_update = time.time()
    stdscr.nodelay(True)
    
    while True:
        # Update matrix
        now = time.time()
        if now - last_update > 0.05:
            matrix.update()
            last_update = now
        
        stdscr.erase()
        draw_matrix(stdscr, matrix)
        
        # Draw centered welcome text with glow effect
        title = "⬡ AESTHETIC COMPUTER"
        subtitle = "Boot Setup"
        prompt = "Press any key to begin..."
        
        # Draw title with bright color
        center_text(stdscr, h // 2 - 3, title, curses.A_BOLD | curses.color_pair(13))
        center_text(stdscr, h // 2 - 1, subtitle, curses.color_pair(11))
        center_text(stdscr, h // 2 + 2, prompt, curses.A_DIM | curses.color_pair(10))
        
        stdscr.refresh()
        
        key = stdscr.getch()
        if key != -1:
            stdscr.nodelay(False)
            return
        
        time.sleep(0.016)

def run_setup(stdscr):
    """Run the setup wizard."""
    # Initialize colors for matrix rain
    init_colors()
    
    # Get screen size and create matrix
    h, w = stdscr.getmaxyx()
    matrix = MatrixRain(h, w)
    
    # Show animated welcome screen
    show_welcome_screen(stdscr, matrix)
    
    return main_menu(stdscr, matrix)

def main():
    # Check if we should run setup
    force = "--force" in sys.argv or "-f" in sys.argv
    
    if is_setup_done() and not force:
        print("Setup already completed. Use --force to run again.")
        print("Starting kiosk...")
        apply_config()
        return
    
    # Set TERM to support 256 colors
    os.environ.setdefault('TERM', 'xterm-256color')
    
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
