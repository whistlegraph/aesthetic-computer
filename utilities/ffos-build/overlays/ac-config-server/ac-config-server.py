#!/usr/bin/env python3
"""
Aesthetic Computer Device Config Server
A simple HTTP server for configuring WiFi and default piece.
Runs on port 8888.
"""

import http.server
import json
import os
import subprocess
import urllib.parse
import socket
import html

PORT = 8888
STATE_DIR = os.path.expanduser("~/.state")
CONFIG_FILE = os.path.join(STATE_DIR, "ac-config.json")

def get_ip():
    """Get the device's IP address."""
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        s.connect(("1.1.1.1", 80))
        ip = s.getsockname()[0]
        s.close()
        return ip
    except:
        return "unknown"

def get_hostname():
    return socket.gethostname()

def get_wifi_networks():
    """Scan for available WiFi networks."""
    try:
        result = subprocess.run(
            ["nmcli", "-t", "-f", "SSID,SIGNAL,SECURITY", "dev", "wifi", "list"],
            capture_output=True, text=True, timeout=10
        )
        networks = []
        seen = set()
        for line in result.stdout.strip().split("\n"):
            if line:
                parts = line.split(":")
                if len(parts) >= 2 and parts[0] and parts[0] not in seen:
                    seen.add(parts[0])
                    networks.append({
                        "ssid": parts[0],
                        "signal": parts[1] if len(parts) > 1 else "?",
                        "security": parts[2] if len(parts) > 2 else ""
                    })
        return sorted(networks, key=lambda x: int(x["signal"] or 0), reverse=True)
    except Exception as e:
        return []

def get_current_wifi():
    """Get currently connected WiFi SSID."""
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
    """Connect to a WiFi network."""
    try:
        # First try to connect using existing connection
        result = subprocess.run(
            ["nmcli", "con", "up", ssid],
            capture_output=True, text=True, timeout=30
        )
        if result.returncode == 0:
            return True, "Connected!"
        
        # Create new connection
        cmd = ["nmcli", "dev", "wifi", "connect", ssid]
        if password:
            cmd.extend(["password", password])
        
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=30)
        if result.returncode == 0:
            return True, "Connected!"
        return False, result.stderr or "Failed to connect"
    except Exception as e:
        return False, str(e)

def load_config():
    """Load saved config."""
    try:
        with open(CONFIG_FILE, "r") as f:
            return json.load(f)
    except:
        return {"piece": "prompt", "url": ""}

def save_config(config):
    """Save config to file."""
    os.makedirs(STATE_DIR, exist_ok=True)
    with open(CONFIG_FILE, "w") as f:
        json.dump(config, f)

def restart_kiosk():
    """Restart the kiosk service with new URL."""
    config = load_config()
    piece = config.get("piece", "prompt")
    custom_url = config.get("url", "")
    
    if custom_url:
        url = custom_url
    else:
        url = f"https://aesthetic.computer/{piece}?tv=true&nogap=true&nolabel=true"
    
    # Update environment override
    override_dir = os.path.expanduser("~/.config/systemd/user/aesthetic-kiosk.service.d")
    os.makedirs(override_dir, exist_ok=True)
    
    with open(os.path.join(override_dir, "override.conf"), "w") as f:
        f.write(f"[Service]\nEnvironment=AC_URL={url}\n")
    
    # Reload and restart
    subprocess.run(["systemctl", "--user", "daemon-reload"], capture_output=True)
    subprocess.run(["systemctl", "--user", "restart", "aesthetic-kiosk"], capture_output=True)

# Popular AC pieces for the dropdown
PIECES = [
    ("prompt", "Prompt (default)"),
    ("notepat", "Notepat"),
    ("wand", "Wand"),
    ("sprout", "Sprout"),
    ("painting", "Painting"),
    ("whistlegraph", "Whistlegraph"),
    ("metronome", "Metronome"),
    ("starfield", "Starfield"),
    ("ff", "FF (Feral File)"),
    ("sage", "Sage"),
    ("bleep", "Bleep"),
    ("freaky-flowers", "Freaky Flowers"),
    ("custom", "Custom URL..."),
]

HTML_TEMPLATE = """<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>AC Device Config</title>
  <style>
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
      background: #111; color: #eee;
      min-height: 100vh; padding: 1rem;
    }
    .container { max-width: 400px; margin: 0 auto; }
    h1 { font-size: 1.25rem; margin-bottom: 1.5rem; color: #888; text-align: center; }
    .section { background: #222; border-radius: 8px; padding: 1rem; margin-bottom: 1rem; }
    .section h2 { font-size: 0.9rem; color: #666; margin-bottom: 0.75rem; text-transform: uppercase; letter-spacing: 0.05em; }
    label { display: block; margin-bottom: 0.5rem; font-size: 0.9rem; color: #aaa; }
    input, select { 
      width: 100%; padding: 0.75rem; border: 1px solid #444; border-radius: 6px;
      background: #1a1a1a; color: #fff; font-size: 1rem; margin-bottom: 0.75rem;
    }
    input:focus, select:focus { outline: none; border-color: #666; }
    button {
      width: 100%; padding: 0.875rem; border: none; border-radius: 6px;
      background: #333; color: #fff; font-size: 1rem; cursor: pointer;
      transition: background 0.2s;
    }
    button:hover { background: #444; }
    button.primary { background: #2563eb; }
    button.primary:hover { background: #1d4ed8; }
    .status { padding: 0.75rem; border-radius: 6px; margin-bottom: 1rem; font-size: 0.9rem; }
    .status.success { background: #064e3b; color: #6ee7b7; }
    .status.error { background: #7f1d1d; color: #fca5a5; }
    .status.info { background: #1e3a5f; color: #93c5fd; }
    .wifi-list { max-height: 200px; overflow-y: auto; margin-bottom: 0.75rem; }
    .wifi-item {
      padding: 0.5rem 0.75rem; border-radius: 4px; cursor: pointer;
      display: flex; justify-content: space-between; align-items: center;
    }
    .wifi-item:hover { background: #333; }
    .wifi-item.connected { background: #064e3b; }
    .wifi-signal { color: #666; font-size: 0.8rem; }
    .custom-url { display: none; }
    .custom-url.show { display: block; }
    .footer { text-align: center; color: #444; font-size: 0.8rem; margin-top: 2rem; }
  </style>
</head>
<body>
  <div class="container">
    <h1>⬡ AESTHETIC COMPUTER</h1>
    
    {status}
    
    <div class="section">
      <h2>WiFi</h2>
      <div id="currentWifi"></div>
      <form method="POST" action="/wifi">
        <label>Network</label>
        <div class="wifi-list" id="wifiList">Scanning...</div>
        <input type="hidden" name="ssid" id="ssidInput">
        <label>Password</label>
        <input type="password" name="password" placeholder="Enter WiFi password">
        <button type="submit">Connect</button>
      </form>
    </div>
    
    <div class="section">
      <h2>Default Piece</h2>
      <form method="POST" action="/piece">
        <label>Select piece to display</label>
        <select name="piece" id="pieceSelect" onchange="toggleCustomUrl()">
          {piece_options}
        </select>
        <div class="custom-url" id="customUrlDiv">
          <label>Custom URL</label>
          <input type="text" name="url" placeholder="https://..." value="{custom_url}">
        </div>
        <button type="submit" class="primary">Apply & Restart Kiosk</button>
      </form>
    </div>
    
    <div class="footer">
      {hostname} · {ip}
    </div>
  </div>
  
  <script>
    const networks = {networks_json};
    const currentWifi = {current_wifi_json};
    const selectedPiece = "{selected_piece}";
    
    // Render WiFi list
    const wifiList = document.getElementById('wifiList');
    const ssidInput = document.getElementById('ssidInput');
    
    if (networks.length === 0) {
      wifiList.innerHTML = '<div style="color:#666;padding:0.5rem">No networks found. Pull down to scan.</div>';
    } else {
      wifiList.innerHTML = networks.map(n => `
        <div class="wifi-item ${{n.ssid === currentWifi ? 'connected' : ''}}" onclick="selectWifi('${{n.ssid.replace(/'/g, "\\\\'")}}')">
          <span>${{n.ssid}} ${{n.ssid === currentWifi ? '✓' : ''}}</span>
          <span class="wifi-signal">${{n.signal}}% ${{n.security ? '🔒' : ''}}</span>
        </div>
      `).join('');
    }
    
    function selectWifi(ssid) {
      ssidInput.value = ssid;
      document.querySelectorAll('.wifi-item').forEach(el => el.style.background = '');
      event.currentTarget.style.background = '#333';
    }
    
    // Custom URL toggle
    function toggleCustomUrl() {
      const select = document.getElementById('pieceSelect');
      const customDiv = document.getElementById('customUrlDiv');
      customDiv.classList.toggle('show', select.value === 'custom');
    }
    
    // Set initial piece selection
    document.getElementById('pieceSelect').value = selectedPiece;
    toggleCustomUrl();
    
    // Auto-select first network if none selected
    if (networks.length > 0 && !ssidInput.value) {
      selectWifi(networks[0].ssid);
    }
  </script>
</body>
</html>
"""

class ConfigHandler(http.server.BaseHTTPRequestHandler):
    def log_message(self, format, *args):
        pass  # Suppress logging
    
    def do_GET(self):
        if self.path == "/" or self.path.startswith("/?"):
            self.send_response(200)
            self.send_header("Content-Type", "text/html")
            self.end_headers()
            
            # Parse status from query string
            query = urllib.parse.urlparse(self.path).query
            params = urllib.parse.parse_qs(query)
            status_html = ""
            if "success" in params:
                status_html = f'<div class="status success">{html.escape(params["success"][0])}</div>'
            elif "error" in params:
                status_html = f'<div class="status error">{html.escape(params["error"][0])}</div>'
            
            config = load_config()
            networks = get_wifi_networks()
            current_wifi = get_current_wifi()
            
            piece_options = "\n".join(
                f'<option value="{p[0]}">{p[1]}</option>' for p in PIECES
            )
            
            page = HTML_TEMPLATE.format(
                status=status_html,
                piece_options=piece_options,
                custom_url=html.escape(config.get("url", "")),
                hostname=get_hostname(),
                ip=get_ip(),
                networks_json=json.dumps(networks),
                current_wifi_json=json.dumps(current_wifi),
                selected_piece=config.get("piece", "prompt")
            )
            self.wfile.write(page.encode())
        else:
            self.send_error(404)
    
    def do_POST(self):
        content_length = int(self.headers.get("Content-Length", 0))
        post_data = self.rfile.read(content_length).decode()
        params = urllib.parse.parse_qs(post_data)
        
        if self.path == "/wifi":
            ssid = params.get("ssid", [""])[0]
            password = params.get("password", [""])[0]
            
            if ssid:
                success, msg = connect_wifi(ssid, password)
                if success:
                    self.send_response(303)
                    self.send_header("Location", f"/?success=Connected to {ssid}")
                else:
                    self.send_response(303)
                    self.send_header("Location", f"/?error={msg}")
            else:
                self.send_response(303)
                self.send_header("Location", "/?error=Please select a network")
            self.end_headers()
        
        elif self.path == "/piece":
            piece = params.get("piece", ["prompt"])[0]
            url = params.get("url", [""])[0]
            
            config = load_config()
            config["piece"] = piece
            config["url"] = url if piece == "custom" else ""
            save_config(config)
            
            restart_kiosk()
            
            self.send_response(303)
            self.send_header("Location", "/?success=Kiosk restarting...")
            self.end_headers()
        
        else:
            self.send_error(404)

def main():
    os.makedirs(STATE_DIR, exist_ok=True)
    
    server = http.server.HTTPServer(("0.0.0.0", PORT), ConfigHandler)
    print(f"AC Config Server running on http://{get_ip()}:{PORT}")
    
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nShutting down...")
        server.shutdown()

if __name__ == "__main__":
    main()
