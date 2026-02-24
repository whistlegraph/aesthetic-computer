#!/usr/bin/env python3
"""FedAC Kiosk WiFi picker — local HTTP server on port 8080.
Serves a single-page app that lets the user pick a WiFi network,
enter a password, connect, then redirects to kidlisp.com.
"""

import http.server
import json
import subprocess
import urllib.parse
import socketserver
import os
import signal
import sys

PORT = 8080
DEST = "https://kidlisp.com"

# Read PALS logo as base64 for embedding
PALS_B64 = ""
try:
    import base64
    logo = "/usr/share/plymouth/themes/spinner/watermark.png"
    if os.path.exists(logo):
        with open(logo, "rb") as f:
            PALS_B64 = base64.b64encode(f.read()).decode()
except Exception:
    pass

HTML = """<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>FedAC Kiosk</title>
<style>
* { margin:0; padding:0; box-sizing:border-box; }
body {
  background:#000; color:#fff; font-family:system-ui,-apple-system,sans-serif;
  min-height:100vh; display:flex; flex-direction:column;
  align-items:center; justify-content:center; padding:2rem;
}
.logo { margin-bottom:2rem; }
.logo img { height:80px; }
.card {
  background:#111; border-radius:16px; padding:2rem;
  width:100%%; max-width:420px; box-shadow:0 4px 30px rgba(200,80,150,.15);
}
h1 { font-size:1.3rem; font-weight:500; margin-bottom:1.5rem; text-align:center; color:#e8a; }
.status { text-align:center; padding:1rem; color:#aaa; font-size:.9rem; }
.status.ok { color:#8e8; }
.status.err { color:#e88; }
.net-list { list-style:none; max-height:300px; overflow-y:auto; margin-bottom:1rem; }
.net-list li {
  padding:.75rem 1rem; border-radius:8px; cursor:pointer;
  display:flex; justify-content:space-between; align-items:center;
  transition:background .15s;
}
.net-list li:hover { background:#222; }
.net-list li.selected { background:#2a1525; outline:2px solid #e8a; }
.net-list .ssid { font-weight:500; }
.net-list .meta { font-size:.8rem; color:#888; display:flex; gap:.5rem; }
.signal { display:inline-block; width:20px; text-align:center; }
.pass-row {
  display:none; margin-bottom:1rem;
}
.pass-row.show { display:block; }
.pass-row label { display:block; font-size:.85rem; color:#aaa; margin-bottom:.4rem; }
.pass-row input {
  width:100%%; padding:.6rem .8rem; border-radius:8px; border:1px solid #333;
  background:#1a1a1a; color:#fff; font-size:1rem; outline:none;
}
.pass-row input:focus { border-color:#e8a; }
button {
  width:100%%; padding:.75rem; border:none; border-radius:8px;
  background:#d07; color:#fff; font-size:1rem; font-weight:600;
  cursor:pointer; transition:background .15s;
}
button:hover { background:#e18; }
button:disabled { background:#333; color:#666; cursor:default; }
.spinner {
  display:inline-block; width:18px; height:18px;
  border:2px solid #666; border-top-color:#e8a; border-radius:50%%;
  animation:spin .6s linear infinite; vertical-align:middle; margin-right:.5rem;
}
@keyframes spin { to { transform:rotate(360deg); } }
.skip { display:block; text-align:center; margin-top:1rem; color:#666;
  font-size:.85rem; text-decoration:none; }
.skip:hover { color:#aaa; }
</style>
</head>
<body>
<div class="logo">
  LOGO_PLACEHOLDER
</div>
<div class="card">
  <h1>Connect to WiFi</h1>
  <div id="status" class="status">Scanning networks...</div>
  <ul id="networks" class="net-list"></ul>
  <div id="passRow" class="pass-row">
    <label for="password">Password</label>
    <input type="password" id="password" placeholder="Enter WiFi password" autocomplete="off">
  </div>
  <button id="connectBtn" disabled onclick="doConnect()">Connect</button>
  <a class="skip" href="DEST_PLACEHOLDER" id="skipLink">Skip — continue without WiFi</a>
</div>
<script>
const DEST = "DEST_PLACEHOLDER";
let chosen = null;
let networks = [];

async function scan() {
  const st = document.getElementById("status");
  st.className = "status";
  st.innerHTML = '<span class="spinner"></span> Scanning networks...';
  try {
    const r = await fetch("/api/networks");
    networks = await r.json();
    renderNetworks();
    if (networks.length === 0) {
      st.textContent = "No WiFi networks found.";
      st.className = "status err";
    } else {
      st.textContent = "Select a network:";
    }
  } catch(e) {
    st.textContent = "Scan failed.";
    st.className = "status err";
  }
}

function signalIcon(s) {
  if (s >= 70) return "\\u2588\\u2588\\u2588";
  if (s >= 40) return "\\u2588\\u2588\\u2581";
  return "\\u2588\\u2581\\u2581";
}

function renderNetworks() {
  const ul = document.getElementById("networks");
  ul.innerHTML = "";
  networks.forEach((n,i) => {
    const li = document.createElement("li");
    li.innerHTML = '<span class="ssid">' + esc(n.ssid) + '</span>'
      + '<span class="meta"><span class="signal">' + signalIcon(n.signal) + '</span>'
      + (n.security ? '\\uD83D\\uDD12' : '') + '</span>';
    li.onclick = () => selectNet(i);
    ul.appendChild(li);
  });
}

function esc(s) { const d=document.createElement("div"); d.textContent=s; return d.innerHTML; }

function selectNet(i) {
  chosen = networks[i];
  document.querySelectorAll(".net-list li").forEach((li,j) => {
    li.className = j===i ? "selected" : "";
  });
  const pr = document.getElementById("passRow");
  if (chosen.security) { pr.className = "pass-row show"; }
  else { pr.className = "pass-row"; }
  document.getElementById("connectBtn").disabled = false;
}

async function doConnect() {
  if (!chosen) return;
  const btn = document.getElementById("connectBtn");
  const st = document.getElementById("status");
  btn.disabled = true;
  btn.innerHTML = '<span class="spinner"></span> Connecting...';
  st.className = "status";
  st.textContent = "Connecting to " + chosen.ssid + "...";

  const body = { ssid: chosen.ssid };
  if (chosen.security) {
    body.password = document.getElementById("password").value;
  }
  try {
    const r = await fetch("/api/connect", {
      method: "POST",
      headers: {"Content-Type":"application/json"},
      body: JSON.stringify(body)
    });
    const res = await r.json();
    if (res.ok) {
      st.textContent = "Connected! Loading...";
      st.className = "status ok";
      setTimeout(() => { window.location.href = DEST; }, 1000);
    } else {
      st.textContent = res.error || "Connection failed.";
      st.className = "status err";
      btn.disabled = false;
      btn.textContent = "Connect";
    }
  } catch(e) {
    st.textContent = "Connection error.";
    st.className = "status err";
    btn.disabled = false;
    btn.textContent = "Connect";
  }
}

// Check if already online
async function checkOnline() {
  try {
    const r = await fetch("/api/status");
    const res = await r.json();
    if (res.connected) {
      window.location.href = DEST;
      return;
    }
  } catch(e) {}
  scan();
}
checkOnline();
</script>
</body>
</html>
""".replace("DEST_PLACEHOLDER", DEST)

# Inject logo if available
if PALS_B64:
    HTML = HTML.replace("LOGO_PLACEHOLDER",
        f'<img src="data:image/png;base64,{PALS_B64}" alt="PALS">')
else:
    HTML = HTML.replace("LOGO_PLACEHOLDER", "")


def nmcli_scan():
    """Return list of WiFi networks."""
    try:
        out = subprocess.check_output(
            ["nmcli", "-t", "-f", "SSID,SIGNAL,SECURITY", "dev", "wifi", "list"],
            stderr=subprocess.DEVNULL, timeout=10
        ).decode("utf-8", errors="replace")
    except Exception:
        return []
    seen = set()
    nets = []
    for line in out.strip().split("\n"):
        parts = line.split(":")
        if len(parts) < 3:
            continue
        ssid = parts[0].strip()
        if not ssid or ssid in seen:
            continue
        seen.add(ssid)
        try:
            sig = int(parts[1])
        except ValueError:
            sig = 0
        sec = parts[2].strip()
        nets.append({"ssid": ssid, "signal": sig,
                      "security": sec if sec and sec != "--" else ""})
    nets.sort(key=lambda n: -n["signal"])
    return nets[:20]


def nmcli_connect(ssid, password=None):
    """Connect to a WiFi network. Returns (ok, error_msg)."""
    cmd = ["nmcli", "dev", "wifi", "connect", ssid]
    if password:
        cmd += ["password", password]
    try:
        subprocess.check_output(cmd, stderr=subprocess.STDOUT, timeout=30)
        return True, ""
    except subprocess.CalledProcessError as e:
        return False, e.output.decode("utf-8", errors="replace").strip()
    except Exception as e:
        return False, str(e)


def nmcli_status():
    """Check if connected to the internet."""
    try:
        out = subprocess.check_output(
            ["nmcli", "-t", "-f", "STATE", "general"],
            stderr=subprocess.DEVNULL, timeout=5
        ).decode().strip()
        return "connected" in out and "disconnected" not in out
    except Exception:
        return False


class Handler(http.server.BaseHTTPRequestHandler):
    def log_message(self, fmt, *args):
        pass  # Silence logs

    def do_GET(self):
        if self.path == "/api/networks":
            nets = nmcli_scan()
            self._json(nets)
        elif self.path == "/api/status":
            self._json({"connected": nmcli_status()})
        else:
            self.send_response(200)
            self.send_header("Content-Type", "text/html; charset=utf-8")
            self.end_headers()
            self.wfile.write(HTML.encode())

    def do_POST(self):
        if self.path == "/api/connect":
            length = int(self.headers.get("Content-Length", 0))
            body = json.loads(self.rfile.read(length)) if length else {}
            ssid = body.get("ssid", "")
            password = body.get("password")
            if not ssid:
                self._json({"ok": False, "error": "No network selected."})
                return
            ok, err = nmcli_connect(ssid, password)
            if ok:
                self._json({"ok": True})
            else:
                self._json({"ok": False, "error": err or "Connection failed."})
        else:
            self.send_response(404)
            self.end_headers()

    def _json(self, obj):
        data = json.dumps(obj).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)


def main():
    socketserver.TCPServer.allow_reuse_address = True
    with socketserver.TCPServer(("127.0.0.1", PORT), Handler) as httpd:
        print(f"WiFi picker running on http://127.0.0.1:{PORT}")
        httpd.serve_forever()


if __name__ == "__main__":
    main()
