#!/usr/bin/env python3
"""FedAC Kiosk piece server — serves kiosk HTML + system APIs on localhost:8080.

Serves any file from /usr/local/share/kiosk/ (piece.html, piece-app.html, etc.)
and provides system APIs for volume control and status.

Supports systemd socket activation (LISTEN_FDS) for instant-ready socket."""
import http.server, socketserver, socket, os, sys, json, subprocess, mimetypes

PORT = 8080
KIOSK_DIR = "/usr/local/share/kiosk"
USER = "liveuser"

MIME_MAP = {
    ".html": "text/html; charset=utf-8",
    ".js": "application/javascript",
    ".css": "text/css",
    ".json": "application/json",
    ".png": "image/png",
    ".svg": "image/svg+xml",
}

def get_runtime_dir():
    try:
        import pwd
        uid = pwd.getpwnam(USER).pw_uid
    except (KeyError, ImportError):
        uid = 1000
    return f"/run/user/{uid}"

RUNTIME_DIR = get_runtime_dir()

def run_wpctl(args, capture=False):
    """Run wpctl as liveuser with their PipeWire session."""
    env = os.environ.copy()
    env["XDG_RUNTIME_DIR"] = RUNTIME_DIR
    env["DBUS_SESSION_BUS_ADDRESS"] = f"unix:path={RUNTIME_DIR}/bus"
    try:
        result = subprocess.run(
            ["runuser", "-u", USER, "--", "wpctl"] + args,
            env=env, timeout=3, capture_output=True, text=True
        )
        if capture:
            return result.stdout.strip()
        return result.returncode == 0
    except Exception:
        return "" if capture else False

def get_volume():
    """Get current volume level (0.0-1.0+) and mute state."""
    output = run_wpctl(["get-volume", "@DEFAULT_AUDIO_SINK@"], capture=True)
    # Output looks like: "Volume: 0.50" or "Volume: 0.50 [MUTED]"
    if not output:
        return {"volume": 0.5, "muted": False, "error": "wpctl unavailable"}
    parts = output.split()
    vol = 0.5
    muted = "[MUTED]" in output
    for i, p in enumerate(parts):
        if p == "Volume:" and i + 1 < len(parts):
            try:
                vol = float(parts[i + 1])
            except ValueError:
                pass
    return {"volume": round(vol, 2), "muted": muted}

class Handler(http.server.BaseHTTPRequestHandler):
    def log_message(self, fmt, *args): pass

    def send_json(self, data, status=200):
        body = json.dumps(data).encode()
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Access-Control-Allow-Origin", "*")
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        path = self.path.split("?")[0]  # Strip query string

        # API: GET /api/volume
        if path == "/api/volume":
            self.send_json(get_volume())
            return

        # API: GET /api/status (system info)
        if path == "/api/status":
            self.send_json({"ok": True, "volume": get_volume()})
            return

        # Static file serving from KIOSK_DIR
        if path == "/" or path == "":
            path = "/piece.html"

        # Security: prevent directory traversal
        safe = os.path.normpath(path.lstrip("/"))
        if safe.startswith("..") or os.sep + ".." in safe:
            self.send_error(403)
            return

        filepath = os.path.join(KIOSK_DIR, safe)
        if not os.path.isfile(filepath):
            self.send_error(404)
            return

        try:
            with open(filepath, "rb") as f:
                data = f.read()
            ext = os.path.splitext(filepath)[1].lower()
            content_type = MIME_MAP.get(ext, "application/octet-stream")
            self.send_response(200)
            self.send_header("Content-Type", content_type)
            self.send_header("Content-Length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)
        except Exception as e:
            self.send_response(500)
            self.end_headers()
            self.wfile.write(str(e).encode())

    def do_POST(self):
        path = self.path.split("?")[0]

        # API: POST /api/volume  body: {"volume": 0.75} or {"delta": "+5%"} or {"mute": "toggle"}
        if path == "/api/volume":
            try:
                length = int(self.headers.get("Content-Length", 0))
                body = json.loads(self.rfile.read(length)) if length else {}
            except Exception:
                self.send_json({"error": "bad request"}, 400)
                return

            if "mute" in body:
                run_wpctl(["set-mute", "@DEFAULT_AUDIO_SINK@", "toggle"])
            elif "volume" in body:
                vol = str(body["volume"])
                run_wpctl(["set-volume", "-l", "1.5", "@DEFAULT_AUDIO_SINK@", vol])
            elif "delta" in body:
                run_wpctl(["set-volume", "-l", "1.5", "@DEFAULT_AUDIO_SINK@", str(body["delta"])])
            else:
                self.send_json({"error": "provide volume, delta, or mute"}, 400)
                return

            self.send_json(get_volume())
            return

        self.send_error(404)

    def do_OPTIONS(self):
        self.send_response(204)
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
        self.send_header("Access-Control-Allow-Headers", "Content-Type")
        self.end_headers()

class PreBoundHTTPServer(http.server.HTTPServer):
    """HTTPServer that accepts a pre-bound socket (standalone or systemd-activated)."""
    def __init__(self, sock, handler):
        # BaseServer.__init__ sets up __is_shut_down Event and other internals
        socketserver.BaseServer.__init__(self, sock.getsockname(), handler)
        self.socket = sock
    def server_bind(self): pass
    def server_activate(self): pass

if __name__ == "__main__":
    # Check for systemd socket activation
    listen_fds = int(os.environ.get("LISTEN_FDS", 0))
    if listen_fds >= 1 and int(os.environ.get("LISTEN_PID", 0)) == os.getpid():
        # Use socket passed from systemd (fd 3 = SD_LISTEN_FDS_START)
        sock = socket.fromfd(3, socket.AF_INET, socket.SOCK_STREAM)
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    else:
        # Standalone mode
        sock = socket.socket()
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        sock.bind(("0.0.0.0", PORT))
        sock.listen(5)
    print(f"Piece server on http://0.0.0.0:{PORT}", flush=True)
    with PreBoundHTTPServer(sock, Handler) as httpd:
        httpd.serve_forever()
