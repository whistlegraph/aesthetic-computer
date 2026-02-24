#!/usr/bin/env python3
"""FedAC Kiosk piece server — serves bundled HTML on localhost:8080.
Supports systemd socket activation (LISTEN_FDS) for instant-ready socket."""
import http.server, socket, os, sys

PORT = 8080
PIECE_PATH = "/usr/local/share/kiosk/piece.html"

class Handler(http.server.BaseHTTPRequestHandler):
    def log_message(self, fmt, *args): pass
    def do_GET(self):
        try:
            with open(PIECE_PATH, "rb") as f:
                data = f.read()
            self.send_response(200)
            self.send_header("Content-Type", "text/html; charset=utf-8")
            self.send_header("Content-Length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)
        except Exception as e:
            self.send_response(500)
            self.end_headers()
            self.wfile.write(str(e).encode())

if __name__ == "__main__":
    # Check for systemd socket activation
    listen_fds = int(os.environ.get("LISTEN_FDS", 0))
    if listen_fds >= 1 and int(os.environ.get("LISTEN_PID", 0)) == os.getpid():
        # Use socket passed from systemd (fd 3 = SD_LISTEN_FDS_START)
        sock = socket.fromfd(3, socket.AF_INET, socket.SOCK_STREAM)
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    else:
        # Standalone mode
        import socketserver
        socketserver.TCPServer.allow_reuse_address = True
        sock = socket.socket()
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        sock.bind(("0.0.0.0", PORT))
        sock.listen(5)
    print(f"Piece server on http://0.0.0.0:{PORT}", flush=True)
    with http.server.HTTPServer.__new__(http.server.HTTPServer) as httpd:
        httpd.socket = sock
        httpd.server_address = sock.getsockname()
        httpd.RequestHandlerClass = Handler
        httpd.serve_forever()
