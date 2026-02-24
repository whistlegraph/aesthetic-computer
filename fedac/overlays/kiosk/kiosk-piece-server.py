#!/usr/bin/env python3
"""FedAC Kiosk piece server — serves bundled HTML on localhost:8080."""
import http.server, socketserver

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
    socketserver.TCPServer.allow_reuse_address = True
    with socketserver.TCPServer(("0.0.0.0", PORT), Handler) as httpd:
        print(f"Piece server on http://0.0.0.0:{PORT}", flush=True)
        httpd.serve_forever()
