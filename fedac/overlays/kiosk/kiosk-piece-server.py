#!/usr/bin/env python3
"""FedAC Kiosk piece server — serves bundled HTML on localhost:8080."""

import http.server
import socketserver
import os

PORT = 8080
PIECE_PATH = "/usr/local/share/kiosk/piece.html"

class Handler(http.server.BaseHTTPRequestHandler):
    def log_message(self, fmt, *args):
        pass

    def do_GET(self):
        self.send_response(200)
        self.send_header("Content-Type", "text/html; charset=utf-8")
        self.end_headers()
        with open(PIECE_PATH, "rb") as f:
            self.wfile.write(f.read())

if __name__ == "__main__":
    socketserver.TCPServer.allow_reuse_address = True
    with socketserver.TCPServer(("127.0.0.1", PORT), Handler) as httpd:
        print(f"Piece server on http://127.0.0.1:{PORT}")
        httpd.serve_forever()
