# Serves nopaint/construct/ zipped as a fresh No Paint.c3p on every request.
# `curl 127.0.0.1:8747/x -o "No Paint (work).c3p"`, then drop that file on the
# Construct editor. (editor.construct.net cannot fetch localhost itself.)
import http.server, io, os, zipfile
ROOT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "construct")
def pack():
    buf = io.BytesIO()
    with zipfile.ZipFile(buf, "w", zipfile.ZIP_DEFLATED) as z:
        for d, dirs, files in os.walk(ROOT):
            dirs[:] = [x for x in dirs if x != ".git"]
            for f in files:
                p = os.path.join(d, f)
                z.write(p, os.path.relpath(p, ROOT))
    return buf.getvalue()
class H(http.server.BaseHTTPRequestHandler):
    def cors(self):
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Private-Network", "true")
        self.send_header("Access-Control-Allow-Headers", "*")
    def do_OPTIONS(self):
        self.send_response(204); self.cors(); self.end_headers()
    def do_GET(self):
        body = pack()
        self.send_response(200); self.cors()
        self.send_header("Content-Type", "application/zip")
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Cache-Control", "no-store")
        self.end_headers(); self.wfile.write(body)
http.server.ThreadingHTTPServer(("127.0.0.1", 8747), H).serve_forever()
