#!/bin/sh
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
python3 - "$root" <<'PY'
import http.server, json, pathlib, subprocess, sys, tempfile, threading
requests = []
class Peer(http.server.BaseHTTPRequestHandler):
    def do_POST(self):
        requests.append((self.path, self.rfile.read(int(self.headers['Content-Length']))))
        data = b'{"ok":true,"factor":1,"engaged":false,"animating":false}'
        self.send_response(200)
        self.send_header('Content-Length',str(len(data)))
        self.end_headers()
        self.wfile.write(data)
    def log_message(self, *_): pass
server = http.server.ThreadingHTTPServer(('127.0.0.1',0), Peer)
threading.Thread(target=server.serve_forever,daemon=True).start()
try:
    with tempfile.TemporaryDirectory(prefix='slab-zoom-fleet-') as temp:
        root = pathlib.Path(temp)
        peers = root / 'peers'
        peers.mkdir()
        # Idle stale peers still need reset. Duplicate cached IPs get one POST.
        for name in ['idle','duplicate']:
            (peers / (name+'.json')).write_text(json.dumps({'ip':'127.0.0.1','updatedAt':0,'entries':[]}))
        (peers / 'broken.json').write_text('not json')
        source = (pathlib.Path(sys.argv[1])/'Sources/SlabMenubar/ZoomEscape.swift').read_text()
        source += '''
enum ZoomLens { static func zoomOut() {} }
enum LedgerStore {
    static let peersDir = %s
    static let port: UInt16 = %s
}
ZoomEscape.cancelFleet()
ZoomEscape.cancelFleet()
RunLoop.main.run(until: Date(timeIntervalSinceNow: 2))
''' % (json.dumps(str(peers)), server.server_address[1])
        script = root/'check.swift'
        script.write_text(source)
        subprocess.run(['swift',str(script)],check=True)
        assert requests == [('/zoom/reset', b'{}')], requests
        print('Fleet HTTP broadcast / idle peers / duplicate suppression checks passed')
finally:
    server.shutdown()
PY
