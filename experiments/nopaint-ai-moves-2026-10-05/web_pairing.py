"""Explicit, origin-bound access from nopaint.art to the running desktop game."""
import secrets

ORIGINS = frozenset(("https://nopaint.art", "https://www.nopaint.art"))


def allowed(headers, port, token):
    if headers.get("Host") != f"127.0.0.1:{port}":
        return False
    origin = headers.get("Origin")
    if origin in (None, f"http://127.0.0.1:{port}"):
        return True
    return origin in ORIGINS and secrets.compare_digest(headers.get("X-NoPaint-Pairing", ""), token)


def cors(headers):
    origin = headers.get("Origin")
    if origin not in ORIGINS:
        return {}
    return {"Access-Control-Allow-Origin": origin, "Vary": "Origin",
            "Access-Control-Allow-Methods": "GET, POST, OPTIONS",
            "Access-Control-Allow-Headers": "Content-Type, X-NoPaint-Pairing",
            "Access-Control-Allow-Private-Network": "true"}


def connect_page(token):
    # Tokens only travel in a fragment; never in HTTP paths, logs or referrers.
    return f'''<!doctype html><meta charset="utf-8"><meta name="referrer" content="no-referrer">
<title>Connect No Paint</title><style>body{{font:18px system-ui;max-width:32em;margin:10vh auto;padding:24px;background:#eee9df;color:#202019}}a{{color:inherit}}</style>
<h1>Connect No Paint</h1><p>Let nopaint.art use this desktop painting, its inference models and its signed-in AC account while No Paint is running.</p>
<p><a href="https://nopaint.art/#companion={token}" rel="noreferrer">Connect Website</a></p>'''
