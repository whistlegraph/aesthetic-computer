#!/opt/homebrew/Cellar/instaloader/4.15.1_1/libexec/bin/python3
"""
Import an Instagram session from a logged-in browser into an instaloader
session file. Works around password-flow login bugs on accounts where
2FA / soft-locks silently strip the auth cookie.

Usage:
  ig-import-cookies.py <browser> <username>

  <browser>: chrome | firefox | safari | brave | edge | arc | chromium | opera
  <username>: instagram username (used to name the session file)

Requires browser-cookie3. On macOS, Chromium-family browsers will prompt for
Keychain access on first run — click "Always Allow" so subsequent runs are
silent. Firefox cookies are unencrypted so no prompt.

The script:
  1. Pulls instagram.com cookies from the named browser via browser-cookie3.
  2. Calls instaloader.test_login() to confirm a real session is present.
  3. Saves an instaloader session file at
     portraits/jeffrey/sessions/<username>.
"""

from __future__ import annotations

import sys
from pathlib import Path

import browser_cookie3
import instaloader

REPO_ROOT = Path(__file__).resolve().parent.parent.parent.parent
SESSION_DIR = REPO_ROOT / "portraits" / "jeffrey" / "sessions"

BROWSERS = {
    "chrome": browser_cookie3.chrome,
    "firefox": browser_cookie3.firefox,
    "safari": browser_cookie3.safari,
    "brave": browser_cookie3.brave,
    "edge": browser_cookie3.edge,
    "arc": browser_cookie3.arc,
    "chromium": browser_cookie3.chromium,
    "opera": browser_cookie3.opera,
}


def main() -> int:
    if len(sys.argv) != 3:
        print(__doc__.strip(), file=sys.stderr)
        return 64

    browser_name, username = sys.argv[1].lower(), sys.argv[2]
    if browser_name not in BROWSERS:
        print(
            f"unknown browser: {browser_name}. Supported: {', '.join(BROWSERS)}",
            file=sys.stderr,
        )
        return 64

    print(f"reading instagram.com cookies from {browser_name}…", file=sys.stderr)
    try:
        cookies = BROWSERS[browser_name](domain_name="instagram.com")
    except browser_cookie3.BrowserCookieError as e:
        print(f"failed to read {browser_name} cookies: {e}", file=sys.stderr)
        return 1

    cookie_dict = {c.name: c.value for c in cookies}
    if "sessionid" not in cookie_dict or not cookie_dict["sessionid"]:
        print(
            "no sessionid cookie found — make sure you're logged in to "
            f"instagram.com in {browser_name}.",
            file=sys.stderr,
        )
        return 2

    L = instaloader.Instaloader(max_connection_attempts=1)
    L.context._session.cookies.update(cookie_dict)

    detected = L.test_login()
    if not detected:
        print(
            "test_login() failed — cookies are present but Instagram doesn't "
            "recognize them. Refresh the IG tab and retry.",
            file=sys.stderr,
        )
        return 3
    if detected.lower() != username.lower():
        print(
            f"warning: browser is logged in as @{detected}, not @{username}. "
            f"Saving session for @{detected} instead.",
            file=sys.stderr,
        )
        username = detected

    L.context.username = username
    SESSION_DIR.mkdir(parents=True, exist_ok=True)
    session_path = SESSION_DIR / username
    L.save_session_to_file(str(session_path))
    print(f"session saved: {session_path.relative_to(REPO_ROOT)}")
    print(f"authenticated as @{username}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
