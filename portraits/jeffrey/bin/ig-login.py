#!/opt/homebrew/Cellar/instaloader/4.15.1_1/libexec/bin/python3
"""
Login to Instagram via instaloader and save a session for re-use.

Usage:
  IG_PASSWORD='...' python3 ig-login.py whistlegraph

Reads the password from $IG_PASSWORD (never from argv, so it doesn't
land in process listings or shell history). Saves the session cookie to
portraits/jeffrey/sessions/<username>.

If 2FA is required, prints a 2FA-needed marker and exits 2 — re-run
interactively (drop the IG_PASSWORD env var) so instaloader can prompt
for the code.
"""

from __future__ import annotations

import os
import sys
from pathlib import Path

import instaloader
from instaloader.exceptions import (
    BadCredentialsException,
    ConnectionException,
    TwoFactorAuthRequiredException,
)

REPO_ROOT = Path(__file__).resolve().parent.parent.parent.parent
SESSION_DIR = REPO_ROOT / "portraits" / "jeffrey" / "sessions"


def main() -> int:
    if len(sys.argv) != 2:
        print("usage: ig-login.py <username>", file=sys.stderr)
        return 64

    username = sys.argv[1]
    password = os.environ.get("IG_PASSWORD")
    if not password:
        print("error: IG_PASSWORD env var is required", file=sys.stderr)
        return 64

    SESSION_DIR.mkdir(parents=True, exist_ok=True)
    session_path = SESSION_DIR / username

    L = instaloader.Instaloader(
        save_metadata=False,
        download_pictures=False,
        download_videos=False,
        download_video_thumbnails=False,
        download_geotags=False,
        download_comments=False,
    )

    try:
        L.login(username, password)
    except TwoFactorAuthRequiredException:
        print("2fa_required", file=sys.stderr)
        return 2
    except BadCredentialsException as e:
        print(f"bad_credentials: {e}", file=sys.stderr)
        return 3
    except ConnectionException as e:
        print(f"connection_error: {e}", file=sys.stderr)
        return 4

    L.save_session_to_file(str(session_path))
    print(f"session saved: {session_path.relative_to(REPO_ROOT)}")

    profile = instaloader.Profile.from_username(L.context, username)
    print(
        f"logged in as @{profile.username}: "
        f"{profile.full_name!r}, "
        f"{profile.mediacount} posts, "
        f"{profile.followers} followers"
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
