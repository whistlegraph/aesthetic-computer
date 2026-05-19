#!/opt/homebrew/Cellar/instaloader/4.15.1_1/libexec/bin/python3
"""
Backfill comment threads for already-archived @whistlegraph grid posts.

Reads `curated/whistlegraph-meta.jsonl` to enumerate shortcodes whose
`typename` is a post (GraphImage/GraphVideo/GraphSidecar). For each, fetches
the comment thread via instaloader's authenticated session and appends one
row per post to `curated/whistlegraph-comments.jsonl`.

Resumable: shortcodes already present in the output JSONL are skipped, so
re-running after a throttle hit just continues where it left off.

Usage:
  bin/ig-pull-comments.py [--account=whistlegraph] [--limit=N]
                          [--max-comments=N] [--sleep-floor=SECS]

Env:
  IG_SESSION  override session file path.

Output rows shape (one per post):
  {
    "shortcode": "ABC123",
    "fetched_at": "2026-05-08T12:34:56Z",
    "comment_count": 12,
    "comments": [
      {"id":"...","owner":"jas.life","text":"...","created_at":1700000000,
       "likes_count":2,"answers":[ ... ]}
    ]
  }
"""

from __future__ import annotations

import argparse
import json
import os
import sys
import time
from datetime import datetime, timezone
from pathlib import Path

import instaloader
from instaloader.exceptions import (
    ConnectionException,
    LoginRequiredException,
    QueryReturnedBadRequestException,
    QueryReturnedNotFoundException,
)

REPO = Path(__file__).resolve().parents[3]
POST_TYPES = {"GraphImage", "GraphVideo", "GraphSidecar"}


def load_done(path: Path) -> set[str]:
    done = set()
    if not path.exists():
        return done
    with path.open() as fh:
        for line in fh:
            try:
                row = json.loads(line)
            except Exception:  # noqa: BLE001
                continue
            sc = row.get("shortcode")
            if sc:
                done.add(sc)
    return done


def to_dict(c) -> dict:
    """Flatten an instaloader PostComment into a JSON-safe dict."""
    base = {
        "id": getattr(c, "id", None),
        "owner": getattr(getattr(c, "owner", None), "username", None),
        "text": getattr(c, "text", None),
        "created_at": int(c.created_at_utc.timestamp())
        if getattr(c, "created_at_utc", None)
        else None,
        "likes_count": getattr(c, "likes_count", None),
    }
    answers = []
    for a in getattr(c, "answers", []) or []:
        answers.append(
            {
                "id": getattr(a, "id", None),
                "owner": getattr(getattr(a, "owner", None), "username", None),
                "text": getattr(a, "text", None),
                "created_at": int(a.created_at_utc.timestamp())
                if getattr(a, "created_at_utc", None)
                else None,
                "likes_count": getattr(a, "likes_count", None),
            }
        )
    if answers:
        base["answers"] = answers
    return base


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--account", default="whistlegraph")
    ap.add_argument("--limit", type=int, default=0,
                    help="cap posts processed this run (0 = no cap)")
    ap.add_argument("--max-comments", type=int, default=0,
                    help="max comments per post (0 = all)")
    ap.add_argument("--sleep-floor", type=float, default=1.5,
                    help="minimum seconds between posts (instaloader has its own throttle on top)")
    args = ap.parse_args()

    meta_path = REPO / "portraits/jeffrey/curated" / f"{args.account}-meta.jsonl"
    out_path = REPO / "portraits/jeffrey/curated" / f"{args.account}-comments.jsonl"
    session_file = (
        os.environ.get("IG_SESSION")
        or str(REPO / "portraits/jeffrey/sessions" / args.account)
    )

    if not meta_path.exists():
        print(f"missing meta catalog: {meta_path}", file=sys.stderr)
        return 1
    if not Path(session_file).exists():
        print(f"missing session file: {session_file}", file=sys.stderr)
        return 1

    # Build the list of shortcodes to fetch (posts only, skipping already-done).
    todo: list[str] = []
    with meta_path.open() as fh:
        for line in fh:
            try:
                row = json.loads(line)
            except Exception:  # noqa: BLE001
                continue
            if row.get("error"):
                continue
            if row.get("typename") not in POST_TYPES:
                continue
            todo.append(row["shortcode"])

    done = load_done(out_path)
    pending = [sc for sc in todo if sc not in done]
    if args.limit:
        pending = pending[: args.limit]

    print(
        f"meta posts: {len(todo)}  done: {len(done)}  pending: {len(pending)}",
        file=sys.stderr,
    )
    if not pending:
        print("nothing to do.", file=sys.stderr)
        return 0

    L = instaloader.Instaloader(
        max_connection_attempts=2,
        request_timeout=20,
        sleep=True,
    )
    L.load_session_from_file(args.account, filename=session_file)
    if not L.test_login():
        print("session not logged in; refresh via bin/ig-import-cookies.py", file=sys.stderr)
        return 2

    started = time.time()
    last_request = 0.0

    out_fh = out_path.open("a", buffering=1)
    try:
        for i, sc in enumerate(pending, 1):
            # Floor sleep on top of instaloader's own throttle.
            elapsed = time.time() - last_request
            if elapsed < args.sleep_floor:
                time.sleep(args.sleep_floor - elapsed)
            last_request = time.time()

            try:
                post = instaloader.Post.from_shortcode(L.context, sc)
                comments_iter = post.get_comments()
                comments = []
                for j, c in enumerate(comments_iter):
                    comments.append(to_dict(c))
                    if args.max_comments and j + 1 >= args.max_comments:
                        break
                row = {
                    "shortcode": sc,
                    "fetched_at": datetime.now(timezone.utc)
                    .replace(microsecond=0)
                    .isoformat()
                    .replace("+00:00", "Z"),
                    "comment_count": len(comments),
                    "comments": comments,
                }
            except QueryReturnedNotFoundException:
                row = {"shortcode": sc, "error": "not_found",
                       "fetched_at": datetime.now(timezone.utc)
                       .replace(microsecond=0).isoformat().replace("+00:00", "Z")}
            except (LoginRequiredException, QueryReturnedBadRequestException) as e:
                print(f"\nauth/throttle error after {i-1} pulled: {e}", file=sys.stderr)
                return 3
            except ConnectionException as e:
                # Transient — log and continue
                print(f"\n[{sc}] connection error: {e}", file=sys.stderr)
                row = {"shortcode": sc, "error": f"connection: {e}",
                       "fetched_at": datetime.now(timezone.utc)
                       .replace(microsecond=0).isoformat().replace("+00:00", "Z")}

            out_fh.write(json.dumps(row, ensure_ascii=False) + "\n")

            if i % 25 == 0 or i == len(pending):
                rate = i / max(time.time() - started, 1e-6)
                eta = (len(pending) - i) / max(rate, 1e-6)
                print(
                    f"  [{i}/{len(pending)}] rate={rate:.2f} posts/s  "
                    f"eta={eta/60:.1f} min  last={sc}",
                    file=sys.stderr,
                )
    finally:
        out_fh.close()

    print(f"done. wrote rows to {out_path}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
