#!/usr/bin/env python3
"""
ig-grid-archive-api.py — Archive ("private") every grid post in our curated
index via Instagram's private mobile API (instagrapi), newest→oldest.

ACCOUNT-SAFETY DISCIPLINE
─────────────────────────
- One-time bootstrap pins a device fingerprint via the existing
  instaloader sessionid; future runs reuse the same instagrapi settings,
  so IG sees a stable "device" across days/weeks of archiving.
- Conservative throttle: 300–600s between archives (5–10 min) by default.
- HARD STOP on any instagrapi exception (rate limit, challenge, login
  required, anything unexpected). The script bails — does NOT retry blindly.
- Resumable: every successful archive is appended to whistlegraph-archived.jsonl.
- Dry-run by default (fetches media_id only, never calls media_archive).
- Always preserves the chronologically-first grid post (the "Hi!" post).

USAGE
─────
    .venv-instagrapi/bin/python bin/ig-grid-archive-api.py --bootstrap
        # One time: trades our instaloader sessionid for an instagrapi
        # session and saves device settings to sessions/whistlegraph.instagrapi.json

    .venv-instagrapi/bin/python bin/ig-grid-archive-api.py
        # Dry-run — verifies each post is reachable + owned by us. No archive.

    .venv-instagrapi/bin/python bin/ig-grid-archive-api.py --live --limit=1
        # Live single-post test on the newest grid post.

    .venv-instagrapi/bin/python bin/ig-grid-archive-api.py --live \\
        --min-delay=300 --max-delay=600
        # Full run, 5–10 min/post.

Flags:
    --account=whistlegraph
    --bootstrap          one-time session pinning
    --live               actually call media_archive()
    --limit=N            stop after N archives
    --keep=<shortcode>   never touch this shortcode (default: oldest)
    --min-delay=300      seconds (clamped ≥ 60)
    --max-delay=600      seconds (clamped ≤ min-delay + 10*60)
"""

import argparse
import json
import os
import pickle
import random
import sys
import time
from pathlib import Path

REPO = Path(__file__).resolve().parents[3]

# ── args ─────────────────────────────────────────────────────────────
ap = argparse.ArgumentParser()
ap.add_argument("--account", default="whistlegraph")
ap.add_argument("--bootstrap", action="store_true")
ap.add_argument("--live", action="store_true")
ap.add_argument("--limit", type=int, default=0)  # 0 = no limit
ap.add_argument("--keep", default=None)
ap.add_argument("--min-delay", type=int, default=300)
ap.add_argument("--max-delay", type=int, default=600)
args = ap.parse_args()

ACCOUNT = args.account
MIN_DELAY = max(60, args.min_delay)
MAX_DELAY = max(MIN_DELAY + 1, args.max_delay)

# ── paths ────────────────────────────────────────────────────────────
INDEX_PATH = REPO / "portraits/jeffrey/curated" / f"{ACCOUNT}-grid.json"
ARCHIVED_PATH = REPO / "portraits/jeffrey/curated" / f"{ACCOUNT}-archived.jsonl"
FAILED_PATH = REPO / "portraits/jeffrey/curated" / f"{ACCOUNT}-archive-api-failed.jsonl"
INSTALOADER_SESSION = REPO / "portraits/jeffrey/sessions" / ACCOUNT
SETTINGS_PATH = REPO / "portraits/jeffrey/sessions" / f"{ACCOUNT}.instagrapi.json"

try:
    from instagrapi import Client
    from instagrapi.exceptions import (
        LoginRequired,
        ClientForbiddenError,
        ChallengeRequired,
        FeedbackRequired,
        RateLimitError,
        PleaseWaitFewMinutes,
        ClientError,
    )
except ImportError:
    print("instagrapi not installed.  run:", file=sys.stderr)
    print(
        "  python3 -m venv portraits/jeffrey/.venv-instagrapi && "
        "portraits/jeffrey/.venv-instagrapi/bin/pip install instagrapi",
        file=sys.stderr,
    )
    sys.exit(1)


def load_sessionid_from_instaloader() -> str:
    with open(INSTALOADER_SESSION, "rb") as f:
        jar = pickle.load(f)
    sid = jar.get("sessionid")
    if not sid:
        print(f"no sessionid in {INSTALOADER_SESSION}", file=sys.stderr)
        sys.exit(1)
    return sid


def build_client() -> Client:
    cl = Client()
    # mild jitter on internal calls — instagrapi already adds some
    cl.delay_range = [1, 3]
    if SETTINGS_PATH.exists():
        cl.load_settings(SETTINGS_PATH)
        # Light validity check — account_info hits /api/v1/users/{pk}/info/
        # which works on freshly-pinned sessions; timeline_feed does not.
        try:
            cl.account_info()
        except Exception as e:
            print(f"saved instagrapi session no longer valid: {e}", file=sys.stderr)
            print("re-run with --bootstrap", file=sys.stderr)
            sys.exit(1)
        return cl

    if not args.bootstrap:
        print(
            f"no settings at {SETTINGS_PATH} — run with --bootstrap first.",
            file=sys.stderr,
        )
        sys.exit(1)

    sid = load_sessionid_from_instaloader()
    print(f"▸ bootstrapping instagrapi session from instaloader sessionid…")
    cl.login_by_sessionid(sid)
    # Warm the session so response headers populate mid/ig_u_rur/ig_www_claim
    # BEFORE we serialize state — otherwise reload sees those fields null and
    # IG rejects subsequent requests with login_required.
    me = cl.account_info()
    print(f"✓ session warm — logged in as @{me.username} (pk={me.pk})")
    cl.dump_settings(SETTINGS_PATH)
    print(f"✓ saved settings → {SETTINGS_PATH}")
    return cl


# ── bootstrap-only path ──────────────────────────────────────────────
if args.bootstrap:
    cl = build_client()
    print("bootstrap complete. re-run without --bootstrap to dry-run.")
    sys.exit(0)


# ── load index + resume ──────────────────────────────────────────────
if not INDEX_PATH.exists():
    print(f"missing {INDEX_PATH} — run ig-grid-index.mjs first", file=sys.stderr)
    sys.exit(1)

index = json.loads(INDEX_PATH.read_text())
keep_shortcode = args.keep or index["first_post"]["shortcode"]

done = set()
if ARCHIVED_PATH.exists():
    for line in ARCHIVED_PATH.read_text().splitlines():
        if not line:
            continue
        try:
            done.add(json.loads(line)["shortcode"])
        except Exception:
            pass

queue = [
    p for p in index["posts"]
    if p["shortcode"] != keep_shortcode and p["shortcode"] not in done
]

print("══ ig-grid-archive-api ══")
print(f"  account:        @{ACCOUNT}")
print(f"  total grid:     {len(index['posts'])}")
print(f"  keep visible:   {keep_shortcode} ({index['first_post']['date']})")
print(f"  already done:   {len(done)}")
print(f"  to archive:     {len(queue)}")
print(f"  delay range:    {MIN_DELAY}–{MAX_DELAY}s ({MIN_DELAY/60:.1f}–{MAX_DELAY/60:.1f} min)")
print(f"  mode:           {'LIVE (will call media_archive)' if args.live else 'DRY-RUN (read-only)'}")
print(f"  limit:          {args.limit if args.limit else 'none'}")
print("")

if not queue:
    print("nothing to do.")
    sys.exit(0)

cl = build_client()
me = cl.account_info()
print(f"✓ session active as @{me.username}  (pk={me.pk})")
print("")

archived = 0
consecutive_fails = 0


def record_done(post, extra=None):
    ARCHIVED_PATH.parent.mkdir(parents=True, exist_ok=True)
    row = {
        "shortcode": post["shortcode"],
        "url": post["url"],
        "taken_at": post["taken_at"],
        "date": post["date"],
        "archived_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "via": "instagrapi",
    }
    if extra:
        row.update(extra)
    with open(ARCHIVED_PATH, "a") as f:
        f.write(json.dumps(row) + "\n")


def record_fail(post, reason):
    FAILED_PATH.parent.mkdir(parents=True, exist_ok=True)
    with open(FAILED_PATH, "a") as f:
        f.write(
            json.dumps({
                "shortcode": post["shortcode"],
                "url": post["url"],
                "date": post["date"],
                "failed_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
                "reason": reason,
            }) + "\n"
        )


def bail(reason):
    print(f"\n⛔ {reason}")
    print(f"   archived {archived}, {len(queue) - archived} remain.")
    print(f"   re-run to resume.")
    sys.exit(1)


limit = args.limit if args.limit else len(queue)

for post in queue:
    if archived >= limit:
        print(f"\n✓ hit --limit={limit}, stopping.")
        break

    idx = archived + 1
    marker = "▶" if post.get("is_video") else " "
    print(
        f"[{idx}/{min(len(queue), limit)}] {post['date']}  {post['shortcode']}  "
        f"♥ {post['likes']}  💬 {post['comments']}  {marker}"
    )

    try:
        media_pk = cl.media_pk_from_code(post["shortcode"])
        if args.live:
            cl.media_archive(media_pk)
            print(f"   ↳ archived (media_pk={media_pk})")
        else:
            print(f"   ↳ dry-run, resolved media_pk={media_pk}")
        record_done(post, {"media_pk": str(media_pk), "dry_run": not args.live})
        archived += 1
        consecutive_fails = 0

    except (LoginRequired, ChallengeRequired) as e:
        record_fail(post, f"{type(e).__name__}: {e}")
        bail(f"session lost / challenge required: {e}")
    except (RateLimitError, PleaseWaitFewMinutes, FeedbackRequired) as e:
        record_fail(post, f"{type(e).__name__}: {e}")
        bail(f"rate-limited by IG: {e}")
    except ClientForbiddenError as e:
        record_fail(post, f"ClientForbiddenError: {e}")
        bail(f"forbidden (possibly account flagged): {e}")
    except ClientError as e:
        print(f"   ↳ ClientError: {e}")
        record_fail(post, f"ClientError: {e}")
        consecutive_fails += 1
    except Exception as e:
        print(f"   ↳ unexpected: {type(e).__name__}: {e}")
        record_fail(post, f"{type(e).__name__}: {e}")
        consecutive_fails += 1

    if consecutive_fails >= 3:
        bail("3 consecutive failures — stop and inspect.")

    if archived < limit and archived < len(queue):
        wait = random.uniform(MIN_DELAY, MAX_DELAY)
        print(f"   sleep {wait:.0f}s  ({wait/60:.1f} min)")
        time.sleep(wait)

print(f"\n✓ done. archived {archived} post(s).")
