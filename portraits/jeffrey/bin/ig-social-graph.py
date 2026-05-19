#!/opt/homebrew/Cellar/instaloader/4.15.1_1/libexec/bin/python3
"""
Phase 1 of the art-world social-graph map: pull the *lightweight* 1st-degree
graph for an account — its followers and the accounts it follows — into JSONL.

Goal (see portraits/jeffrey/README.md → "Social graph / art-world map"):
map the contemporary art gallery world that @whistlegraph is embedded in —
galleries, museums, institutions, artist-run/project spaces, art media/fairs,
and the people who run or work at them. This script only fetches the cheap
node lists; classification and the costly per-profile enrichment happen in
later, prioritized passes so we never spend a risky authenticated request on
a random fan.

Account safety
--------------
Follower/followee enumeration is the single most anti-bot-flagged Instagram
operation — it is what soft-locked @whistlegraph before (see
reports/instagram-api-migration-2026-03-29.md). So this does ONE paged pass
with instaloader's own rate controller plus a sleep floor, streams each node
to disk immediately (a mid-list 401 keeps everything pulled so far), and exits
cleanly the moment Instagram returns a login/throttle error rather than
hammering. Re-run after a cooldown to continue the other list.

Usage
-----
  bin/ig-social-graph.py whistlegraph                 # both lists
  bin/ig-social-graph.py whistlegraph --only=following # one list only
  bin/ig-social-graph.py whistlegraph --sleep-floor=3

Env
---
  IG_SESSION   override session file path.

Outputs (gitignored — authenticated personal social data)
  portraits/jeffrey/social/<acct>/profile.json    account totals + run metadata
  portraits/jeffrey/social/<acct>/followers.jsonl  one node per line
  portraits/jeffrey/social/<acct>/following.jsonl  one node per line

Node row shape:
  {"username","userid","full_name","is_verified","is_private",
   "is_business_account","seen_at"}
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
from instaloader import load_structure_from_file, save_structure_to_file
from instaloader.exceptions import (
    ConnectionException,
    LoginRequiredException,
    QueryReturnedBadRequestException,
    TooManyRequestsException,
)

REPO = Path(__file__).resolve().parents[3]


def now_z() -> str:
    return (
        datetime.now(timezone.utc)
        .replace(microsecond=0)
        .isoformat()
        .replace("+00:00", "Z")
    )


def load_seen(path: Path) -> set[str]:
    seen: set[str] = set()
    if not path.exists():
        return seen
    with path.open() as fh:
        for line in fh:
            try:
                u = json.loads(line).get("username")
            except Exception:  # noqa: BLE001
                continue
            if u:
                seen.add(u)
    return seen


def node_row(p: "instaloader.Profile") -> dict:
    # All fields below come off the lightweight Profile objects the
    # followers/followees iterators yield — no extra request per node.
    return {
        "username": p.username,
        "userid": getattr(p, "userid", None),
        "full_name": getattr(p, "full_name", None),
        "is_verified": getattr(p, "is_verified", None),
        "is_private": getattr(p, "is_private", None),
        "is_business_account": getattr(p, "is_business_account", None),
        "seen_at": now_z(),
    }


def resume_path(out_path: Path) -> Path:
    # e.g. following.jsonl → following.resume.json (gitignored under social/)
    return out_path.parent / (out_path.stem + ".resume.json")


# Network resilience: an unstable *local* connection (NXDOMAIN, ECONNRESET,
# "Network is unreachable", read timeouts) is not an Instagram signal — it
# should self-heal, not abort a multi-hour pull or get manually relaunched
# every few minutes. A genuine IG throttle (401/Login-required/429) is the
# opposite: it must hard-stop immediately so we never hammer a flagged
# account. So ConnectionException → backoff + thaw + continue; the IG
# throttle exceptions → stop and return 3.
NET_RETRY_MAX = 20
NET_BACKOFF_START = 15
NET_BACKOFF_CAP = 180


def pull_list(L, profile, kind: str, out_path: Path, sleep_floor: float,
              cap: int = 0) -> int:
    """Stream one list (followers|following) to JSONL. Returns 0 on full
    completion, 3 on a genuine IG throttle or after exhausting network
    retries (resumable either way), 1 on hard error.

    Crash- and network-resilient: the instaloader page cursor is frozen to
    disk every 50 nodes and on any stop, then thawed to resume from the last
    checkpoint via cursor (no re-pagination of the already-pulled prefix).
    Pure network drops are retried in-process with exponential backoff,
    re-thawing the cursor each attempt. The JSONL seen-set is a second
    safety net that de-dups even if the cursor file is missing or stale."""
    seen = load_seen(out_path)
    total = profile.followers if kind == "followers" else profile.followees
    rpath = resume_path(out_path)
    attempt = 0
    backoff = NET_BACKOFF_START

    while True:
        attempt += 1
        iterator = (
            profile.get_followers()
            if kind == "followers"
            else profile.get_followees()
        )
        cursor_at = 0
        if rpath.exists():
            try:
                iterator.thaw(load_structure_from_file(L.context, str(rpath)))
                cursor_at = getattr(iterator, "total_index", 0)
            except Exception as e:  # noqa: BLE001 — stale/mismatched cursor
                print(
                    f"{kind}: resume cursor unusable ({e}); restarting walk "
                    f"(JSONL seen-set still de-dups)",
                    file=sys.stderr,
                )

        print(
            f"{kind}: target≈{total}  on disk={len(seen)}  cursor={cursor_at}"
            f"  attempt={attempt}/{NET_RETRY_MAX}",
            file=sys.stderr,
        )

        def freeze(tag: str) -> None:
            try:
                save_structure_to_file(iterator.freeze(), str(rpath))
            except Exception as e:  # noqa: BLE001
                print(f"  [{kind}] WARN: cursor save failed ({tag}): {e}",
                      file=sys.stderr)

        out_fh = out_path.open("a", buffering=1)
        started = time.time()
        last = 0.0
        n = 0
        try:
            for p in iterator:
                if p.username in seen:
                    continue
                gap = time.time() - last
                if gap < sleep_floor:
                    time.sleep(sleep_floor - gap)
                last = time.time()

                out_fh.write(json.dumps(node_row(p), ensure_ascii=False) + "\n")
                out_fh.flush()
                seen.add(p.username)
                n += 1

                if cap and len(seen) >= cap:
                    freeze("cap-reached")
                    out_fh.close()
                    print(
                        f"[{kind}] cap {cap} reached "
                        f"({len(seen)} on disk) → {out_path}",
                        file=sys.stderr,
                    )
                    if rpath.exists():
                        rpath.unlink()
                    return 0

                if n % 50 == 0:
                    freeze("checkpoint")  # crash-resilient cursor save
                    backoff = NET_BACKOFF_START  # healthy → reset backoff
                    rate = n / max(time.time() - started, 1e-6)
                    print(
                        f"  [{kind}] +{n} new  ({len(seen)}/{total})  "
                        f"{rate:.2f}/s  last=@{p.username}  [cursor saved]",
                        file=sys.stderr,
                    )
        except (
            LoginRequiredException,
            TooManyRequestsException,
            QueryReturnedBadRequestException,
        ) as e:
            freeze("throttle-stop")
            out_fh.close()
            print(
                f"\n[{kind}] GENUINE IG throttle after +{n} this leg "
                f"({len(seen)}/{total} on disk): {e}\n"
                f"  hard stop (account safety). Cool down, then re-run — "
                f"cursor saved, will thaw.",
                file=sys.stderr,
            )
            return 3
        except ConnectionException as e:
            freeze("network-drop")
            out_fh.close()
            if attempt >= NET_RETRY_MAX:
                print(
                    f"\n[{kind}] network unreachable, gave up after "
                    f"{attempt} attempts ({len(seen)}/{total} on disk). "
                    f"Not an IG block — just re-run when your connection is "
                    f"back; cursor saved.",
                    file=sys.stderr,
                )
                return 3
            wait = min(backoff, NET_BACKOFF_CAP)
            print(
                f"\n[{kind}] network drop after +{n} this leg "
                f"({len(seen)}/{total} on disk) — NOT an IG block. "
                f"retry {attempt}/{NET_RETRY_MAX} in {wait}s; cursor saved.",
                file=sys.stderr,
            )
            time.sleep(wait)
            backoff = min(backoff * 2, NET_BACKOFF_CAP)
            continue  # rebuild iterator, thaw cursor, resume

        out_fh.close()
        print(f"[{kind}] complete: {len(seen)} nodes → {out_path}",
              file=sys.stderr)
        if rpath.exists():
            rpath.unlink()  # clean run → drop the cursor
        return 0


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("account")
    ap.add_argument(
        "--only",
        choices=["followers", "following"],
        help="pull just one list (default: both)",
    )
    ap.add_argument(
        "--sleep-floor",
        type=float,
        default=2.0,
        help="min seconds between node writes (instaloader throttles on top)",
    )
    ap.add_argument(
        "--target",
        help="pull the FOLLOWERS of this other account instead of the "
        "session account's own lists (auth still via <account>). Opt-in, "
        "account-risky third-party enumeration — always use with --cap. "
        "Writes social/<account>/targets/<target>.followers.jsonl.",
    )
    ap.add_argument(
        "--cap",
        type=int,
        default=0,
        help="hard max nodes for a --target pull (0 = no cap; required in "
        "spirit for third-party scrapes — keep it bounded).",
    )
    args = ap.parse_args()

    out_dir = REPO / "portraits" / "jeffrey" / "social" / args.account
    out_dir.mkdir(parents=True, exist_ok=True)
    session_file = os.environ.get("IG_SESSION") or str(
        REPO / "portraits" / "jeffrey" / "sessions" / args.account
    )
    if not Path(session_file).exists():
        print(
            f"missing session file: {session_file}\n"
            f"bootstrap: bin/ig-import-cookies.py chrome {args.account}",
            file=sys.stderr,
        )
        return 1

    L = instaloader.Instaloader(
        max_connection_attempts=2,
        request_timeout=20,
        sleep=True,
    )
    L.load_session_from_file(args.account, filename=session_file)
    if not L.test_login():
        print(
            "session not logged in (stale or throttled). Refresh with\n"
            f"  bin/ig-import-cookies.py chrome {args.account}\n"
            "and, if Instagram says 'wait a few minutes', wait out the "
            "cooldown before retrying.",
            file=sys.stderr,
        )
        return 2

    # --target: enumerate ANOTHER account's followers (capped, opt-in) into
    # targets/<target>.followers.jsonl. Auth is still the session account.
    if args.target:
        tgt = args.target.lstrip("@")
        profile = instaloader.Profile.from_username(L.context, tgt)
        tdir = out_dir / "targets"
        tdir.mkdir(parents=True, exist_ok=True)
        if not args.cap:
            print(
                "refusing an uncapped third-party follower scrape — pass "
                "--cap N (e.g. --cap 5000). This is the ban-risky op.",
                file=sys.stderr,
            )
            return 64
        print(
            f"TARGET pull: @{tgt} followers≈{profile.followers}, "
            f"cap={args.cap}, sleep-floor={args.sleep_floor}",
            file=sys.stderr,
        )
        rc = pull_list(
            L, profile, "followers",
            tdir / f"{tgt}.followers.jsonl",
            args.sleep_floor, cap=args.cap,
        )
        return rc

    profile = instaloader.Profile.from_username(L.context, args.account)
    profile_path = out_dir / "profile.json"
    profile_path.write_text(
        json.dumps(
            {
                "username": profile.username,
                "userid": profile.userid,
                "full_name": profile.full_name,
                "followers": profile.followers,
                "followees": profile.followees,
                "mediacount": profile.mediacount,
                "is_verified": profile.is_verified,
                "fetched_at": now_z(),
            },
            ensure_ascii=False,
            indent=2,
        )
    )

    lists = (
        [args.only]
        if args.only
        else ["following", "followers"]  # smaller list first → fail cheap
    )
    rc = 0
    for kind in lists:
        out_path = out_dir / f"{kind}.jsonl"
        rc = pull_list(L, profile, kind, out_path, args.sleep_floor)
        if rc != 0:
            return rc
    return rc


if __name__ == "__main__":
    sys.exit(main())
