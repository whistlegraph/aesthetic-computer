#!/opt/homebrew/Cellar/instaloader/4.15.1_1/libexec/bin/python3
"""
Phase 2 of the art-world social-graph map: enrich the PRIORITIZED art-world
candidates with Instagram's own metadata, and parse who-runs-what.

This is the risky pass. Each node here costs one authenticated
Profile.from_username request — the kind of traffic that soft-locked
@whistlegraph before. Prioritization *is* the risk mitigation: we read the
pre-sorted work order from candidates.json (galleries/museums/institutions/
art-media/curators first, generic fans never) and stop at a hard --cap, so
the scarce request budget only ever buys art-world signal.

For each enriched node we capture:
  - Instagram's own category_name / business_category_name
    ("Art Gallery", "Museum", "Artist", "Art", "Performance & Event Venue"…)
    — the authoritative type, far better than the name heuristics.
  - biography, external_url, follower/followee counts.
  - affiliations: @-mentions in the bio paired with role words
    ("Director @x", "Curator at @y", "Founder of @z") → person→institution
    edges. This is how "the people who run / work at them" gets mapped.

Account safety: instaloader rate controller + sleep floor, resumable
(usernames already in profiles.jsonl are skipped), clean exit on the first
login/throttle error. Re-run after a cooldown to continue down the list.

Usage
-----
  bin/ig-social-enrich.py whistlegraph                 # art-world, cap 150
  bin/ig-social-enrich.py whistlegraph --cap 60 --sleep-floor 6
  bin/ig-social-enrich.py whistlegraph --include-fans   # also non-art nodes
  bin/ig-social-enrich.py whistlegraph --min-score 30

Env
---
  IG_SESSION   override session file path.

Reads:  portraits/jeffrey/social/<acct>/candidates.json   (work order)
Writes: portraits/jeffrey/social/<acct>/profiles.jsonl     (one node/line)
"""

from __future__ import annotations

import argparse
import json
import os
import re
import sys
import time
from datetime import datetime, timezone
from pathlib import Path

import instaloader
from instaloader.exceptions import (
    ConnectionException,
    LoginRequiredException,
    ProfileNotExistsException,
    QueryReturnedBadRequestException,
    QueryReturnedNotFoundException,
    TooManyRequestsException,
)

REPO = Path(__file__).resolve().parents[3]

MENTION_RE = re.compile(r"@([A-Za-z0-9_.]{2,30})")
ROLE_WORDS = [
    "founder",
    "co-founder",
    "cofounder",
    "director",
    "co-director",
    "artistic director",
    "executive director",
    "managing director",
    "chief curator",
    "curator",
    "curatorial",
    "gallerist",
    "dealer",
    "registrar",
    "preparator",
    "art handler",
    "archivist",
    "owner",
    "partner",
    "president",
    "chair",
    "head of",
    "advisor",
    "consultant",
    "represented by",
    "represents",
    "studio manager",
]
ROLE_RE = re.compile(
    r"(" + "|".join(re.escape(w) for w in ROLE_WORDS) + r")", re.IGNORECASE
)


def now_z() -> str:
    return (
        datetime.now(timezone.utc)
        .replace(microsecond=0)
        .isoformat()
        .replace("+00:00", "Z")
    )


def load_done(path: Path) -> set[str]:
    done: set[str] = set()
    if not path.exists():
        return done
    with path.open() as fh:
        for line in fh:
            try:
                row = json.loads(line)
            except Exception:  # noqa: BLE001
                continue
            u = row.get("username")
            # Only treat a node as done if it enriched cleanly. Rows with an
            # "error" (network blip / transient) are intentionally NOT marked
            # done so a re-run retries them — important on a flaky network.
            if u and not row.get("error"):
                done.add(u)
    return done


def category_of(p: "instaloader.Profile"):
    """Instagram's own account category — the authoritative type signal.
    Different IG payload versions expose it under different keys."""
    for attr in ("business_category_name",):
        v = getattr(p, attr, None)
        if v:
            return v
    for key in ("category_name", "category_enum"):
        try:
            v = p._metadata(key)  # noqa: SLF001 — instaloader's own accessor
            if v:
                return v
        except Exception:  # noqa: BLE001
            pass
    return None


def parse_affiliations(bio: str) -> list[dict]:
    """Pair @-mentions with the nearest preceding role word in the bio.
    'Director @gagosian · Curator at @kw_institute' →
      [{role:'director', handle:'gagosian'}, {role:'curator', handle:'kw_institute'}]
    Mentions with no nearby role word are still kept as plain edges."""
    if not bio:
        return []
    out: list[dict] = []
    for m in MENTION_RE.finditer(bio):
        handle = m.group(1).strip(".").lower()
        window = bio[max(0, m.start() - 60) : m.start()]
        rm = None
        for r in ROLE_RE.finditer(window):
            rm = r  # last role word before the mention wins
        out.append(
            {
                "handle": handle,
                "role": rm.group(1).lower() if rm else None,
            }
        )
    # de-dupe on (handle, role)
    seen = set()
    uniq = []
    for a in out:
        k = (a["handle"], a["role"])
        if k in seen:
            continue
        seen.add(k)
        uniq.append(a)
    return uniq


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("account")
    ap.add_argument("--cap", type=int, default=150,
                    help="hard max profiles fetched this run")
    ap.add_argument("--sleep-floor", type=float, default=5.0,
                    help="min seconds between profile fetches")
    ap.add_argument("--min-score", type=int, default=1,
                    help="skip candidates scoring below this")
    ap.add_argument("--include-fans", action="store_true",
                    help="also enrich non-art-world nodes (default: skip)")
    ap.add_argument("--from-list", metavar="FILE",
                    help="enrich exactly the usernames in FILE (one per "
                         "line), in that order. Explicit, auditable target "
                         "set — bypasses the art_world/min-score/include-fans "
                         "gates (still resumable, capped, throttle-stopped).")
    args = ap.parse_args()

    social_dir = REPO / "portraits" / "jeffrey" / "social" / args.account
    cand_path = social_dir / "candidates.json"
    out_path = social_dir / "profiles.jsonl"
    session_file = os.environ.get("IG_SESSION") or str(
        REPO / "portraits" / "jeffrey" / "sessions" / args.account
    )

    if not cand_path.exists():
        print(
            f"missing {cand_path} — run "
            f"node bin/ig-artworld-classify.mjs {args.account} first.",
            file=sys.stderr,
        )
        return 1
    if not Path(session_file).exists():
        print(f"missing session: {session_file}", file=sys.stderr)
        return 1

    candidates = json.loads(cand_path.read_text())["candidates"]
    by_user = {c["username"]: c for c in candidates}
    done = load_done(out_path)

    if args.from_list:
        list_path = Path(args.from_list)
        if not list_path.is_absolute():
            list_path = social_dir / list_path.name
        if not list_path.exists():
            print(f"missing --from-list file: {list_path}", file=sys.stderr)
            return 1
        wanted = [
            u.strip()
            for u in list_path.read_text().splitlines()
            if u.strip() and not u.startswith("#")
        ]
        # Preserve the file's order; carry classifier metadata when known.
        work = [
            by_user.get(
                u,
                {"username": u, "type": "other", "score": 0,
                 "art_world": False, "relation": None},
            )
            for u in wanted
            if u not in done
        ][: args.cap]
    else:
        work = [
            c
            for c in candidates  # already sorted: art-world first, by score
            if c["username"] not in done
            and c["score"] >= args.min_score
            and (args.include_fans or c["art_world"])
        ][: args.cap]

    print(
        f"candidates={len(candidates)} done={len(done)} "
        f"eligible-this-run={len(work)} (cap={args.cap})",
        file=sys.stderr,
    )
    if not work:
        print("nothing to do.", file=sys.stderr)
        return 0

    L = instaloader.Instaloader(
        max_connection_attempts=3, request_timeout=20, sleep=True
    )
    L.load_session_from_file(args.account, filename=session_file)
    if not L.test_login():
        print(
            "session not logged in (stale/throttled). Refresh via "
            f"bin/ig-import-cookies.py chrome {args.account} and wait out "
            "any 'try again in a few minutes' cooldown.",
            file=sys.stderr,
        )
        return 2

    out_fh = out_path.open("a", buffering=1)
    started = time.time()
    last = 0.0
    n = 0
    try:
        for i, c in enumerate(work, 1):
            gap = time.time() - last
            if gap < args.sleep_floor:
                time.sleep(args.sleep_floor - gap)
            last = time.time()

            uname = c["username"]
            # Per-profile retry with backoff: a flaky LOCAL network should
            # retry the same profile, not burn it to an error row and march
            # on (that produced 441/600 error rows on one drop-prone run).
            # A genuine IG throttle still hard-stops immediately — account
            # safety is never traded for resilience.
            row = None
            for attempt in range(1, 5):
                try:
                    p = instaloader.Profile.from_username(L.context, uname)
                    bio = p.biography or ""
                    row = {
                        "username": uname,
                        "userid": p.userid,
                        "full_name": p.full_name,
                        "ig_category": category_of(p),
                        "is_business_account": p.is_business_account,
                        "is_verified": p.is_verified,
                        "is_private": p.is_private,
                        "followers": p.followers,
                        "followees": p.followees,
                        "mediacount": p.mediacount,
                        "external_url": p.external_url,
                        "biography": bio,
                        "affiliations": parse_affiliations(bio),
                        "heuristic_type": c["type"],
                        "heuristic_score": c["score"],
                        "relation": c["relation"],
                        "fetched_at": now_z(),
                    }
                    n += 1
                    break
                except (ProfileNotExistsException,
                        QueryReturnedNotFoundException):
                    row = {"username": uname, "error": "not_found",
                           "fetched_at": now_z()}
                    break
                except (
                    LoginRequiredException,
                    TooManyRequestsException,
                    QueryReturnedBadRequestException,
                ) as e:
                    print(
                        f"\nGENUINE IG throttle after {n} enriched this run: "
                        f"{e}\n  hard stop (account safety); progress saved to "
                        f"{out_path.name}, cool down then re-run.",
                        file=sys.stderr,
                    )
                    return 3
                except ConnectionException as e:
                    if attempt >= 4:
                        print(
                            f"\n[@{uname}] network drop, gave up after "
                            f"{attempt} tries (NOT IG): {e}",
                            file=sys.stderr,
                        )
                        row = {"username": uname,
                               "error": f"connection: {e}",
                               "fetched_at": now_z()}
                        break
                    wait = min(10 * attempt, 60)
                    print(
                        f"  [@{uname}] network drop — retry "
                        f"{attempt}/4 in {wait}s (NOT IG)",
                        file=sys.stderr,
                    )
                    time.sleep(wait)

            out_fh.write(json.dumps(row, ensure_ascii=False) + "\n")

            if i % 10 == 0 or i == len(work):
                rate = i / max(time.time() - started, 1e-6)
                eta = (len(work) - i) / max(rate, 1e-6)
                cat = row.get("ig_category") or row.get("error") or "?"
                print(
                    f"  [{i}/{len(work)}] @{uname} → {cat}  "
                    f"{rate:.2f}/s eta={eta/60:.1f}m",
                    file=sys.stderr,
                )
    finally:
        out_fh.close()

    print(f"done. enriched {n} profiles → {out_path}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
