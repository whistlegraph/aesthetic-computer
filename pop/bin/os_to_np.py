#!/usr/bin/env python3
"""os_to_np.py — extract a melody from an Online Sequencer URL.

Online Sequencer encodes the song as a base64-protobuf blob inside the
HTML page (`var data = '...'`). This script:
  1. Fetches the HTML.
  2. Pulls the data blob.
  3. Parses the protobuf into per-instrument note lists.
  4. With no --instrument, prints a summary of all instruments.
  5. With --instrument N, emits a .np matching that voice.

Usage:
  os_to_np.py https://onlinesequencer.net/509647 out.np
  os_to_np.py https://onlinesequencer.net/509647 out.np --instrument 14
  os_to_np.py https://onlinesequencer.net/509647 out.np \
              --instrument 14 --start-time 8 --end-time 200 \
              --lyrics "its a small world af- -ter all ..."
"""
import argparse, base64, re, struct, sys, urllib.request
from collections import defaultdict
from pathlib import Path

NOTE = ['C','C#','D','D#','E','F','F#','G','G#','A','A#','B']
def note_name(p): return f"{NOTE[p%12]}{p//12-1}"

def read_varint(b, i):
    n = 0; shift = 0
    while True:
        if i >= len(b): return None, i
        v = b[i]; i += 1
        n |= (v & 0x7f) << shift
        if v < 0x80: return n, i
        shift += 7

def parse_msg(b, end):
    out = {}; i = 0
    while i < end:
        tag, i = read_varint(b, i)
        if tag is None: break
        wire = tag & 7; fid = tag >> 3
        if wire == 0:
            v, i = read_varint(b, i); out.setdefault(fid, []).append(v)
        elif wire == 5:
            if i + 4 > end: break
            v = struct.unpack('<f', b[i:i+4])[0]; i += 4
            out.setdefault(fid, []).append(v)
        elif wire == 2:
            length, i = read_varint(b, i)
            if i + length > end: break
            out.setdefault(fid, []).append(b[i:i+length]); i += length
        elif wire == 1:
            if i + 8 > end: break
            i += 8
        else: return out
    return out

def fetch_data(url):
    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    html = urllib.request.urlopen(req, timeout=30).read().decode("utf-8", errors="ignore")
    m = re.search(r"var data = '([^']+)'", html)
    if not m: raise RuntimeError("no `var data` blob found in page")
    blob = m.group(1)
    # Add padding for stray base64 if needed
    pad = len(blob) % 4
    if pad: blob += "=" * (4 - pad)
    return base64.b64decode(blob, validate=False)

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("url")
    ap.add_argument("out")
    ap.add_argument("--instrument", type=int, help="instrument id (run without to list)")
    ap.add_argument("--start-time", type=float, default=0,
                    help="OS time units (16th-notes) — skip earlier notes")
    ap.add_argument("--end-time", type=float, default=1e12)
    ap.add_argument("--lyrics", help="whitespace-separated syllables")
    ap.add_argument("--bpm", type=int, default=120)
    ap.add_argument("--units-per-beat", type=int, default=4,
                    help="OS time unit → beat divisor (default 4 = sixteenth-notes)")
    args = ap.parse_args()

    blob = fetch_data(args.url)
    top = parse_msg(blob, len(blob))
    by_instr = defaultdict(list)
    for n_raw in top.get(2, []):
        m = parse_msg(n_raw, len(n_raw))
        p = m.get(1, [None])[0]
        s = m.get(2, [0.0])[0]
        l = m.get(3, [None])[0]
        instr = m.get(4, [None])[0]
        if p is not None: by_instr[instr].append((s, p, l))

    if args.instrument is None:
        print(f"\n{args.url}\n{len(by_instr)} instruments\n")
        for instr in sorted(by_instr):
            notes = sorted(by_instr[instr])
            ps = [n[1] for n in notes]
            lo, hi = min(ps), max(ps)
            med = sorted(ps)[len(ps)//2]
            first = notes[0]
            print(f"  instr {instr:3d}: {len(notes):4d} notes  range {note_name(lo)}-{note_name(hi)}  median {note_name(med)}  first {note_name(first[1])}@t={first[0]:.0f}")
        print("\npass --instrument N to extract that voice")
        return 0

    notes = sorted(by_instr.get(args.instrument, []))
    notes = [n for n in notes if args.start_time <= n[0] < args.end_time]
    if not notes:
        print(f"✗ no notes for instrument {args.instrument}", file=sys.stderr); return 1

    # OS uses sixteenth-notes (units_per_beat=4). Convert each note's
    # length to .np beat weights by rounding to the nearest integer
    # ≥ 1. Inflate weights to absorb rests so the next syllable lands
    # at the right beat.
    upb = args.units_per_beat
    syls = []
    for i, (s, p, l) in enumerate(notes):
        # Effective length = max(note length, gap until next note start)
        if i + 1 < len(notes):
            next_s = notes[i + 1][0]
            eff = next_s - s
        else:
            eff = l or 0
        beats = max(1, round(eff / upb))
        syls.append((p, beats))

    toks = (args.lyrics.split() if args.lyrics else
            [f"n{i:02d}" for i in range(len(syls))])
    if args.lyrics and len(toks) != len(syls):
        print(f"⚠ {len(toks)} lyric tokens vs {len(syls)} notes — using min({min(len(toks), len(syls))})", file=sys.stderr)
    n = min(len(toks), len(syls))

    lines = []
    lines.append(f"# Extracted by os_to_np.py from {args.url}")
    lines.append(f"# instrument {args.instrument}  ·  {n} syllables  ·  {args.bpm} bpm")
    if args.start_time or args.end_time < 1e12:
        lines.append(f"# window: t={args.start_time}-{args.end_time} (16th-notes)")
    lines.append("")
    lines.append("verse 1")
    line, cum = [], 0
    for i in range(n):
        p, beats = syls[i]
        line.append(f"{note_name(p)}:{toks[i]}*{beats}")
        cum += beats
        if cum >= 12:
            lines.append(" ".join(line)); line, cum = [], 0
    if line: lines.append(" ".join(line))
    Path(args.out).write_text("\n".join(lines) + "\n")
    print(f"✓ {args.out}  ·  {n} syllables  ·  bpm {args.bpm}")
    return 0

if __name__ == "__main__":
    sys.exit(main())
