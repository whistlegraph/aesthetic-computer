#!/usr/bin/env python3
# bachbeat-seq.py — emit an AC_INJECT_SEQUENCE playing the opening of
# Bach's Cello Suite No. 1 Prelude (BWV 1007) on one grid + a simple
# 4/4 drum groove on the other.
#
# Source: artery/test-notepat-bach-prelude.mjs (MIDI_EVENTS array,
# transposed to notepat range). We take the first N events so the demo
# stays under ~20 s. Notes are held for ~90% of their duration so the
# phrasing comes through; gaps are ~10% so consecutive same-pitch
# repeats still register as separate hits.
#
# Drums: steady kick on beat 1, hat every eighth note, snare on beat 3.
# Simple rock/lofi beat that underlays the baroque line.

import re, sys
from pathlib import Path

# ---- Note-label → notepat keyboard key (matches fedac/native/pieces/notepat.mjs) ----
LABEL_TO_KEY = {
    "-a":  "control", "-a#": "z", "-b": "x",
    "c":   "c", "c#": "v", "d": "d", "d#": "s", "e": "e", "f": "f",
    "f#":  "w", "g":  "g", "g#": "r", "a": "a", "a#": "q", "b": "b",
    "+c":  "h", "+c#": "t", "+d": "i", "+d#": "y", "+e": "j", "+f": "k",
    "+f#": "u", "+g":  "l", "+g#": "o", "+a": "m", "+a#": "p", "+b": "n",
    "++c": ";", "++c#": "'", "++d": "]",
}

# Drum mapping for the RIGHT grid (matches waltz-seq.py).
DRUM_KEY = {"kick": "h", "snare": "i", "hat": "l", "hatOpen": "m"}

MIDI_TICKS_PER_BEAT = 480

def load_events(max_events: int):
    """Parse MIDI_EVENTS from the artery source — regex beats pulling in
    Node deps. Extracts {key, ticks} tuples in order."""
    # scripts/ → macos/ → native/ → fedac/ → REPO/ → artery/
    src = (Path(__file__).resolve().parent.parent.parent.parent.parent
           / "artery/test-notepat-bach-prelude.mjs").resolve()
    text = src.read_text()
    pat = re.compile(r'\{\s*key:\s*"([^"]+)"\s*,\s*ticks:\s*(\d+)')
    out = []
    for m in pat.finditer(text):
        label, ticks = m.group(1), int(m.group(2))
        key = LABEL_TO_KEY.get(label)
        if not key:
            continue
        out.append((key, ticks))
        if len(out) >= max_events: break
    return out

def build(bpm: int, events_count: int, start_offset_ms: int,
          melody_grid: str = "left") -> str:
    """Build the combined bach + drums sequence. melody_grid picks which
    grid holds the notes; the other grid plays drums."""
    if melody_grid not in ("left", "right"):
        raise SystemExit("melody_grid must be 'left' or 'right'")
    # Drum grid is the other one — load the right drum-key table.
    # For this script we only handle drums on the RIGHT (the typical
    # live-piano layout); if melody_grid=="right" we fall back to
    # LEFT-grid drum keys (c/d/g/a).
    drum_keys = DRUM_KEY if melody_grid == "left" else {
        "kick": "c", "snare": "d", "hat": "g", "hatOpen": "a",
    }

    ms_per_beat = 60000 / bpm
    ms_per_tick = ms_per_beat / MIDI_TICKS_PER_BEAT

    # ---- Melody notes ----
    events = load_events(events_count)
    melody_hits = []  # (abs_ms, key, hold_ms)
    t = 0
    for key, ticks in events:
        dur_ms = max(60, int(round(ticks * ms_per_tick)))
        hold   = max(40, int(dur_ms * 0.88))
        melody_hits.append((t, key, hold))
        t += dur_ms

    total_ms = t

    # ---- Drum beat (4/4) underneath ----
    # Steady eighth-note hat, kick on 1, snare on 3. Simple & spaced so
    # it doesn't mask the baroque line.
    drum_hits = []
    eighth_ms = ms_per_beat / 2
    beat_ms   = ms_per_beat
    n_beats   = int(total_ms / beat_ms) + 1
    for beat in range(n_beats):
        b_start = beat * beat_ms
        beat_in_bar = beat % 4
        # Kick on beat 1 of every bar.
        if beat_in_bar == 0:
            drum_hits.append((int(b_start), drum_keys["kick"], 0))
        # Snare on beat 3.
        if beat_in_bar == 2:
            drum_hits.append((int(b_start), drum_keys["snare"], 0))
        # Hat on every eighth (on + off).
        drum_hits.append((int(b_start),              drum_keys["hat"], 0))
        drum_hits.append((int(b_start + eighth_ms), drum_keys["hat"], 0))
        # Occasional open hat for flavor on the "and of 4".
        if beat_in_bar == 3:
            drum_hits.append((int(b_start + eighth_ms), drum_keys["hatOpen"], 0))

    # Merge melody + drums, sort by time (then key for stable order).
    all_hits = [(t, k, h) for (t, k, h) in melody_hits] \
             + [(t, k, h) for (t, k, h) in drum_hits if t < total_ms]
    all_hits.sort(key=lambda h: (h[0], h[1]))

    # Emit deltas from start_offset_ms anchor. Optional per-event hold.
    out, prev = [], -start_offset_ms
    for t, k, h in all_hits:
        delta = t - prev
        if delta < 0: delta = 0
        if h > 0: out.append(f"{k},{delta},{h}")
        else:     out.append(f"{k},{delta}")
        prev = t
    return "|".join(out)

def main():
    bpm    = int(sys.argv[1]) if len(sys.argv) > 1 else 100
    evts   = int(sys.argv[2]) if len(sys.argv) > 2 else 64
    start  = int(sys.argv[3]) if len(sys.argv) > 3 else 0
    grid   = sys.argv[4] if len(sys.argv) > 4 else "left"
    print(build(bpm, evts, start, grid))

if __name__ == "__main__":
    main()
