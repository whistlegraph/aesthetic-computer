#!/usr/bin/env python3
# waltz-seq.py — emit an AC_INJECT_SEQUENCE string for a given waltz style.
#
# Patterns are lifted straight from artery/test-notepat.mjs (WALTZ_PATTERNS).
# Each 3/4 bar has 12 sixteenth-note steps (4 per beat × 3 beats).
#
# notepat has two grids (left + right) that can independently switch
# between "notes" and "perc" kit via PageUp / PageDown. This script emits
# drum hits for whichever grid is holding the kit:
#   --grid=left  → letters c/d/g/a  (keys: c/d/g/a)
#   --grid=right → letters +c/+d/+g/+a (keys: h/i/l/m)
#
# With --melody on, the script also interleaves a simple oom-pah-pah
# bass line on the OTHER grid (left-hand notes when grid=right, and
# vice versa), so drums and notes play simultaneously. The melody uses
# I–IV–V–I over the BARS count.
#
# Output format expected by main.c's AC_INJECT_SEQUENCE parser:
#   <key>,<delta_ms>|<key>,<delta_ms>|...
# deltas are cumulative from the prior event (not absolute).
#
# Usage:
#   waltz-seq.py [style [bpm [bars [start_offset_ms [grid [melody]]]]]]
# Defaults: classic 120 4 0 right off

import sys

WALTZ_PATTERNS = {
    "classic": {
        "kick":    [1,0,0,0, 0,0,0,0, 0,0,1,0],
        "snare":   [0,0,0,0, 1,0,0,0, 0,0,0,0],
        "hat":     [1,1,1,1, 1,1,1,1, 1,1,1,1],
        "hatOpen": [0,0,0,0, 0,0,0,1, 0,0,0,0],
    },
    "dark": {
        "kick":    [1,0,0,0, 0,0,1,0, 0,0,0,0],
        "snare":   [0,0,0,0, 1,0,0,0, 1,0,0,0],
        "hat":     [1,1,1,1, 1,1,1,1, 1,1,1,1],
        "hatOpen": [0,0,0,1, 0,0,0,1, 0,0,0,1],
    },
    "dreamy": {
        "kick":    [1,0,0,0, 0,0,0,0, 0,0,0,0],
        "snare":   [0,0,0,0, 1,0,0,0, 0,0,0,0],
        "hat":     [1,0,1,0, 1,0,1,0, 1,0,1,0],
        "hatOpen": [0,0,0,0, 0,0,0,0, 0,0,0,1],
    },
    "baroque": {
        "kick":    [1,0,0,1, 0,0,0,0, 1,0,0,0],
        "snare":   [0,0,0,0, 1,0,0,0, 0,0,1,0],
        "hat":     [1,1,1,1, 1,1,1,1, 1,1,1,1],
        "hatOpen": [0,0,0,0, 0,0,1,0, 0,0,0,1],
    },
    "minimal": {
        "kick":    [1,0,0,0, 0,0,0,0, 0,0,0,0],
        "snare":   [0,0,0,0, 0,0,0,0, 1,0,0,0],
        "hat":     [0,0,0,0, 1,0,0,0, 0,0,0,0],
    },
    "phonk": {
        "kick":    [1,0,0,0, 0,0,1,0, 0,1,0,0],
        "snare":   [0,0,0,0, 1,0,0,0, 0,0,0,1],
        "hat":     [1,0,1,1, 1,0,1,1, 1,0,1,1],
        "hatOpen": [0,0,0,0, 0,0,0,1, 0,0,0,1],
    },
    "viennese": {
        "kick":    [1,0,0,0, 0,0,0,0, 0,0,0,0],
        "snare":   [0,0,0,0, 1,0,1,0, 1,0,0,0],
        "hat":     [1,1,1,1, 1,1,1,1, 1,1,1,1],
    },
    "drill": {
        "kick":    [1,0,0,0, 0,0,0,0, 0,0,1,0],
        "snare":   [0,0,0,0, 0,0,0,0, 1,0,0,0],
        "hat":     [1,1,1,1, 1,1,1,1, 1,1,1,1],
        "hatOpen": [0,0,0,1, 0,0,0,1, 0,0,0,1],
    },
}

# Drum name → notepat perc-kit keyboard key, per grid.
# Left grid:  c(c4) d(d4) g(g4) a(a4)
# Right grid: h(+c5) i(+d5) l(+g5) m(+a5)  [via NOTE_TO_KEY in notepat.mjs]
DRUM_KEY_BY_GRID = {
    "left":  {"kick": "c", "snare": "d", "hat": "g", "hatOpen": "a"},
    "right": {"kick": "h", "snare": "i", "hat": "l", "hatOpen": "m"},
}

# Melody grooves — list of (key, step_index) pairs per bar. Step indices
# are 0..11 over the 12 sixteenth-note steps of a 3/4 bar.
#
# Left-hand grooves run on the LEFT grid in C minor pentatonic
# (c, d#, f, g, a#). Notepat left-grid keys:
#   c → c4     d#→ s     f → f     g → g     a#→ q
#   z → -a# (sub-octave bass)      x → -b (sub-octave bass alt)
#
# Four-bar phrase with varied patterns so it doesn't feel mechanical:
#   bar 0: bass anchor + ascending arp
#   bar 1: syncopated with call-response
#   bar 2: 8th-note run up the pentatonic
#   bar 3: bar 0 again (return home)
MELODY_BY_GRID = {
    "left": [
        # bar 0 — long bass + rising arp resolving on held 5th
        [("z", 0, 6), ("c", 4, 2), ("s", 5, 2), ("f", 6, 2),
         ("g", 8, 3), ("q", 9, 2), ("g", 10, 3)],
        # bar 1 — syncopated: bass + upper-neighbor, held 7th
        [("z", 0, 4), ("f", 2, 3), ("g", 3, 2), ("q", 4, 4),
         ("c", 6, 3), ("s", 8, 2), ("g", 10, 3)],
        # bar 2 — 8th-note run up the pentatonic
        [("z", 0, 3), ("c", 2, 2), ("s", 3, 2), ("f", 4, 2),
         ("g", 5, 2), ("q", 6, 2), ("c", 8, 3), ("s", 10, 3)],
        # bar 3 — return to bar 0 shape
        [("z", 0, 6), ("c", 4, 2), ("s", 5, 2), ("f", 6, 2),
         ("g", 8, 3), ("q", 9, 2), ("g", 10, 3)],
    ],
    "right": [
        [("h", 0, 6), ("h", 4, 2), ("y", 5, 2), ("k", 6, 2),
         ("l", 8, 3), ("p", 9, 2), ("l", 10, 3)],
        [("h", 0, 4), ("k", 2, 3), ("l", 3, 2), ("p", 4, 4),
         ("h", 6, 3), ("y", 8, 2), ("l", 10, 3)],
        [("h", 0, 3), ("h", 2, 2), ("y", 3, 2), ("k", 4, 2),
         ("l", 5, 2), ("p", 6, 2), ("h", 8, 3), ("y", 10, 3)],
        [("h", 0, 6), ("h", 4, 2), ("y", 5, 2), ("k", 6, 2),
         ("l", 8, 3), ("p", 9, 2), ("l", 10, 3)],
    ],
}

def build(style: str, bpm: int, bars: int, start_ms: int,
          grid: str = "right", melody: bool = False) -> str:
    pat = WALTZ_PATTERNS.get(style)
    if not pat:
        raise SystemExit(f"unknown style '{style}'. pick from: {', '.join(WALTZ_PATTERNS)}")
    if grid not in DRUM_KEY_BY_GRID:
        raise SystemExit(f"unknown grid '{grid}'. pick from: left, right")
    drum_keys = DRUM_KEY_BY_GRID[grid]
    # 3/4 has 3 beats per bar; 4 sixteenth-steps per beat → 12 steps/bar.
    step_ms = int(round(60000 / bpm / 4))
    steps_per_bar = 12

    # Collect every hit as (abs_ms, key, hold_ms). Drums default hold=0
    # (taps); melody notes carry hold durations so they sustain.
    hits = []
    for bar in range(bars):
        bar_start = bar * steps_per_bar * step_ms
        for step in range(steps_per_bar):
            t = bar_start + step * step_ms
            for drum, arr in pat.items():
                if arr[step]:
                    hits.append((t, drum_keys[drum], 0))

        # Optional minor-pentatonic groove on the OTHER grid. hold_steps
        # is in 16th-step units — multiplied by step_ms for absolute ms.
        if melody:
            melody_grid = "left" if grid == "right" else "right"
            groove = MELODY_BY_GRID[melody_grid]
            bar_groove = groove[bar % len(groove)]
            for key, step, hold_steps in bar_groove:
                hits.append((bar_start + step * step_ms, key,
                             hold_steps * step_ms))

    hits.sort(key=lambda h: (h[0], h[1]))

    # Emit with deltas. Events at same ms share delta=0. Notes with
    # hold > 0 emit three fields (key,delay,hold); drums stay two-field.
    out, prev = [], -start_ms
    for t, k, h in hits:
        delta = t - prev
        if delta < 0: delta = 0
        if h > 0: out.append(f"{k},{delta},{h}")
        else:     out.append(f"{k},{delta}")
        prev = t
    return "|".join(out)

def main():
    style  = sys.argv[1] if len(sys.argv) > 1 else "classic"
    bpm    = int(sys.argv[2]) if len(sys.argv) > 2 else 120
    bars   = int(sys.argv[3]) if len(sys.argv) > 3 else 4
    start  = int(sys.argv[4]) if len(sys.argv) > 4 else 0
    grid   = sys.argv[5] if len(sys.argv) > 5 else "right"
    melody = (len(sys.argv) > 6 and sys.argv[6].lower() in ("on", "1", "true", "yes"))
    print(build(style, bpm, bars, start, grid, melody))

if __name__ == "__main__":
    main()
