#!/usr/bin/env python3
"""midi_to_np.py — extract a melody track from a MIDI file and write it
out as a `.np` notepat score that the big-pictures pipeline can sing.

Usage:
  midi_to_np.py input.mid output.np \
        [--track CLARINET] \
        [--track-index 14] \
        [--lyrics "its a world of laughter ..."] \
        [--start-beat 111] [--end-beat 130] \
        [--bpm 122]

You can either pass `--track NAME` (matched case-insensitive substring)
or `--track-index N`. If neither is set, the script prints a summary
of every track + range so you can pick by ear/eye, then exits.

The `--lyrics` flag is a flat list of words (whitespace separated). The
script aligns one syllable per MIDI note in the chosen window — so the
word/note counts have to line up. Use `--start-beat` / `--end-beat` to
trim the window down to just the chorus or verse you need.

Multi-syllable words use leading hyphens on continuation tokens
(notepat convention): "amazing" → "a- -ma- -zing".

If --lyrics isn't passed, the script writes "n01 n02 n03 …" placeholder
syllables so the score is still playable; you can edit by hand.
"""
import argparse
import sys
from pathlib import Path

import mido

NAMES = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"]
def midi_name(n): return f"{NAMES[n%12]}{n//12-1}"

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("midi"); ap.add_argument("out")
    ap.add_argument("--track", help="case-insensitive substring of track name")
    ap.add_argument("--track-index", type=int, help="numeric track index")
    ap.add_argument("--lyrics", help="flat whitespace-separated syllables; use - for continuation")
    ap.add_argument("--start-beat", type=float, default=0)
    ap.add_argument("--end-beat", type=float, default=1e9)
    ap.add_argument("--min-dur-beats", type=float, default=0.4,
                    help="ignore notes shorter than this (skips ornaments)")
    ap.add_argument("--bpm", type=int, default=None,
                    help="override tempo annotation in the .np header")
    args = ap.parse_args()

    m = mido.MidiFile(args.midi)
    tpb = m.ticks_per_beat

    # Detect tempo for the header.
    bpm = args.bpm
    if bpm is None:
        for tr in m.tracks:
            for msg in tr:
                if msg.type == "set_tempo":
                    bpm = round(60_000_000 / msg.tempo); break
            if bpm: break
        if not bpm: bpm = 120

    # Pick the track.
    tracks = list(m.tracks)
    chosen = None
    if args.track_index is not None:
        chosen = tracks[args.track_index]
    elif args.track:
        needle = args.track.lower()
        for tr in tracks:
            if needle in (tr.name or "").lower():
                chosen = tr; break
    if chosen is None:
        # No selection — print summary and exit.
        print(f"\n{Path(args.midi).name}  ·  {tpb} ticks/beat  ·  detected {bpm} bpm  ·  {len(tracks)} tracks\n")
        for i, tr in enumerate(tracks):
            notes = [msg.note for msg in tr if msg.type=='note_on' and msg.velocity>0]
            if not notes: continue
            lo = midi_name(min(notes)); hi = midi_name(max(notes))
            med = midi_name(sorted(notes)[len(notes)//2])
            print(f"  [{i:2d}] {(tr.name or '<unnamed>'):16s} {len(notes):4d} notes  range {lo}-{hi}  median {med}")
        print("\npass --track NAME or --track-index N to extract a melody")
        return 0

    # Extract melodic notes from the chosen track.
    t = 0
    notes = []
    active = {}
    for msg in chosen:
        t += msg.time
        if msg.type == "note_on" and msg.velocity > 0:
            active[msg.note] = t
        elif (msg.type == "note_off") or (msg.type == "note_on" and msg.velocity == 0):
            if msg.note in active:
                start = active.pop(msg.note)
                notes.append((start, msg.note, t - start))
    notes.sort()
    notes = [(s, n, d) for s, n, d in notes
             if d / tpb >= args.min_dur_beats
             and args.start_beat <= s/tpb < args.end_beat]
    if not notes:
        print(f"✗ no notes in window beat {args.start_beat}-{args.end_beat}", file=sys.stderr)
        return 1

    # Build syllables: lyric tokens (split on whitespace) get matched
    # 1:1 to extracted notes. If counts mismatch we just truncate to
    # min(len) and warn.
    syls = []
    if args.lyrics:
        toks = args.lyrics.split()
        if len(toks) != len(notes):
            print(f"⚠ {len(toks)} lyric tokens vs {len(notes)} notes — truncating to {min(len(toks), len(notes))}", file=sys.stderr)
        n = min(len(toks), len(notes))
        for i in range(n):
            s, midi, d = notes[i]
            syls.append((midi, toks[i], max(1, round(d / tpb))))
    else:
        for i, (s, midi, d) in enumerate(notes):
            syls.append((midi, f"n{i:02d}", max(1, round(d / tpb))))

    # Emit .np
    lines = []
    lines.append(f"# Extracted by midi_to_np.py from {Path(args.midi).name}")
    lines.append(f"# track: {chosen.name or '<unnamed>'}  ·  {len(syls)} syllables  ·  {bpm} bpm")
    if args.start_beat > 0 or args.end_beat < 1e9:
        lines.append(f"# window: beats {args.start_beat} to {args.end_beat}")
    lines.append("")
    lines.append("verse 1")
    line = []
    cum_beats = 0
    for midi, syl, beats in syls:
        line.append(f"{midi_name(midi)}:{syl}*{beats}")
        cum_beats += beats
        # Wrap at ~12 beats per .np line for readability
        if cum_beats >= 12:
            lines.append(" ".join(line)); line = []; cum_beats = 0
    if line: lines.append(" ".join(line))

    Path(args.out).write_text("\n".join(lines) + "\n")
    print(f"✓ {args.out}  ·  {len(syls)} notes  ·  {bpm} bpm")
    return 0

if __name__ == "__main__":
    sys.exit(main())
