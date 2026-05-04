#!/usr/bin/env python3
"""
musicxml_to_np.py — convert a MusicXML lead sheet (melody + lyrics) to
AC's `.np` score format used by pitchsnap.mjs.

The .np format pairs one note with one syllable per token:
    NOTE:syllable*beats

  - NOTE — scientific pitch notation, e.g. D3, G#3 (flats normalized to
    enharmonic sharps so the AC parser doesn't have to know "Bb3")
  - syllable — lowercase. MusicXML <syllabic> markers map to AC dashes:
      single  → "grace"
      begin   → "a-"
      middle  → "-ma-"
      end     → "-zing"
  - beats — duration in beats (MusicXML <duration> / <divisions>).
    Rounded to int when whole, otherwise 2-decimal.

Tied notes are merged into one logical note (sum of durations, lyric
from the first). Chord tones (non-melody pitches) and rests are
skipped — this is a melody-extraction pass for the AC vocal lane, not
a full MusicXML round-trip.

Usage:
    python bin/musicxml_to_np.py input.musicxml output.np \\
        [--bpm 70] [--key "G major"] [--title "Amazing Grace"]

Designed to be run from sources like Hymnary, Mutopia (after
`lilypond --output=musicxml`), or any MuseScore export.
"""
import argparse
import sys
import xml.etree.ElementTree as ET
from pathlib import Path
from fractions import Fraction


def strip_ns(tag):
    """Strip XML namespace if present (MusicXML files in the wild are
    inconsistent about whether they declare a namespace)."""
    return tag.split("}", 1)[-1] if "}" in tag else tag


def find(elem, name):
    """Find a direct child by local name, ignoring namespaces."""
    if elem is None:
        return None
    for child in elem:
        if strip_ns(child.tag) == name:
            return child
    return None


def findall(elem, name):
    if elem is None:
        return []
    return [c for c in elem if strip_ns(c.tag) == name]


# Flats → enharmonic sharps (AC pitch parser sticks to sharps)
FLAT_TO_SHARP = {"D": "C#", "E": "D#", "G": "F#", "A": "G#", "B": "A#"}


def pitch_to_np(step, alter, octave):
    name = step.upper()
    if alter == 1:
        name += "#"
    elif alter == 2:
        # Double sharp — rare. Roll forward one whole step.
        roll = {"C": "D", "D": "E", "F": "G", "G": "A", "A": "B"}
        name = roll.get(name, name + "##")
    elif alter == -1:
        eq = FLAT_TO_SHARP.get(step.upper())
        if eq is not None:
            name = eq
        elif step.upper() == "C":
            # Cb → B (same pitch, octave - 1)
            return f"B{octave - 1}"
        elif step.upper() == "F":
            # Fb → E
            name = "E"
    elif alter == -2:
        # Double flat — also rare; roll back one whole step
        roll = {"E": "D", "B": "A", "A": "G", "G": "F", "D": "C"}
        name = roll.get(name, name + "bb")
    return f"{name}{octave}"


def syllabify(text, syllabic):
    text = (text or "").lower().strip()
    # Strip punctuation that would confuse the AC parser
    text = text.replace("_", "").replace(",", "").replace(".", "").replace("?", "").replace("!", "")
    if not text:
        return "_"
    if syllabic == "begin":
        return text + "-"
    if syllabic == "middle":
        return "-" + text + "-"
    if syllabic == "end":
        return "-" + text
    return text  # single (or default)


def beats_str(beats):
    if beats == int(beats):
        return str(int(beats))
    # 2-dec, but trim trailing zeros (1.50 → 1.5)
    s = f"{beats:.2f}".rstrip("0").rstrip(".")
    return s or "0"


class TiedAccumulator:
    """Collects duration across <tie type='start' | 'continue'> until we
    see <tie type='stop'>. The first note in the chain owns the lyric."""

    def __init__(self):
        self.active = False
        self.pitch = None
        self.duration = 0
        self.divisions = 1
        self.lyric = None
        self.syllabic = None

    def reset(self):
        self.__init__()


def extract_melody(part):
    """Walk one <part>, yielding (np_pitch, syllable, beats) tuples in
    order. Handles tied notes, chord tones, rests, key/voice changes."""
    tokens = []
    line_breaks = []  # measure index of each line break candidate

    divisions = 1
    tied = TiedAccumulator()
    main_voice = None

    measures = findall(part, "measure")
    for m_idx, measure in enumerate(measures):
        attrs = find(measure, "attributes")
        if attrs is not None:
            d = find(attrs, "divisions")
            if d is not None and d.text:
                divisions = int(d.text)

        for note in findall(measure, "note"):
            voice_el = find(note, "voice")
            voice = voice_el.text if voice_el is not None else "1"
            if main_voice is None:
                main_voice = voice
            if voice != main_voice:
                continue

            # Chord tone (non-first pitch in a chord) — skip; we only
            # transcribe the topmost melody.
            if find(note, "chord") is not None:
                continue

            duration_el = find(note, "duration")
            if duration_el is None or not duration_el.text:
                continue
            duration = int(duration_el.text)

            # Rest: flush any tied note, then skip
            if find(note, "rest") is not None:
                if tied.active:
                    tokens.append(_emit_tied(tied))
                    tied.reset()
                continue

            pitch = find(note, "pitch")
            if pitch is None:
                continue

            step = (find(pitch, "step").text or "C").upper()
            octave = int((find(pitch, "octave").text or "4"))
            alter_el = find(pitch, "alter")
            alter = int(alter_el.text) if alter_el is not None and alter_el.text else 0
            np_pitch = pitch_to_np(step, alter, octave)

            # Lyric (only from notes that *start* a syllable)
            lyric = None
            syllabic = "single"
            for ly in findall(note, "lyric"):
                txt = find(ly, "text")
                if txt is not None and txt.text:
                    lyric = txt.text
                    syl = find(ly, "syllabic")
                    syllabic = syl.text if syl is not None and syl.text else "single"
                    break

            # Tie handling
            ties = findall(note, "tie")
            tie_types = {t.attrib.get("type") for t in ties}

            if "start" in tie_types and "stop" not in tie_types:
                # Start of a tied chain
                if tied.active:
                    tokens.append(_emit_tied(tied))
                tied.active = True
                tied.pitch = np_pitch
                tied.duration = duration
                tied.divisions = divisions
                tied.lyric = lyric
                tied.syllabic = syllabic
            elif tie_types == {"start", "stop"} or tie_types == {"stop", "start"}:
                # Continuation in the middle of a chain
                if tied.active and tied.pitch == np_pitch:
                    tied.duration += duration
                else:
                    # Stray; treat as new
                    if tied.active:
                        tokens.append(_emit_tied(tied))
                    tied.reset()
                    tokens.append((np_pitch, syllabify(lyric, syllabic),
                                   duration / divisions))
            elif "stop" in tie_types:
                # End of a tied chain
                if tied.active and tied.pitch == np_pitch:
                    tied.duration += duration
                    tokens.append(_emit_tied(tied))
                else:
                    tokens.append((np_pitch, syllabify(lyric, syllabic),
                                   duration / divisions))
                tied.reset()
            else:
                # Plain note
                if tied.active:
                    tokens.append(_emit_tied(tied))
                    tied.reset()
                tokens.append((np_pitch, syllabify(lyric, syllabic),
                               duration / divisions))

        line_breaks.append(len(tokens))

    if tied.active:
        tokens.append(_emit_tied(tied))

    return tokens, line_breaks


def _emit_tied(t):
    return (t.pitch, syllabify(t.lyric, t.syllabic), t.duration / t.divisions)


def find_part(root):
    """Return the first <part> element. Score may have <score-partwise>
    or <score-timewise> at the root, with namespaces, etc."""
    for elem in root.iter():
        if strip_ns(elem.tag) == "part":
            return elem
    return None


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("input", help="MusicXML file")
    ap.add_argument("output", help="Output .np file")
    ap.add_argument("--bpm", type=int, help="Tempo in BPM (written as a comment)")
    ap.add_argument("--key", help="Key name (written as a comment)")
    ap.add_argument("--title", help="Title (written as a comment)")
    ap.add_argument("--verse", default="verse 1", help='Verse heading (default: "verse 1")')
    ap.add_argument("--line-every", type=int, default=8,
                    help="Wrap output to a new line every N notes (default: 8)")
    args = ap.parse_args()

    tree = ET.parse(args.input)
    root = tree.getroot()
    part = find_part(root)
    if part is None:
        print("✗ no <part> in MusicXML", file=sys.stderr)
        sys.exit(1)

    tokens, line_breaks = extract_melody(part)
    if not tokens:
        print("✗ no melody notes extracted", file=sys.stderr)
        sys.exit(1)

    # Build output
    lines = []
    if args.title:
        lines.append(f"# {args.title}")
    if args.key:
        lines.append(f"# key: {args.key}")
    if args.bpm:
        lines.append(f"# Use --beat-mode --bpm {args.bpm}.")
    if lines:
        lines.append("")  # blank separator

    lines.append(args.verse)

    # Wrap line every N notes (simple heuristic; user re-flows by hand)
    cur = []
    for i, (pitch, syl, beats) in enumerate(tokens):
        cur.append(f"{pitch}:{syl}*{beats_str(beats)}")
        if len(cur) >= args.line_every:
            lines.append(" ".join(cur))
            cur = []
    if cur:
        lines.append(" ".join(cur))

    Path(args.output).write_text("\n".join(lines) + "\n")
    print(f"✓ {args.output}")
    print(f"  {len(tokens)} notes · {len(line_breaks)} measures · "
          f"≈{sum(t[2] for t in tokens):.1f} beats")
    # Show first line preview
    first_line = next((l for l in lines if l and not l.startswith("#") and l != args.verse), "")
    if first_line:
        print(f"  first line: {first_line[:120]}{'…' if len(first_line) > 120 else ''}")


if __name__ == "__main__":
    main()
