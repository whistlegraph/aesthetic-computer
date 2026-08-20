# takes.py — every utterance she has, across every take.
#
# @jeffrey: "process other takes of the whistlegraph / other lyrics /
# samples · that we can use for harmonizes and other lines / swap out
# utterances as needed or whjatnot".
#
# Swapping a word today means remembering which take had a better "pa"
# and hunting for it. This makes it a lookup: every aligned slice's words,
# each with the span that holds it, the pitch she actually sings there,
# and how far that sits off the A# minor grid in her own 237 Hz frame.
# Words are keyed by their lowercased text, so `stone` lists every stone
# in the bank side by side and a swap is a choice between measured
# options rather than a memory.
#
# Same measurements as audit.py — one instrument for the whole lane.
#
#   pop/.venv/bin/python pop/loner/bin/takes.py            # the index
#   pop/.venv/bin/python pop/loner/bin/takes.py stone      # one word

import json, os, sys
import numpy as np
import soundfile as sf

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
sys.path.insert(0, HERE)
from audit import columns, TONIC, FRAME_S

MINOR = [0, 2, 3, 5, 7, 8, 10]
NAMES = ["A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A"]


def note_name(st):
    """Her semitone, as a name in the take's own frame (A#3 = 0)."""
    k = int(round(st))
    return f"{NAMES[k % 12]}{3 + (k + 0) // 12}"


def off_grid(st):
    """Cents from the nearest scale tone — how far she is from the grid."""
    pc = (st * 100.0) % 1200.0
    return min(((pc - g * 100.0 + 600) % 1200) - 600 for g in MINOR + [12])


def main():
    want = sys.argv[1].lower() if len(sys.argv) > 1 else None
    align = json.load(open(os.path.join(LANE, "samples", ".align.json")))
    man = json.load(open(os.path.join(LANE, "samples", ".manifest.json")))
    index = {}

    for slice_name in sorted(align):
        wav = os.path.join(LANE, "samples", f"{slice_name}.wav")
        if not os.path.exists(wav):
            continue
        x, fs = sf.read(wav, dtype="float64")
        if x.ndim > 1:
            x = x.mean(axis=1)
        f0, st, rms, hi, peak, m = columns(x, fs)

        for w in align[slice_name]["words"]:
            a, b = w["start"], w["end"]
            k0, k1 = int(a / FRAME_S), min(m, int(b / FRAME_S))
            seg = st[k0:max(k0 + 1, k1)]
            seg = seg[~np.isnan(seg)]
            if not len(seg):
                continue
            med = float(np.median(seg))
            db = 20.0 * np.log10(max(float(np.max(rms[k0:max(k0 + 1, k1)])), 1e-9) / peak)
            index.setdefault(w["t"].lower().strip(".,!?"), []).append(dict(
                slice=slice_name, take=slice_name.split("-")[0],
                start=round(a, 3), end=round(b, 3), dur=round(b - a, 3),
                st=round(med, 2), note=note_name(med),
                cents=round(off_grid(med)), peak_db=round(db, 1)))

    path = os.path.join(LANE, "samples", ".takes.json")
    json.dump(index, open(path, "w"), indent=1, sort_keys=True)

    words = [want] if want else sorted(index)
    multi = sum(1 for k in index if len({e["take"] for e in index[k]}) > 1)
    if not want:
        print(f"{len(index)} distinct utterances across "
              f"{len({e['slice'] for v in index.values() for e in v})} takes · "
              f"{multi} exist in more than one take\n")
    for k in words:
        if k not in index:
            print(f"  {k}: not in the bank")
            continue
        rows = sorted(index[k], key=lambda e: (e["take"], e["slice"]))
        print(f"  {k}")
        for e in rows:
            print(f"      {e['take']}  {e['slice']:<22} {e['start']:6.2f}–{e['end']:6.2f}s "
                  f"({e['dur']:.2f}s)  {e['note']:>4} {e['st']:+6.2f}st  "
                  f"{e['cents']:+4d}¢  peak {e['peak_db']:.0f}dB")
    if not want:
        print(f"\nWROTE {path}")


if __name__ == "__main__":
    main()
