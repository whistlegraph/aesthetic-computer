# align.py — word alignment for the factory slices, from OpenAI whisper-1.
#
# The lane's receipts in harvest.json came from whisper.cpp ggml-small run
# with `-ml 1` (max one word per segment), and that flag does not return
# WORDS — it returns SUB-WORD TOKENS. The proof is sitting in the lane's
# own transcript: take a's line 3 is recorded as
#
#     ' Spin'@13.85  'ning'@14.45  ' away'@15.28
#
# "Spinning" split at its /n/. On the loner lane the same flag cut
# "curled" into cur+led and every label after it slid by a syllable; here
# it is one word early rather than one late, but the failure is the same
# and it is invisible unless you count. So every slice the chart plays is
# re-aligned through OpenAI whisper-1 with word timestamps.
#
#   OPENAI_API_KEY=... pop/.venv/bin/python pop/factory/bin/align.py
#     → samples/.align.json   (per slice, per word: start, end, f0, note)
#
# THE AUDIT, and why it is in this file. An alignment that is wrong by one
# unit still looks perfectly reasonable — every word has a plausible span,
# nothing overlaps, the JSON validates. The only thing that catches it is
# counting: segment the take into sung events (loud AND voiced runs, cut
# again wherever the de-spiked pitch takes a sustained step) and compare
# the total to the poem's syllables. This run prints both numbers and the
# per-word breakdown, so a slid label shows up as a word holding two
# events where its spelling says one.
#
# The key is read from the environment, or from the vault env file if that
# is present. It is never written to the receipt.

import json, os, subprocess, sys
import numpy as np
import soundfile as sf
import pyworld as pw

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import audit                # ONE event detector for the lane, not two
LANE = os.path.dirname(HERE)
REPO = os.path.dirname(os.path.dirname(LANE))
SAMPLES = os.path.join(LANE, "samples")
VAULT_ENV = os.path.join(REPO, "aesthetic-computer-vault",
                         ".devcontainer", "envs", "devcontainer.env")

# HER D, measured — not equal temperament, and not harvest's 147.0 either.
# See bin/chart.py's header for the derivation. Taken from audit.py so the
# whole lane reads one number.
TONIC = audit.TONIC
FLOOR = audit.FLOOR
FRAME_S = audit.FRAME_S

# the slices the chart plays — see CHART in chart.py
SLICES = [
    "chant-full", "line1", "line2", "line3",
    "line1-b", "line2-b", "line3-d", "line3-e",
    "spinning-away", "break-free",
]

CHROM = ["D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B", "C", "C#"]


def note_name(hz):
    st = int(round(12.0 * np.log2(hz / TONIC)))
    return f"{CHROM[st % 12]}{3 + (st + 2) // 12}"


def api_key():
    k = os.environ.get("OPENAI_API_KEY")
    if k:
        return k
    if os.path.exists(VAULT_ENV):
        for line in open(VAULT_ENV):
            if line.startswith("OPENAI_API_KEY"):
                return line.split("=", 1)[1].strip().strip("'\"")
    sys.exit("no OPENAI_API_KEY (env or vault)")


# The whole work is one known poem; priming whisper with it stops the
# short clips from being misheard. FACTORY'S poem, not another lane's.
POEM = ("factory cookie cutter personalities, "
        "we must break free from the states that we're in, "
        "spinning away, i hear a bird")


def harvest_slices():
    """basename → the lane's own receipt (words, span, measured f0)."""
    h = json.load(open(os.path.join(LANE, "harvest.json")))
    out = {}
    for take, t in h["takes"].items():
        for s in t.get("samples", []):
            name = os.path.basename(s["file"])[:-4]
            out.setdefault(name, dict(s, take=take))
    return out


def prompt_for(name, slices):
    """A slice's own words beat the whole poem: priming a one-line clip
    with all three lines invites whisper-1 to hallucinate the other two.
    The harvest receipt already records what each slice says."""
    w = (slices.get(name) or {}).get("words", "")
    if w and not any(k in w for k in ("whole poem", "held", "low", "(")):
        return w
    return POEM


def transcribe(path, key, prompt):
    """whisper-1 with word-level timestamps. curl keeps the key off the
    process list of anything but this call."""
    out = subprocess.run(
        ["curl", "-s", "https://api.openai.com/v1/audio/transcriptions",
         "-H", f"Authorization: Bearer {key}",
         "-F", f"file=@{path}",
         "-F", "model=whisper-1",
         "-F", f"prompt={prompt}",
         "-F", "response_format=verbose_json",
         "-F", "timestamp_granularities[]=word"],
        capture_output=True, text=True, check=True).stdout
    d = json.loads(out)
    if "error" in d:
        sys.exit(f"{os.path.basename(path)}: {d['error'].get('message')}")
    return d


def repair(words, x, fs):
    """whisper-1 occasionally emits a zero-width word. Rather than discard
    an otherwise perfect alignment, find that word's real onset: the first
    sustained-loud frame between its neighbours."""
    fixed = []
    peak = np.max(np.abs(x)) or 1.0
    n = int(round(fs * FRAME_S))
    for i, w in enumerate(words):
        w = dict(w)
        if w["end"] - w["start"] > 0.05:
            fixed.append(w)
            continue
        lo = fixed[-1]["end"] if fixed else 0.0
        hi = words[i + 1]["start"] if i + 1 < len(words) else len(x) / fs
        a, b = int(lo * fs / n), int(hi * fs / n)
        seg = x[a * n:b * n]
        m = len(seg) // n
        if m < 4 or hi <= lo:
            fixed.append(w)
            continue
        rms = np.sqrt((seg[:m * n].reshape(m, n) ** 2).mean(axis=1))
        loud = rms > peak * 10.0 ** (-36.0 / 20.0)
        run, onset = int(0.060 / FRAME_S), None
        for k in range(m - run):
            if loud[k:k + run].all():
                onset = k
                break
        w["start"] = round(lo + (onset or 0) * FRAME_S, 3)
        w["end"] = round(hi, 3)
        fixed.append(w)
        print(f"    repaired zero-width «{w['word']}» → {w['start']:.2f}–{w['end']:.2f}")
    return fixed


# despike() and events() live in bin/audit.py. They used to be copied here
# with looser thresholds, and the two copies drifted: this file's receipt
# claimed 40 events for the unbroken take while the audit that the build
# actually runs found 34. A receipt that disagrees with the build is worse
# than no receipt, so there is now one detector and both read it.
despike = audit.despike
events = lambda x, fs, f0d: audit.find_events(x, fs, f0d)


# syllables per word, for the audit. The poem is short enough to count by
# hand and long enough that counting by hand is the only way to be sure.
SYLLABLES = {
    "factory": 3, "cookie": 2, "cutter": 2, "personalities": 5,
    "we": 1, "must": 1, "break": 1, "free": 1, "from": 1, "the": 1,
    "states": 1, "that": 1, "we're": 1, "in": 1,
    "spinning": 2, "away": 2, "i": 1, "hear": 1, "a": 1, "bird": 1,
}


def measure(x, fs, words, f0):
    """Median f0 over each word's voiced frames — the note she sings."""
    out = []
    for w in words:
        a, b = int(w["start"] / FRAME_S), int(w["end"] / FRAME_S)
        seg = f0[max(0, a):max(1, min(b, len(f0)))]
        seg = seg[seg > 0]
        hz = float(np.median(seg)) if len(seg) else 0.0
        out.append(dict(t=w["word"], start=round(w["start"], 3),
                        end=round(w["end"], 3), f0_hz=round(hz, 1),
                        note=note_name(hz) if hz else ""))
    return out


def main():
    # --audit-only re-runs the counting against the SAVED receipt. Tuning
    # the detector should not cost ten transcriptions, and re-transcribing
    # would hand back slightly different times and quietly invalidate every
    # boundary the chart has pinned against these ones.
    audit_only = "--audit-only" in sys.argv
    dest = os.path.join(SAMPLES, ".align.json")
    if audit_only:
        align = json.load(open(dest))
        for name, rec in align.items():
            path = os.path.join(SAMPLES, f"{name}.wav")
            x, fs, f0d, folds = audit.analyze(path)
            ev = events(x, fs, f0d)
            want = sum(SYLLABLES.get(w["t"].strip().strip(",.!?").lower(), 1)
                       for w in rec["words"])
            rec["audit"] = dict(events=len(ev), syllables=want, octave_folds=folds)
            print(f"  {name:16s} events {len(ev):2d} · syllables {want:2d} · "
                  f"{'MATCH' if len(ev) == want else f'{len(ev)-want:+d}'}")
        json.dump(align, open(dest, "w"), indent=1)
        print(f"RE-AUDITED {dest} ({len(align)} slices, no API calls)")
        return
    key = api_key()
    slices = harvest_slices()
    align = {}
    for name in SLICES:
        path = os.path.join(SAMPLES, f"{name}.wav")
        if not os.path.exists(path):
            print(f"  {name:16s} — missing, skipped")
            continue
        d = transcribe(path, key, prompt_for(name, slices))
        words = d.get("words") or []
        x, fs = sf.read(path, dtype="float64")
        if x.ndim > 1:
            x = x.mean(axis=1)
        words = repair(words, x, fs)
        f0r, t = pw.harvest(x, fs, f0_floor=FLOOR, f0_ceil=600.0, frame_period=5.0)
        f0 = pw.stonemask(x, f0r, t, fs)
        f0d, folds = despike(f0)
        entry = measure(x, fs, words, f0d)
        # a usable alignment is strictly ordered with no zero-width word
        ok = all(b["start"] > a["start"] and b["start"] >= a["end"] - 0.001
                 for a, b in zip(entry[:-1], entry[1:])) \
            and all(w["end"] - w["start"] > 0.05 for w in entry) and len(entry) > 1
        if not ok:
            print(f"  {name:16s} — DEGENERATE alignment, left on the old receipt")
            continue

        # ── THE AUDIT ────────────────────────────────────────────────
        ev = events(x, fs, f0d)
        want = sum(SYLLABLES.get(w["t"].strip().strip(",.!?").lower(), 1)
                   for w in entry)
        print(f"  {name:16s} {len(entry):2d} words  «{d.get('text','').strip()[:50]}»")
        print(f"    events {len(ev):2d} · syllables {want:2d} · "
              f"{'MATCH' if len(ev) == want else f'OFF BY {len(ev)-want:+d}'}"
              f"  (octave folds {folds})")
        per = []
        for w in entry:
            a, b = w["start"], w["end"]
            k = sum(1 for (e0, e1) in ev
                    if a - 0.02 <= (e0 + e1) / 2 * FRAME_S < b + 0.02)
            s = SYLLABLES.get(w["t"].strip().strip(",.!?").lower(), 1)
            per.append(f"{w['t'].strip()}{'' if k == s else f'[{k}≠{s}]'}")
        print("    " + " · ".join(per))
        print("    " + " · ".join(f"{w['t'].strip()}@{w['start']:.2f}{w['note']}"
                                  for w in entry))
        align[name] = dict(model="whisper-1 (openai, word timestamps)",
                           text=d.get("text", "").strip(), words=entry,
                           audit=dict(events=len(ev), syllables=want,
                                      octave_folds=folds))
    json.dump(align, open(dest, "w"), indent=1)
    print(f"WROTE {dest} ({len(align)} slices)")


if __name__ == "__main__":
    main()
