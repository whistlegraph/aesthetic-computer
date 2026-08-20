# singdub.py — make a translated line SING her melody.
#
# @jeffrey: "i wanna hear the other langs being sung with the beat · in the
# mix".
#
# bin/dub.py returns the translation SPOKEN in her voice. Spoken is not a
# verse: it has its own prosody, its own length, and no relationship to the
# bar. This puts it on the chart — the same 60-beat melody, note for note,
# that halo3 warps her English take onto.
#
# The move is f0-replace ([[pop-world-autotune]]), not pitch correction:
# the spoken line's own contour is discarded and the CHART's semitone is
# written in its place, because the point is her tune, not the carrier's
# intonation. What survives from the recording is the part that carries the
# language — the formants, the consonants, the aperiodicity.
#
# MAPPING. Her line is 24 charted units; a translation has a different
# number of syllables in a different order, so a word-for-word mapping does
# not exist and pretending otherwise is how you get gibberish on the beat.
# Instead the spoken line is split into VOICED RUNS (its own syllable
# nuclei, found in the audio) and those runs are distributed across the
# chart's notes proportionally. A language with more syllables than she has
# notes puts several syllables inside one note — which is melisma's
# opposite and exactly what a translated verse does.
#
#   pop/.venv/bin/python pop/loner/bin/singdub.py fr es da ru hi
#   → vox-dub/sung-<lang>.wav, one phrase long, starting at beat 0

import json, os, subprocess, sys, tempfile
import numpy as np
import soundfile as sf
import pyworld as pw

HERE = os.path.dirname(os.path.abspath(__file__))
LANE = os.path.dirname(HERE)
sys.path.insert(0, HERE)
sys.path.insert(0, os.path.join(os.path.dirname(LANE), "lib"))
from nervox import waver as nervox_waver, flange as nervox_flange

TONIC = 237.0
BPM = 122.0
SPB = 60.0 / BPM
FRAME_MS = 5.0
FRAME_S = FRAME_MS / 1000.0
GATE_DB = -34.0
MIN_RUN_S = 0.070
GLIDE_S = 0.055          # how long f0 takes to arrive at a new note
VALLEY = 0.55            # an energy dip this deep is a syllable boundary

# THE STRETCH IS NOT UNIFORM. halo3 warps her English this way and singdub
# has to as well — @jeffrey: "we need better durations / syllable checking ·
# consonsnant / vowerl stretching · it feels broken". A vowel can be held
# for a whole bar and still sound like the vowel; a consonant held for a
# whole bar is a smear. So the slot's extra time is spent almost entirely
# on voiced frames, and consonants ride near their natural length.
W_VOWEL = 1.0
W_CONS = 0.18
HOLD_RATIO = 2.2         # past this, sustain the vowel instead of stretching


def ratio_cap(r):
    """How much a CONSONANT may be slowed even inside a held note."""
    return min(r, 1.6)


def load_mp3(path):
    """No mp3 decoder in soundfile — go through ffmpeg."""
    tmp = tempfile.mktemp(suffix=".wav")
    subprocess.run(["ffmpeg", "-y", "-v", "error", "-i", path,
                    "-ac", "1", "-ar", "44100", tmp], check=True)
    x, fs = sf.read(tmp, dtype="float64")
    os.unlink(tmp)
    return x, fs


def voiced_runs(x, fs, f0):
    """Syllable nuclei, straight from the audio.

    Splitting on unvoiced gaps ALONE is not enough and French proved it:
    "recroquevillée en moi-même" is continuously voiced, so the whole
    phrase came back as one run, got assigned to one note, and the words
    inside it were stretched into mush — @jeffrey heard it immediately.
    A voiced run is therefore cut again at its energy VALLEYS, which is
    where one syllable hands over to the next in a legato language.
    """
    n = int(round(fs * FRAME_S))
    m = min(len(f0), len(x) // n)
    rms = np.sqrt((x[:m * n].reshape(m, n) ** 2).mean(axis=1))
    gate = (np.max(np.abs(x)) or 1.0) * 10.0 ** (GATE_DB / 20.0)
    on = (rms > gate) & (f0[:m] > 0)

    gross, k = [], 0
    while k < m:
        if on[k]:
            j = k
            while j < m and on[j]:
                j += 1
            if (j - k) * FRAME_S >= MIN_RUN_S:
                gross.append((k, j))
            k = j
        else:
            k += 1

    # smooth the envelope, then cut at every dip that is a real valley:
    # deep enough against BOTH neighbouring peaks, and far enough from the
    # last cut to be a syllable rather than a wobble.
    w = max(1, int(round(0.030 / FRAME_S)))
    env = np.convolve(rms, np.ones(w) / w, mode="same")
    minsep = int(round(MIN_RUN_S / FRAME_S))
    runs = []
    for (a, b) in gross:
        cuts = [a]
        i = a + minsep
        while i < b - minsep:
            lo = env[i]
            left = env[cuts[-1]:i].max() if i > cuts[-1] else lo
            right = env[i:min(b, i + 2 * minsep)].max()
            if lo < VALLEY * min(left, right) and lo == env[i - minsep // 2:i + minsep // 2].min():
                cuts.append(i)
                i += minsep
            else:
                i += 1
        cuts.append(b)
        runs += [(u, v) for u, v in zip(cuts[:-1], cuts[1:]) if v - u >= minsep // 2]
    return runs, m


# THE LYRIC, in order. bin/takes.py indexed every utterance across nineteen
# takes and ten of them have all eighteen of these words on their own — so
# a whole line can be BUILT from a take rather than sliced out of one, with
# boundaries that are exact instead of transcribed.
LYRIC = ("sitting curled up in myself i think of a stone just waiting "
         "very patiently for time to pass").split()
CORPUS_GAP_S = 0.06      # a breath between words, so they do not run together


def corpus_line(take):
    """One take's whole line, assembled from its per-word corpus files.

    @jeffrey: "i guess we could try and map out other takes and see how
    they sound solo". This is the cheap way in. singdub's other sources —
    a scribed dub, an energy-detected spoken line — both GUESS where the
    words are; samples/corpus is already cut per word, so the spans it
    returns are measured, and any take that owns all eighteen words can be
    charted without a transcription step at all.
    """
    d = os.path.join(LANE, "samples", "corpus")
    if not os.path.isdir(d):
        return None
    idx = {}
    for f in os.listdir(d):
        if not f.endswith(".wav") or f.count("-") < 2:
            continue
        tk, _, w = f[:-4].split("-", 2)
        if tk == take:
            idx[w] = os.path.join(d, f)
    if any(w not in idx for w in LYRIC):
        return None
    parts, spans, fs, n = [], [], None, 0
    for w in LYRIC:
        y, fs = sf.read(idx[w], dtype="float64")
        if y.ndim > 1:
            y = y.mean(axis=1)
        spans.append((n / fs, (n + len(y)) / fs))
        parts.append(y)
        n += len(y)
        gap = np.zeros(int(CORPUS_GAP_S * fs))
        parts.append(gap)
        n += len(gap)
    return np.concatenate(parts), fs, spans, list(LYRIC)


def main():
    langs = sys.argv[1:] or ["fr"]
    chart = json.load(open(os.path.join(LANE, "vox4", ".chart.json")))["w-whole-line"]
    notes = chart["notes"]
    total_beats = chart["beats"]

    for lang in langs:
        # `lang` may name a TAKE instead of a translation. @jeffrey: "lets
        # work on swap lead and also use / bring in group takes · but the
        # idea is we start small with camille's softest take then we build
        # up each one". Putting another take on the chart is the same job
        # as putting another language on it — different words in, her
        # melody out — so s-whole-line and the ensemble o-whole-line go
        # through this path rather than needing their own hand-pinned
        # chart, which is a session's work each.
        built = corpus_line(lang)
        take = os.path.join(LANE, "samples", f"{lang}.wav")
        src = os.path.join(LANE, "vox-dub", f"sts-{lang}.mp3")
        if built:
            x, fs, corpus_spans, corpus_labels = built
        else:
            corpus_spans = None
            if os.path.exists(take):
                src = take
            elif not os.path.exists(src):
                src = os.path.join(LANE, "vox-dub", f"spoken-{lang}.mp3")
            if not os.path.exists(src):
                print(f"  {lang}: no corpus take, no samples/{lang}.wav, "
                      f"nothing in vox-dub/")
                continue
            if src.endswith(".wav"):
                x, fs = sf.read(src, dtype="float64")
                if x.ndim > 1:
                    x = x.mean(axis=1)
            else:
                x, fs = load_mp3(src)

        f0r, t = pw.harvest(x, fs, f0_floor=80.0, f0_ceil=600.0, frame_period=FRAME_MS)
        f0 = pw.stonemask(x, f0r, t, fs)
        fft = pw.get_cheaptrick_fft_size(fs, f0_floor=80.0)
        sp = pw.cheaptrick(x, f0, t, fs, fft_size=fft, f0_floor=80.0)
        ap = pw.d4c(x, f0, t, fs, fft_size=fft)
        # PREFER THE WORDS. @jeffrey: "per language we should be able to map
        # the shape of the words etc". dub.py scribe returns this language's
        # real word spans, so the verse is charted word by word the way the
        # English one is; the energy detector below is only the fallback for
        # a language that has not been scribed yet.
        wpath = os.path.join(LANE, "vox-dub", ".words.json")
        words = json.load(open(wpath)).get(lang) if os.path.exists(wpath) else None
        if corpus_spans:
            runs = [(int(a_ / FRAME_S), int(b_ / FRAME_S))
                    for (a_, b_) in corpus_spans]
            labels = corpus_labels
            src_kind = "corpus words"
        elif words:
            runs = [(int(w["start"] / FRAME_S), int(w["end"] / FRAME_S))
                    for w in words]
            runs = [(a_, b_) for (a_, b_) in runs if b_ > a_]
            labels = [w["t"] for w in words]
            src_kind = "words"
        else:
            runs, _m = voiced_runs(x, fs, f0)
            labels = [f"~{i}" for i in range(len(runs))]
            src_kind = "syllables"
        if not runs:
            print(f"  {lang}: nothing to map"); continue

        # the output timeline: one phrase, on the grid
        out_frames = int(round(total_beats * SPB / FRAME_S))
        idx = np.zeros(out_frames, dtype=int)
        st_out = np.full(out_frames, np.nan)
        ratios = []          # so a bad duration is visible, not just audible

        # distribute this language's syllables across her notes
        # EVERY NOTE GETS A WORD. Mapping word→note leaves notes empty
        # whenever a language has fewer words than she has notes — French
        # left seven silent, Russian nine, and a melody with holes in it is
        # not her melody. Mapping note→word instead guarantees coverage: a
        # language with fewer words simply holds one across several notes,
        # which is what a singer does with a long line and few syllables.
        R = len(runs)
        buckets = [[] for _ in notes]
        nwords = max(nt.get("w", k) for k, nt in enumerate(notes)) + 1
        if R == nwords:
            # THE SAME LYRIC, A DIFFERENT TAKE. This chart plays 24 units
            # but the lyric is 18 words — "patiently" is three units,
            # "sitting" and "myself" and "waiting" and "very" are two
            # each. The index spread below is for TRANSLATIONS, where the
            # word counts genuinely differ and holding one word across
            # several notes is what a singer would do. Run it on another
            # take of the same words and `k * 18 // 24` hands words 0, 3,
            # 6, 9, 12 and 15 to two adjacent notes apiece — @jeffrey:
            # "the second run of samples around 1:10 is all fucked up".
            # It was: six words sung twice, then flattened by THE HOLD.
            #
            # When the counts match, the mapping is known exactly. Each
            # word goes to ITS units, and a word spanning several units is
            # CUT between them rather than repeated — proportionally by
            # note length, then nudged onto the nearest unvoiced frame,
            # because the seam inside "pa|tient|ly" is the /t/.
            by_word = {}
            for k, nt in enumerate(notes):
                by_word.setdefault(nt.get("w", k), []).append(k)
            for w, ks in sorted(by_word.items()):
                u, v = runs[w]
                durs = [notes[k]["dur"] for k in ks]
                tot = sum(durs) or 1.0
                edges, acc = [u], 0.0
                for d in durs[:-1]:
                    acc += d
                    e = u + int(round((v - u) * acc / tot))
                    # snap to a consonant: search ±12% of the word for a
                    # frame she is not voicing
                    win = max(2, int(0.12 * (v - u)))
                    cand = [f for f in range(max(u + 1, e - win),
                                             min(v - 1, e + win))
                            if f < len(f0) and f0[f] <= 0]
                    if cand:
                        e = min(cand, key=lambda f: abs(f - e))
                    edges.append(max(edges[-1] + 1, e))
                edges.append(max(edges[-1] + 1, v))
                for j, k in enumerate(ks):
                    buckets[k] = [(edges[j], edges[j + 1])]
            print(f"  {lang}: same lyric — {R} words → {len(notes)} units, "
                  f"{len(notes) - R} syllable cuts")
        else:
            for k in range(len(notes)):
                buckets[k].append(runs[min(R - 1, k * R // len(notes))])

        for k, nt in enumerate(notes):
            a = int(round(nt["beat"] * SPB / FRAME_S))
            b = int(round((nt["beat"] + nt["dur"]) * SPB / FRAME_S))
            a, b = max(0, a), min(out_frames, b)
            if b <= a:
                continue
            group = buckets[k]
            if not group:                       # nobody sings here — hold the
                idx[a:b] = idx[a - 1] if a else 0     # previous frame, silently
                continue
            # this bucket's source frames…
            srcf = np.concatenate([np.arange(u, v) for (u, v) in group])
            # …warped through a WEIGHTED clock, so the slot's extra time is
            # spent on vowels and the consonants keep their own length.
            wts = np.where(f0[srcf] > 0, W_VOWEL, W_CONS)
            cum = np.concatenate([[0.0], np.cumsum(wts)])
            cum /= cum[-1]
            pos = np.interp((np.arange(b - a) + 0.5) / (b - a),
                            cum, np.arange(len(cum), dtype=float)) - 0.5

            # THE HOLD. A spoken syllable is ~0.15 s and one of her notes is
            # up to 2 s, so the honest stretch is 10–30x — and marching
            # through a syllable twenty times too slowly is a slur, not a
            # held note. Past HOLD_RATIO the syllable stops being stretched
            # and its VOWEL is sustained instead: the frame index dwells
            # around the vowel's centre, drifting slowly so the spectrum
            # keeps evolving rather than freezing into a buzz. Consonants
            # are untouched — they still play once, at their own speed.
            ratio = (b - a) / max(1, len(srcf))
            if ratio > HOLD_RATIO:
                vi = np.where(f0[srcf] > 0)[0]
                if len(vi) >= 3:
                    c0, c1 = vi[0], vi[-1]
                    head = int(c0 / max(ratio_cap(ratio), 1e-6))
                    tail = int((len(srcf) - 1 - c1) / max(ratio_cap(ratio), 1e-6))
                    n_out = b - a
                    head = min(head, n_out // 4)
                    tail = min(tail, n_out // 4)
                    body = max(1, n_out - head - tail)
                    mid, span = 0.5 * (c0 + c1), max(1.0, (c1 - c0) * 0.5)
                    # a slow triangle through the vowel — never the same
                    # frame twice in a row, never outside the vowel
                    ph = np.linspace(0, body / (0.55 / FRAME_S), body)
                    drift = mid + span * (2.0 / np.pi) * np.arcsin(np.sin(ph))
                    pos = np.concatenate([
                        np.linspace(0, c0, head, endpoint=False) if head else np.zeros(0),
                        drift,
                        np.linspace(c1, len(srcf) - 1, tail) if tail else np.zeros(0),
                    ])[:n_out]
                    if len(pos) < n_out:
                        pos = np.pad(pos, (0, n_out - len(pos)), mode="edge")
            idx[a:b] = srcf[np.clip(np.round(pos).astype(int), 0, len(srcf) - 1)]
            st_out[a:b] = nt["st"]
            ratios.append((nt["t"], len(group), len(srcf) * FRAME_S,
                           (b - a) * FRAME_S))

        # HER MELODY, written over the carrier's intonation. Glide into each
        # new note instead of stepping — a step reads as a splice.
        st_f = st_out.copy()
        last = st_f[~np.isnan(st_f)][0] if np.any(~np.isnan(st_f)) else 0.0
        g = max(1, int(GLIDE_S / FRAME_S))
        for i in range(out_frames):
            if np.isnan(st_f[i]):
                st_f[i] = last
            else:
                last = st_f[i]
        sm = np.convolve(st_f, np.ones(g) / g, mode="same")
        f0_new = TONIC * 2.0 ** (sm / 12.0)
        voiced_o = f0[idx] > 0
        f0_new = np.where(voiced_o, f0_new, 0.0)
        f0_new = nervox_waver(f0_new, FRAME_S, voiced=voiced_o)

        y = pw.synthesize(f0_new, sp[idx].copy(order="C"), ap[idx].copy(order="C"),
                          fs, frame_period=FRAME_MS)
        y = nervox_flange(y, fs)
        pk = np.max(np.abs(y)) or 1.0
        y = y / pk * 0.85

        dest = os.path.join(LANE, "vox-dub", f"sung-{lang}.wav")
        sf.write(dest, y, fs)
        # SYLLABLE CHECKING. A note holding four syllables, or one stretched
        # 4x, is where a translated verse turns to mush — print it rather
        # than wait to hear it.
        worst = sorted(ratios, key=lambda r: -(r[3] / max(r[2], 1e-6)))[:3]
        empty = len(notes) - len(ratios)
        crowd = max((r[1] for r in ratios), default=0)
        print(f"  ✓ {lang}: {dest}  {len(y) / fs:.1f}s · {R} syllables over "
              f"{len(notes)} notes · {empty} notes empty · "
              f"up to {crowd} syllables in one note")
        for t, ns, src_s, slot_s in worst:
            print(f"      {t:>8}  {ns} syll  {src_s:.2f}s → {slot_s:.2f}s  "
                  f"{slot_s / max(src_s, 1e-6):.2f}x")


if __name__ == "__main__":
    main()
