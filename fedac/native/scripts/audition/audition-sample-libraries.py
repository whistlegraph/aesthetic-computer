#!/usr/bin/env python3
"""audition-sample-libraries.py — hear open-licensed sample libraries the way
AC would play them.

Each candidate is rendered twice:

  note.mp3    one recorded sample near the phrase's middle, untouched
              (recording quality, room, noise floor)
  phrase.mp3  a short arpeggio + held chord played the way audio.c and
              MenuBandACPianoVoice.swift play the `1 grand piano: nearest
              anchor by MIDI distance, step 2^(semitones/12), linear
              interpolation, a 70 ms damper on key-up (or let ring for
              mallets, harps and bells)

Only the anchors the phrase needs are downloaded, into a cache. Writes
audio/, manifest.json and index.html (report.html with the manifest inlined). Candidates and their
licenses live in fedac/native/samples/CANDIDATES.md.

  python3 fedac/native/scripts/audition/audition-sample-libraries.py \
      --out ~/Desktop/sampled-instruments

Requires: numpy, ffmpeg, gh (GitHub trees), cc (SF2 renderer), bsdtar.
"""

import argparse, json, math, os, re, subprocess, sys, urllib.parse, urllib.request
from pathlib import Path

import numpy as np

RATE = 48_000
HERE = Path(__file__).resolve().parent
REPO = HERE.parents[3]
CACHE = Path(os.environ.get("AUDITION_CACHE", Path.home() / ".cache/ac-sample-audition"))

# The audition phrase, in semitones above the instrument's root: an
# arpeggio up and back, then a held triad. (onset s, duration s, offset)
PHRASE = [(0.00, 0.30, 0), (0.32, 0.30, 4), (0.64, 0.30, 7), (0.96, 0.30, 12),
          (1.28, 0.30, 7), (1.60, 0.30, 4),
          (2.00, 2.20, 0), (2.00, 2.20, 4), (2.00, 2.20, 7)]

# ── Candidates ────────────────────────────────────────────────────────
# repo: GitHub "owner/name" (tree read via gh) · iowa: MIS page name ·
# local: AC's own bank · sf2: (url, archive member or None, preset index)
# pick: regex a file path must match · root: MIDI note the phrase starts on
# ring: let notes ring past key-up instead of damping them
# shift: semitones to add to file-name pitches. Much of VCSL and VSCO 2 CE
#   names middle C "C3", so those samples sound an octave above their
#   names (checked with YIN and spectral peaks against the rendered notes)

CANDIDATES = [
  dict(id="salamander", name="Salamander Grand (our `1)", family="Keys", local="fedac/native/samples/piano",
       license="CC0", root=60, note="Reference: the bank AC OS and Menu Band play today."),

  dict(id="vcsl-steinway", name="Steinway B grand", family="Keys", repo="sgossner/VCSL",
       pick=r"Grand Piano, Steinway B/.*JHPiano_NoSus_Close_.*_vl3_rr1\.wav$", license="CC0", root=60),
  dict(id="vcsl-kawai", name="Kawai grand", family="Keys", repo="sgossner/VCSL",
       pick=r"Grand Piano, Kawai/.*GPiano_sus_.*_v3_rr1_Player\.wav$", license="CC0", root=60, shift=12),
  dict(id="vcsl-yamaha-upright", name="Yamaha upright", family="Keys", repo="sgossner/VCSL",
       pick=r"Upright Piano, Yamaha/.*Upright1_Sus_.*_vl2_rr1\.wav$", license="CC0", root=60, shift=12),
  dict(id="freepats-kw-upright", name="Kawai upright (living room)", family="Keys", repo="freepats/upright-piano-KW",
       pick=r"^samples/[A-G]#?\dvL\.flac$", license="CC0", root=60),
  dict(id="vcsl-harpsichord", name="Flemish harpsichord", family="Keys", repo="sgossner/VCSL",
       pick=r"Harpsichord, Flemish/.*HarpsiRH_Low_Far_.*_rr1\.wav$", license="CC0", root=60, shift=12),
  dict(id="vcsl-organ", name="Pipe organ, open manual", family="Keys", repo="sgossner/VCSL",
       pick=r"Pipe Organ/.*Rode_Man3Open_.*\.wav$", license="CC0", root=60),

  dict(id="vcsl-vibraphone", name="Vibraphone", family="Mallets", repo="sgossner/VCSL",
       pick=r"Vibraphone/.*Vibes_soft_.*_v2_rr1_Main\.wav$", license="CC0", root=60, ring=True, shift=12),
  dict(id="vcsl-marimba", name="Marimba", family="Mallets", repo="sgossner/VCSL",
       pick=r"Marimba/.*Marimba_hit_Outrigger_.*_med_01\.wav$", license="CC0", root=60, ring=True, shift=12),
  dict(id="vcsl-xylophone", name="Xylophone", family="Mallets", repo="sgossner/VCSL",
       pick=r"Xylophone/.*Xylo_Medium_.*_pp_01_far\.wav$", license="CC0", root=72, ring=True, shift=12),
  dict(id="vcsl-glockenspiel", name="Glockenspiel", family="Mallets", repo="sgossner/VCSL",
       pick=r"Glockenspiel/.*glock_loud_.*_01\.wav$", license="CC0", root=84, ring=True, shift=12),
  dict(id="vcsl-chimes", name="Tubular bells", family="Mallets", repo="sgossner/VCSL",
       pick=r"Tubular Bells 1/.*chimes_.*_(ff|p)_rr1\.wav$", license="CC0", root=62, ring=True, shift=12),
  dict(id="celesta", name="Mustel celesta", family="Mallets",
       sf2=("https://gitlab.com/echoparallax/a-sampled-celesta/-/raw/main/Samples/Celesta%20Tuned%20Denoised%20Mix.sf2", None, 0),
       license="CC0 (samples), MIT (tools)", root=72, ring=True),
  dict(id="vcsl-balafon", name="Balafon", family="Mallets", repo="sgossner/VCSL",
       pick=r"Balafon/.*EthnicXylo_hardM_.*_vl2_rr1_Mid\.wav$", license="CC0", root=62, ring=True, shift=12),

  dict(id="vcsl-harp", name="Concert harp", family="Plucked", repo="sgossner/VCSL",
       pick=r"Concert Harp/.*KSHarp_.*_mf1\.wav$", license="CC0", root=60, ring=True),
  dict(id="vcsl-kalimba", name="Kalimba (Kenya)", family="Plucked", repo="sgossner/VCSL",
       pick=r"Kalimba, Kenya/.*_vl3_rr2\.wav$", license="CC0", root=60, ring=True, shift=12),
  dict(id="freepats-guitar", name="Spanish classical guitar", family="Plucked",
       sf2=("https://freepats.zenvoid.org/Guitar/SpanishClassicalGuitar/SpanishClassicalGuitar-SF2-20190618.7z", ".sf2", 0),
       license="CC0", root=48, ring=True),
  dict(id="karoryfer-meatbass", name="Double bass, pizzicato (Meatbass)", family="Plucked", repo="sfzinstruments/karoryfer.meatbass",
       pick=r"Samples/pizz/.*_vl2_rr1\.wav$", license="CC0", root=36, ring=True),

  dict(id="vsco-violin", name="Solo violin, arco vibrato", family="Bowed & blown", repo="sgossner/VSCO-2-CE",
       pick=r"Solo Violin/Arco Vib/LLVln_ArcoVib_.*_p\.wav$", license="CC0", root=67, sustain=True),
  dict(id="iowa-cello", name="Cello, arco (anechoic)", family="Bowed & blown", iowa="MISCello2012",
       pick=r"Cello\.arco\.ff\.sul[DG]\.", license="Iowa MIS (free, no stated license)", root=48, sustain=True),
  dict(id="vsco-flute", name="Flute, vibrato", family="Bowed & blown", repo="sgossner/VSCO-2-CE",
       pick=r"Flute/susvib/LDFlute_susvib_.*_v1_1\.wav$", license="CC0", root=72, sustain=True, shift=12),
  dict(id="iowa-flute", name="Flute, vibrato (anechoic)", family="Bowed & blown", iowa="MISFlute2012",
       pick=r"Flute\.vib\.ff\.", license="Iowa MIS (free, no stated license)", root=72, sustain=True),
  dict(id="vsco-clarinet", name="Clarinet", family="Bowed & blown", repo="sgossner/VSCO-2-CE",
       pick=r"Clarinet/susLong/DCClar_susLong_.*_v2_rr1_sum\.wav$", license="CC0", root=60, sustain=True, shift=12),
  dict(id="vcsl-tenor-sax", name="Tenor sax", family="Bowed & blown", repo="sgossner/VCSL",
       pick=r"Tenor Saxophone/.*BrettTenor_NV_Main_.*_vl2_rr1\.wav$", license="CC0", root=48, sustain=True, shift=12),
  dict(id="vcsl-ocarina", name="Ocarina", family="Bowed & blown", repo="sgossner/VCSL",
       pick=r"Ocarina, Typical/.*StdOcarina_Sus_.*_rel\.wav$", license="CC0", root=72, sustain=True, shift=12),
  dict(id="vcsl-glasses", name="Wine glasses", family="Bowed & blown", repo="sgossner/VCSL",
       pick=r"Wine Glasses/.*_Slow_.*_Main\.wav$", license="CC0", root=75, sustain=True, shift=12),
]

# ── Pitch from file names ─────────────────────────────────────────────

NOTE_TOKEN = re.compile(r"^([A-Ga-g])(#|b)?(-?\d)(v[HL])?$")
SEMITONE = dict(c=0, d=2, e=4, f=5, g=7, a=9, b=11)

def midi_of(path):
    """MIDI pitch from the first note-name token in a file name (C4 = 60)."""
    stem = path.rsplit("/", 1)[-1].rsplit(".", 1)[0]
    for token in re.split(r"[_ .\-]", stem):
        m = NOTE_TOKEN.match(token)
        if m:
            letter, acc, octave = m.group(1).lower(), m.group(2), int(m.group(3))
            return 12 * (octave + 1) + SEMITONE[letter] + (1 if acc == "#" else -1 if acc == "b" else 0)
    return None

# ── Sources ───────────────────────────────────────────────────────────

def fetch(url, dest):
    dest.parent.mkdir(parents=True, exist_ok=True)
    if not dest.exists():
        print(f"    fetch {url.rsplit('/', 1)[-1][:60]}", file=sys.stderr)
        req = urllib.request.Request(url, headers={"User-Agent": "aesthetic-computer-audition"})
        with urllib.request.urlopen(req, timeout=120) as r, open(dest.with_suffix(".part"), "wb") as f:
            f.write(r.read())
        dest.with_suffix(".part").rename(dest)
    return dest

def github_tree(repo):
    cached = CACHE / "trees" / (repo.replace("/", "_") + ".json")
    if not cached.exists():
        branch = subprocess.check_output(["gh", "api", f"repos/{repo}", "--jq", ".default_branch"], text=True).strip()
        tree = json.loads(subprocess.check_output(["gh", "api", f"repos/{repo}/git/trees/{branch}?recursive=1"]))
        cached.parent.mkdir(parents=True, exist_ok=True)
        cached.write_text(json.dumps({"branch": branch, "tree": tree["tree"]}))
    return json.loads(cached.read_text())

def anchors_for(c):
    """{midi: url-or-path} for every file in the candidate's chosen layer."""
    found = {}
    if "local" in c:
        for f in sorted((REPO / c["local"]).glob("*.raw")):
            found[int(f.stem)] = str(f)
    elif "repo" in c:
        t = github_tree(c["repo"])
        for node in t["tree"]:
            p = node["path"]
            if node["type"] == "blob" and re.search(c["pick"], p):
                m = midi_of(p)
                if m is not None and m + c.get("shift", 0) not in found:
                    found[m + c.get("shift", 0)] = f"https://raw.githubusercontent.com/{c['repo']}/{t['branch']}/" + urllib.parse.quote(p)
    elif "iowa" in c:
        page = fetch(f"https://theremin.music.uiowa.edu/MIS-Pitches-2012/{c['iowa']}.html", CACHE / "iowa" / f"{c['iowa']}.html")
        for href in re.findall(r'href="([^"]+\.aif)"', page.read_text(errors="ignore")):
            if re.search(c["pick"], href):
                m = midi_of(href)
                if m is not None and m not in found:
                    found[m] = urllib.parse.urljoin("https://theremin.music.uiowa.edu/MIS-Pitches-2012/", urllib.parse.quote(href))
    return dict(sorted(found.items()))

def decode(src, cid):
    """Any audio file (path or URL) → float32 mono 48 kHz, cached."""
    if src.endswith(".raw"):
        return np.fromfile(src, dtype="<f4")
    name = urllib.parse.unquote(src.rsplit("/", 1)[-1])
    raw = fetch(src, CACHE / "src" / cid / name)
    out = raw.with_suffix(raw.suffix + ".f32")
    if not out.exists():
        subprocess.run(["ffmpeg", "-v", "error", "-y", "-i", str(raw), "-ac", "1", "-ar", str(RATE),
                        "-f", "f32le", str(out)], check=True)
    x = np.fromfile(out, dtype="<f4")
    # Some sets (Iowa MIS especially) leave room tone before the attack; start
    # 5 ms ahead of the first sample above -50 dB so onsets land on time.
    start = int(np.argmax(np.abs(x) > 10 ** (-50 / 20)))
    return x[max(0, start - RATE // 200):]

# ── Rendering (the audio.c way) ───────────────────────────────────────

RELEASE = math.exp(-1.0 / (RATE * 0.07))

def nearest(anchors, midi):
    return min(anchors, key=lambda a: (abs(a - midi), a))

def play(sample, semitones, held, ring):
    """One voice: resample by 2^(st/12) with linear interpolation; damp after
    `held` seconds unless the instrument rings."""
    step = 2.0 ** (semitones / 12.0)
    length = int(min((len(sample) - 1) / step, RATE * (4.0 if ring else held + 0.6)))
    pos = np.arange(length) * step
    i = pos.astype(np.int64)
    frac = (pos - i).astype(np.float32)
    v = sample[i] * (1 - frac) + sample[np.minimum(i + 1, len(sample) - 1)] * frac
    if not ring:
        k = int(held * RATE)
        if k < length:
            v[k:] *= RELEASE ** np.arange(length - k, dtype=np.float32)
    fade = min(length, int(0.02 * RATE))
    v[length - fade:] *= np.linspace(1, 0, fade, dtype=np.float32)
    return v

def render_phrase(c, anchors, banks):
    out = np.zeros(int(RATE * 7.5), dtype=np.float32)
    for onset, dur, off in PHRASE:
        midi = c["root"] + off
        a = nearest(anchors, midi)
        v = play(banks[a], midi - a, dur, c.get("ring", False))
        s = int(onset * RATE)
        n = min(len(v), len(out) - s)
        out[s:s + n] += v[:n]
    return out

def render_sf2(c):
    url, member, preset = c["sf2"]
    tool = CACHE / "sf2render"
    if not tool.exists():
        subprocess.run(["cc", "-O2", "-I", str(REPO / "slab/menuband-win"), str(HERE / "sf2render.c"),
                        "-o", str(tool), "-lm"], check=True)
    src = fetch(url, CACHE / "src" / c["id"] / urllib.parse.unquote(url.rsplit("/", 1)[-1]))
    sf2 = src
    if member:
        unpacked = CACHE / "src" / c["id"] / "unpacked"
        if not unpacked.exists():
            unpacked.mkdir(parents=True)
            subprocess.run(["bsdtar", "-xf", str(src), "-C", str(unpacked)], check=True)
        sf2 = next(unpacked.rglob("*" + member))
    def run(events):
        out = CACHE / "src" / c["id"] / "render.f32"
        lines = "\n".join(f"{t} {d} {m}" for t, d, m in events)
        subprocess.run([str(tool), str(sf2), str(preset), str(out)], input=lines, text=True, check=True)
        return np.fromfile(out, dtype="<f4")
    ring = c.get("ring", False)
    phrase = run([(t, -1 if ring else d, c["root"] + o) for t, d, o in PHRASE])
    note = run([(0, -1 if ring else 2.5, c["root"] + 4)])
    return note, phrase

# ── Output ────────────────────────────────────────────────────────────

def level(x, target_rms=0.1):
    """Even out loudness across candidates, then a tanh catches chord peaks."""
    active = x[np.abs(x) > 1e-4]
    rms = float(np.sqrt(np.mean(active ** 2))) if len(active) else 0
    if rms > 0:
        x = x * (target_rms / rms)
    return np.tanh(x * 1.2) / 1.2

def trim(x, floor=1e-4):
    loud = np.nonzero(np.abs(x) > floor)[0]
    return x[: loud[-1] + int(0.05 * RATE)] if len(loud) else x

def peaks(x, n=180):
    buckets = np.array_split(np.abs(x), n)
    p = np.array([b.max() if len(b) else 0 for b in buckets])
    return [round(float(v), 3) for v in p / max(p.max(), 1e-9)]

def write_mp3(x, path):
    path.parent.mkdir(parents=True, exist_ok=True)
    subprocess.run(["ffmpeg", "-v", "error", "-y", "-f", "f32le", "-ar", str(RATE), "-ac", "1", "-i", "-",
                    "-codec:a", "libmp3lame", "-b:a", "128k", str(path)], input=x.astype("<f4").tobytes(), check=True)

def note_name(m):
    return ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"][m % 12] + str(m // 12 - 1)

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", default=str(Path.home() / "Desktop/sampled-instruments"))
    ap.add_argument("--only", help="comma-separated candidate ids")
    args = ap.parse_args()
    out = Path(args.out).expanduser()
    only = set(args.only.split(",")) if args.only else None
    manifest = []
    for c in CANDIDATES:
        if only and c["id"] not in only:
            continue
        print(f"[{c['id']}]", file=sys.stderr)
        try:
            entry = {k: v for k, v in c.items() if k in ("id", "name", "family", "license", "root", "note")}
            entry["playback"] = "ring" if c.get("ring") else "sustain" if c.get("sustain") else "damped"
            if "sf2" in c:
                note, phrase = render_sf2(c)
                entry.update(renderer="SF2 via TinySoundFont", anchors=None)
            else:
                anchors = anchors_for(c)
                if not anchors:
                    raise RuntimeError("no files matched the pick pattern")
                needed = {nearest(anchors, c["root"] + o) for _, _, o in PHRASE}
                centre = nearest(anchors, c["root"] + 4)
                banks = {a: decode(anchors[a], c["id"]) for a in needed | {centre}}
                phrase = render_phrase(c, anchors, banks)
                note = banks[centre][: RATE * 4].copy()
                ms = list(anchors)
                gaps = np.diff(ms) if len(ms) > 1 else [0]
                entry.update(renderer="nearest anchor, as audio.c", anchors=len(ms),
                             range=f"{note_name(ms[0])}–{note_name(ms[-1])}",
                             spacing=round(float(np.median(gaps)), 1), noteName=note_name(centre))
            note, phrase = level(trim(note)), level(trim(phrase))
            write_mp3(note, out / "audio" / f"{c['id']}-note.mp3")
            write_mp3(phrase, out / "audio" / f"{c['id']}-phrase.mp3")
            entry.update(peaks=peaks(phrase), seconds=round(len(phrase) / RATE, 2))
            manifest.append(entry)
        except Exception as e:  # keep going; the report lists what failed
            print(f"    FAILED: {e}", file=sys.stderr)
            manifest.append({**{k: c[k] for k in ("id", "name", "family", "license")}, "error": str(e)})
    (out / "manifest.json").write_text(json.dumps(manifest, indent=1))
    page = (HERE / "report.html").read_text()
    (out / "index.html").write_text(page.replace("/*MANIFEST*/[]", json.dumps(manifest)))
    ok = sum("error" not in m for m in manifest)
    print(f"{ok}/{len(manifest)} rendered → {out}", file=sys.stderr)

if __name__ == "__main__":
    main()
