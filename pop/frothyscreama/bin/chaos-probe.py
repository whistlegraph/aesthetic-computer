#!/usr/bin/env python3
"""
chaos-probe.py — measure the chaos takes (bin/chaos.mjs), keep the ones that
are really screamed, and write the bank.

A wordless take is not judged by its words. What is measured: how long it is,
how loud, how rough (spectral flatness, and how much of the loud part has no
pitch at all), how unsteady the pitch is, and — by whisper.cpp — whether the
model screamed or only read the tag or the letters aloud.

  pop/.venv/bin/python pop/frothyscreama/bin/chaos-probe.py           measure new takes, print the table
  pop/.venv/bin/python pop/frothyscreama/bin/chaos-probe.py --bank    …and write out/chaos/: keepers, manifest.json, the audition reel
"""
import json, os, re, subprocess, sys, tempfile, warnings
warnings.filterwarnings("ignore")
from pathlib import Path
import numpy as np, librosa

HERE = Path(__file__).resolve().parent
BANK = HERE.parent / "out" / "chaos"; BANK.mkdir(parents=True, exist_ok=True)
CACHE = BANK / "measure.json"
FF = "/opt/homebrew/bin/ffmpeg"
MODEL = os.environ.get("WHISPER_MODEL", str(Path.home() / "Models/ggml-large-v3-turbo-q5_0.bin"))
# Words whisper writes for a scream. Anything else it hears is speech.
NOISE = set("a ah aah ahh ahhh aaah aaa oh ooh ohh ow ugh argh agh gah rah raah ha haha hah eh ee eek hey ho whoa wow yeah ya yah yay no ooo uh hmm mm grr rawr wah waah woo woah hi ay aye ai".split())
# Decided from the table, by hand: take id → why it is out.
REJECT = {
    "x28": "a hum through the teeth, not a roar",
}
KIND = {}


def decode(path, sr):
    raw = subprocess.run([FF, "-v", "error", "-i", str(path), "-f", "f32le", "-ac", "1", "-ar", str(sr), "-"], capture_output=True, check=True).stdout
    return np.frombuffer(raw, dtype=np.float32)


def measure(path):
    sr = 22050
    y = decode(path, sr)
    rms = librosa.feature.rms(y=y, frame_length=2048, hop_length=512)[0]
    loud = rms > rms.max() * 0.15
    on = np.flatnonzero(loud)
    f0, vf, vp = librosa.pyin(y, fmin=70, fmax=900, sr=sr, frame_length=2048, hop_length=512)
    n = min(len(f0), len(loud)); f0, loud_n = f0[:n], loud[:n]
    v = f0[loud_n & ~np.isnan(f0)]
    semi = 12 * np.log2(v / 440.0) if len(v) > 3 else np.zeros(1)
    flat = librosa.feature.spectral_flatness(y=y, n_fft=2048, hop_length=512)[0][:n]
    cen = librosa.feature.spectral_centroid(y=y, sr=sr, n_fft=2048, hop_length=512)[0][:n]
    with tempfile.TemporaryDirectory() as d:
        wav = Path(d) / "x.wav"
        subprocess.run([FF, "-v", "error", "-y", "-i", str(path), "-ar", "16000", "-ac", "1", str(wav)], check=True)
        heard = subprocess.run(["whisper-cli", "-m", MODEL, "-l", "en", "-np", "-nt", "-f", str(wav)], capture_output=True, text=True).stdout.strip().replace("\n", " ")
    return dict(
        seconds=round((on[-1] - on[0] + 1) * 512 / sr, 2) if len(on) else 0.0,
        peak_db=round(float(20 * np.log10(np.abs(y).max() + 1e-9)), 1),
        rms_db=round(float(20 * np.log10(np.sqrt(np.mean(rms[loud] ** 2)) + 1e-9)), 1),
        flatness=round(float(np.median(flat[loud_n])), 4),
        unpitched=round(float(1 - np.mean(~np.isnan(f0[loud_n]))), 2),
        f0_hz=round(float(np.median(v)), 0) if len(v) else 0.0,
        f0_spread_st=round(float(np.std(semi)), 2),
        f0_jump_st=round(float(np.mean(np.abs(np.diff(semi)))), 2) if len(semi) > 1 else 0.0,
        centroid_hz=round(float(np.median(cen[loud_n])), 0),
        heard=heard,
    )


def words(s):
    return [re.sub(r"(.)\1+", r"\1", w) for w in re.sub(r"\[[^\]]*\]|\*[^*]*\*|\([^)]*\)", " ", s.lower()).replace("'", "").split() if re.sub(r"[^a-z]", "", w)]


def spoken(m, text):
    """Did it speak? Real words in what whisper heard, beyond scream syllables."""
    w = [re.sub(r"[^a-z]", "", x) for x in words(m["heard"])]
    real = [x for x in w if x and x not in NOISE and re.sub(r"(.)\1+", r"\1", x) not in NOISE and not re.fullmatch(r"[aeiouhrgwyn]+", x) and not re.fullmatch(r"[bgy]l?[aeiou]+r?g?h+", x)]
    return real


def chaos(m):
    """One number to rank by: rough, unpitched, unsteady, bright."""
    return round(40 * min(1, m["flatness"] / 0.005) + 25 * m["unpitched"] + 20 * min(1, m["f0_spread_st"] / 6) + 15 * min(1, m["centroid_hz"] / 3000), 1)


def kind(m):
    if m["seconds"] < 1.0: return "stab"
    if re.search(r"\bha ha\b", m["heard"].lower()): return "laugh"
    if m["f0_hz"] == 0 or m["centroid_hz"] < 1300 or m["f0_hz"] < 300: return "growl"
    if m["f0_hz"] < 400: return "roar"
    if m["f0_hz"] >= 600: return "shriek"
    return "howl" if m["f0_spread_st"] < 1.5 else "scream"


def tame(m):
    """A clean held note: nearly all of it pitched, and the pitch not moving."""
    return m["seconds"] >= 1.0 and m["unpitched"] < 0.12 and m["f0_spread_st"] < 1.5


takes = json.loads(subprocess.run(["node", str(HERE / "chaos.mjs"), "--list"], capture_output=True, text=True, check=True).stdout)
cache = json.load(open(CACHE)) if CACHE.exists() else {}
rows = []
for t in takes:
    if not os.path.exists(t["path"]): continue
    k = os.path.basename(t["path"])
    if k not in cache:
        cache[k] = measure(t["path"]); json.dump(cache, open(CACHE, "w"), indent=1)
    m = dict(cache[k]); real = spoken(m, t["text"])
    why = REJECT.get(t["id"]) or (f"spoke: {' '.join(real)}" if real else None) or ("barely a sound" if m["seconds"] < 0.25 else None) or ("a clean held note, not a scream" if tame(m) else None)
    rows.append(dict(id=t["id"], take=k, text=t["text"], stability=t["stability"], style=t["style"], **m, chaos=chaos(m), kind=KIND.get(t["id"], kind(m)), rejected=why))
rows.sort(key=lambda r: -r["chaos"])
for r in rows:
    print(f"{r['id']} {'OUT ' if r['rejected'] else 'keep'} {r['kind']:6s} chaos {r['chaos']:5.1f}  {r['seconds']:4.1f}s rms {r['rms_db']:6.1f} flat {r['flatness']:.3f} unp {r['unpitched']:.2f} f0 {r['f0_hz']:4.0f} ±{r['f0_spread_st']:.1f}st jump {r['f0_jump_st']:.2f} cen {r['centroid_hz']:5.0f}  {r['text']!r} -> {r['heard'][:50]!r}{'  [' + r['rejected'] + ']' if r['rejected'] else ''}")

if "--bank" in sys.argv:
    SR = 48000
    keep = [r for r in rows if not r["rejected"]]
    count, reel, at, gate = {}, [], 0.0, "silenceremove=start_periods=1:start_threshold=-45dB:start_silence=0.02"
    for old in BANK.glob("*.wav"): old.unlink()
    for r in keep:
        count[r["kind"]] = count.get(r["kind"], 0) + 1
        r["file"] = f"{r['kind']}-{count[r['kind']]:02d}.wav"
        # the keeper: trimmed to the breath, mono 48 k float — what the renderer places
        subprocess.run([FF, "-v", "error", "-y", "-i", str(HERE.parent / "out/takes" / r["take"]), "-af", f"{gate},areverse,{gate},areverse", "-ar", str(SR), "-ac", "1", "-c:a", "pcm_f32le", str(BANK / r["file"])], check=True)
        y = decode(BANK / r["file"], SR); y = y / (np.abs(y).max() + 1e-9) * 0.8
        r["reel_at"] = round(at, 1)
        reel += [y, np.zeros(int(0.7 * SR), np.float32)]; at += len(y) / SR + 0.7
    json.dump(dict(voice="jeffrey-pvc", model="eleven_v4", made=len(rows), kept=keep, rejected=[r for r in rows if r["rejected"]]), open(BANK / "manifest.json", "w"), indent=1)
    out = BANK / "frothyscreama-chaos-reel.mp3"
    subprocess.run([FF, "-v", "error", "-y", "-f", "f32le", "-ar", str(SR), "-ac", "1", "-i", "-", "-b:a", "192k", str(out)], input=np.concatenate(reel).astype(np.float32).tobytes(), check=True)
    print(f"\n{out}")
    for r in keep: print(f"  {int(r['reel_at'] // 60)}:{r['reel_at'] % 60:04.1f}  {r['file']:14s} {r['seconds']:4.1f}s  chaos {r['chaos']:5.1f}  {r['text']}")
