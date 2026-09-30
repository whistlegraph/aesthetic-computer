"""natural-vox.py — her real voice, gently tuned, no vocoder.

aesthetivox.py rebuilds every sung frame with WORLD and splices her own
consonants back in between. That composite is what made the vowels skip and
lose their sung quality (v19 ear-check). This keeps the recording itself:
it takes aesthetivox's tuning curve (the note it would sing, per 5 ms frame),
turns it into a per-frame pitch OFFSET against what she actually sang, and
runs the dry stem through ONE rubberband R3 pass with that pitch map
(formant-preserving). Her breath, grit and vibrato stay hers.

  pop/.venv/bin/python pop/sailor-song/bin/natural-vox.py [--strength 0.7]
    → src/vox/vocals-natural.wav   (the engine prefers it over vocals-aesthetivox)
"""
import argparse, os, subprocess, tempfile
import numpy as np
import soundfile as sf
from scipy.signal import correlate

HERE = os.path.dirname(os.path.abspath(__file__))
VOX = os.path.join(HERE, "..", "src", "vox")
FRAME_MS = 5.0

ap = argparse.ArgumentParser()
ap.add_argument("--strength", type=float, default=0.7)   # of aesthetivox's correction; 1.0 = the full snap
ap.add_argument("--max-st", type=float, default=1.5)     # never shift a frame further than this
args = ap.parse_args()

dbg = os.path.join(tempfile.gettempdir(), "sailor-avox-debug.npz")
if not os.path.exists(dbg):
    subprocess.run([os.path.join(HERE, "..", "..", ".venv", "bin", "python"), os.path.join(HERE, "aesthetivox.py")],
                   env={**os.environ, "AVOX_DEBUG": dbg}, check=True)
d = np.load(dbg)
f0, sung, ms = d["f0"], d["sung"].astype(bool), d["ms"]
her_hz = 440.0 * 2 ** ((ms - 69) / 12)
corr = np.where(sung & (ms > 0), 12 * np.log2(np.maximum(f0, 1) / np.maximum(her_hz, 1)), np.nan)
corr = np.clip(corr * args.strength, -args.max_st, args.max_st)

# hold the correction through unsung frames (so a consonant never snaps the pitch
# back mid-word) and smooth it over 25 ms so the shift glides, never steps
idx = np.where(np.isfinite(corr))[0]
corr = np.interp(np.arange(len(corr)), idx, corr[idx])
k = int(25 / FRAME_MS)
corr = np.convolve(np.pad(corr, k, mode="edge"), np.ones(2 * k + 1) / (2 * k + 1), "valid")

dry = os.path.join(VOX, "vocals-dry-48k.wav")
x, fs = sf.read(dry, dtype="float64", always_2d=True)
spf = int(round(fs * FRAME_MS / 1000))
out = os.path.join(VOX, "vocals-natural.wav")
with tempfile.TemporaryDirectory() as tmp:
    pmap = os.path.join(tmp, "pitch.map")
    with open(pmap, "w") as f:
        f.write("".join(f"{i * spf} {c:.4f}\n" for i, c in enumerate(corr)))
    raw = os.path.join(tmp, "shifted.wav")
    subprocess.run(["rubberband", "-3", "-F", "-q", "--pitchmap", pmap, dry, raw], check=True)
    y, _ = sf.read(raw, dtype="float64", always_2d=True)

# the pitch map runs rubberband in realtime mode, which adds latency: find it by
# cross-correlating against the dry stem and slide the result back into place
a, b = x[: 30 * fs, 0], y[: 30 * fs + fs, 0]
lag = int(np.argmax(correlate(b, a, mode="valid", method="fft")))
y = y[lag: lag + len(x)]
if len(y) < len(x):
    y = np.pad(y, ((0, len(x) - len(y)), (0, 0)))
# the bedroom out, a crisp pop vocal in: the phone mic's boxy room (250) and honk (450)
# cut, presence at 3.2 k and air above 9 k lifted, the esses held back after the lift
EQ = ("highpass=f=110:p=2,equalizer=f=250:t=q:w=1.2:g=-3.5,equalizer=f=450:t=q:w=1.5:g=-2.5,"
      "equalizer=f=900:t=q:w=2:g=-1,equalizer=f=3200:t=q:w=1.2:g=2.5,highshelf=f=9000:g=3.5,"
      "deesser=i=0.35:m=0.5:f=0.5")
with tempfile.TemporaryDirectory() as tmp:
    flat = os.path.join(tmp, "flat.wav")
    sf.write(flat, y, fs, subtype="FLOAT")
    subprocess.run(["ffmpeg", "-v", "error", "-y", "-i", flat, "-af", EQ, "-c:a", "pcm_f32le", out], check=True)
print(f"strength {args.strength} · median |shift| {np.median(np.abs(corr[idx])):.2f} st · latency {lag / fs * 1000:.1f} ms → {out}")
