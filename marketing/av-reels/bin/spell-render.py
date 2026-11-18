#!/usr/bin/env python3
"""spell-render.py — render spells from Aesthetic Spells being cast in the hand.

    python spell-render.py notepat line "tone 440" [--ground street|grass] [--final]
                           [--name SLUG] [--out DIR] [--poly 0.18] [--no-home]

Each spell is typed at the prompt, cast, used for a moment, and left by
tapping the corner label, so the clip returns to the prompt between spells.
The result is your own view of walking with the slate in one hand while the
other hand casts.

Steps, each on the machine suited to it:
  1. panda   capture-av.mjs plays the performance on aesthetic.computer and
             records the screen with the page's own audio.
  2. here    lock-lights-to-audio.py puts notepat's key lights on their notes
             and writes the touch track; footsteps.py adds steps on the beat.
  3. jastow  Blender renders scene/liquid-walk.py; ffmpeg joins the sound.

Default is a scratch render (half size, every other frame, a couple of
minutes). --final renders every frame at 1080x1920 with motion blur.

How a spell is used once cast is decided by what kind of thing it is:
notes for notepat, a few taps for the sounding pieces, a stroke for the
drawing ones, and a look for the rest. Spells that publish, charge, notify
or delete are refused, as in tests/browser/spellbook.e2e.mjs.

Needs: ssh access to the capture and render hosts (AC_CAPTURE_HOST, default
panda; AC_RENDER_HOST, default jastow by its tailnet address), ffmpeg, and a
Python with numpy for the two analysis steps (pop/.venv has it).
"""
import argparse, json, math, os, re, subprocess, sys, tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.abspath(os.path.join(HERE, "..", "..", ".."))
PANDA = os.environ.get("AC_CAPTURE_HOST", "panda")                 # captures (headless Chrome)
JASTOW = os.environ.get("AC_RENDER_HOST", "me@100.103.42.46")      # renders (Blender on the GPU tower)
PY = os.path.join(REPO, "pop/.venv/bin/python")
if not os.path.exists(PY): PY = sys.executable

REFUSED = {"done", "publish", "print", "mug", "scream", "delete-erase-and-forget-me", "give", "pp", "bye", "imnew", "hi"}
NOTES = {"notepat"}
TAPS = {"bleep", "metronome", "tone", "clock", "stample", "bubble", "bgm", "whistle"}
DRAWS = {"line", "oval", "rect", "shape", "smear", "sparkle", "blur", "crop"}
STAYS = {"dark", "light", "flip", "flop", "left", "right", "fill", "resize", "new", "no", "yes", "word",
         "danish", "english", "spanish", "chinese", "japanese", "russian"}          # these answer at the prompt

ap = argparse.ArgumentParser()
ap.add_argument("spells", nargs="+")
ap.add_argument("--ground", default="street", choices=["street", "grass"])
ap.add_argument("--final", action="store_true")
ap.add_argument("--name")
ap.add_argument("--out", default=os.path.expanduser("~/Desktop/ac-ads/spells"))
ap.add_argument("--poly", default="0.18")
ap.add_argument("--no-home", action="store_true", help="skip the opening homepage and tap")
args = ap.parse_args()
for sp in args.spells:
    if sp.split()[0].split(":")[0] in REFUSED: sys.exit(f"refusing to cast '{sp}': it publishes, charges, notifies or deletes")
name = args.name or re.sub(r"[^a-z0-9]+", "-", "-".join(args.spells).lower()).strip("-")[:48]
tag = f"{name}-{args.ground}" + ("" if args.final else "-scratch")
os.makedirs(args.out, exist_ok=True)
work = tempfile.mkdtemp(prefix="spell-render-")

def run(cmd, **kw):
    r = subprocess.run(cmd, shell=isinstance(cmd, str), text=True, capture_output=True, **kw)
    if r.returncode != 0: sys.exit(f"failed: {cmd if isinstance(cmd, str) else ' '.join(cmd)}\n{r.stdout[-1500:]}\n{r.stderr[-1500:]}")
    return r.stdout

# ── 1. the performance ───────────────────────────────────────────────────
WI = {c: i for i, c in enumerate("cdefgab")}; WI.update({c: i for i, c in enumerate("hijklmn")})
def key_pos(k):
    top = k in "cdefgab"; return {"x": round((8 + (WI[k] + 0.5) * 26.29) / 200, 4), "y": 0.45 if top else 0.857}
A = []; ids = [0]
def nid(): ids[0] += 1; return ids[0]
def tap(t, x, y, tag): A.append({"t": int(t), "type": "tap", "x": x, "y": y, "id": nid(), "tag": tag})
def typed(t, text):
    for i, ch in enumerate(text): A.append({"t": int(t + i * 190), "type": "press", "key": "Space" if ch == " " else ch, "tag": "letter"})
    A.append({"t": int(t + len(text) * 190 + 250), "type": "press", "key": "Enter", "tag": "enter"}); return t + len(text) * 190 + 250
def phrase(t, pattern, slot=333):
    seq = [(i, ch) for i, ch in enumerate(pattern) if ch != "."]
    for n, (s, ch) in enumerate(seq):
        tt = t + s * slot; nxt = t + seq[n + 1][0] * slot if n + 1 < len(seq) else tt + 900
        i = nid(); A.append({"t": int(tt), "type": "mdown", "id": i, "tag": "note", **key_pos(ch)})
        A.append({"t": int(tt + max(120, min(nxt - tt - 150, 560))), "type": "mup", "id": i})
    return t + len(pattern) * slot
def stroke(t, pts, dur):
    i = nid(); A.append({"t": int(t), "type": "mdown", "id": i, "x": round(pts[0][0], 4), "y": round(pts[0][1], 4), "tag": "draw"})
    for k, (x, y) in enumerate(pts[1:], 1): A.append({"t": int(t + dur * k / (len(pts) - 1)), "type": "move", "id": i, "x": round(x, 4), "y": round(y, 4)})
    A.append({"t": int(t + dur + 60), "type": "mup", "id": i}); return t + dur + 60
LABEL = (0.1, 0.022)
PHRASES = ["a.h.j.iha...", "j.i.h.a.g.a.", "a.c.e.g.a..."]
t = 1800 if not args.no_home else 800
tap(t, 0.5, 0.3, "wake"); t += 1200
for n, sp in enumerate(args.spells):
    word = sp.split()[0].split(":")[0]
    t = typed(t, sp) + 2000                                   # let it load
    if word in NOTES:
        tap(t - 500, 0.5, 0.975, "prime")                     # the first touch in a fresh piece sounds but does not light
        t = phrase(t + 300, PHRASES[n % len(PHRASES)]) + 300
    elif word in TAPS:
        for i, (x, y) in enumerate([(0.3, 0.70), (0.55, 0.52), (0.72, 0.36), (0.42, 0.26)]): tap(t + i * 666, x, y, "use")
        t += 4 * 666 + 300
    elif word in DRAWS:
        wave = [(0.18 + 0.64 * u, 0.55 + 0.16 * math.sin(u * math.pi * 3)) for u in [i / 26 for i in range(27)]]
        t = stroke(t, wave, 1500)
        loop = [(0.5 + 0.2 * math.cos(a), 0.30 + 0.11 * math.sin(a)) for a in [i / 22 * 2 * math.pi for i in range(23)]]
        t = stroke(t + 450, loop, 1200) + 400
    else:
        t += 1500                                             # just look
    if word not in STAYS:
        tap(t + 300, *LABEL, "back"); t += 1500
    else:
        t += 600
A.sort(key=lambda a: a["t"]); dur = A[-1]["t"] / 1000 + 1.6
perf = os.path.join(work, "perf.json"); json.dump(A, open(perf, "w"))
print(f"▸ {tag}: {len(args.spells)} spell(s), {dur:.1f}s performance")

# ── 2. capture on panda ──────────────────────────────────────────────────
run(f"scp -q -o ConnectTimeout=10 {HERE}/capture-av.mjs {HERE}/performances.mjs {PANDA}:ac-ads/marketing/av-reels/bin/")
run(f"scp -q -o ConnectTimeout=10 {perf} {PANDA}:ac-ads/spell-perf.json")
out = run(f"ssh -o ConnectTimeout=10 {PANDA} 'zsh -lic \"cd ~/ac-ads && node marketing/av-reels/bin/capture-av.mjs prompt --slug spell --no-wake --label --duration {dur:.1f} --density 6 --perform-file spell-perf.json 2>&1 | tail -4\"'")
if "✓ base" not in out: sys.exit("capture failed:\n" + out)
base, cap = os.path.join(work, "base.mp4"), os.path.join(work, "capture.json")
run(f"scp -q -o ConnectTimeout=10 {PANDA}:ac-ads/marketing/av-reels/out/spell/base-spell.mp4 {base}")
run(f"scp -q -o ConnectTimeout=10 {PANDA}:ac-ads/marketing/av-reels/out/spell/capture.json {cap}")
print("  captured")

# ── 3. lock, touch track, footsteps ──────────────────────────────────────
locked, events = os.path.join(work, "locked.mp4"), os.path.join(work, "events.json")
print("  " + run([PY, f"{HERE}/lock-lights-to-audio.py", base, cap, locked, events, "--perf", perf]).strip().replace("\n", "\n  "))
wav, steps = os.path.join(work, "walk.wav"), os.path.join(work, "steps.json")
print("  " + run([PY, f"{HERE}/footsteps.py", locked, events, wav, steps, "--period", "0.666"] + (["--grass"] if args.ground == "grass" else [])).strip())
st = json.load(open(steps))

# ── 4. render on jastow ──────────────────────────────────────────────────
R = f"reelfarm/ads/spells/{tag}"
hdri = "meadow_2_2k.hdr" if args.ground == "grass" else "urban_street_04_2k.hdr"
run(f"ssh -o ConnectTimeout=10 {JASTOW} 'rm -rf ~/{R} && mkdir -p ~/{R}/screen ~/{R}/frames'")
# jastow's link drops now and then mid-upload: resume and retry
for attempt in range(4):
    r = subprocess.run(f'rsync -a --partial --timeout=40 -e "ssh -o ConnectTimeout=10 -o ServerAliveInterval=10" '
                       f'{REPO}/marketing/av-reels/scene/liquid-walk.py {locked} {events} {wav} {JASTOW}:{R}/', shell=True, capture_output=True, text=True)
    if r.returncode == 0: break
    print(f"  upload dropped (try {attempt + 1}), retrying")
else: sys.exit("could not upload to jastow:\n" + r.stderr[-600:])
flags = "" if args.final else "--scratch"
rate = 30 if args.final else 15
post = "noise=alls=6:allf=t+u,vignette=PI/5.5" if args.final else "null"
remote = f"""cd ~/{R} && ffmpeg -nostdin -v error -i locked.mp4 -vf fps=30 -start_number 1 screen/%05d.png && N=$(ls screen | wc -l) &&
~/.local/bin/render-progress-wall ~/{R}/frames $(( {'N' if args.final else '(N+1)/2'} )) "spell render: {tag}";
~/.local/bin/blender -b --gpu-backend vulkan -P liquid-walk.py -- --frames screen --count $N --events events.json \
  --hand ~/reelfarm/ads/hand/right.glb --hand-left ~/reelfarm/ads/hand/left.glb --out frames \
  --hdri ~/.cache/ac-reel-assets/hdri/{hdri} --ground {args.ground} --poly {args.poly} \
  --steps {st['stepsPerSecond']:.4f} --step-phase {st['t0']:.3f} {flags} > render.log 2>&1 < /dev/null;
grep -E "SCENE_DONE|Error|Traceback" render.log | tail -3;
ffmpeg -nostdin -v error -y -framerate {rate} -pattern_type glob -i "frames/*.png" -i walk.wav -map 0:v -map 1:a -shortest -vf {post} \
  -c:v libx264 -crf 18 -preset medium -pix_fmt yuv420p -c:a aac -b:a 256k -movflags +faststart out.mp4 && ls -la out.mp4"""
print("  rendering on jastow …")
# jastow's login shell is fish, so the script goes to bash on stdin
print("  " + run(["ssh", "-o", "ConnectTimeout=10", "-o", "ServerAliveInterval=15", JASTOW, "bash -s"], input=remote).strip().replace("\n", "\n  "))
final = os.path.join(args.out, f"{tag}.mp4")
for attempt in range(4):
    r = subprocess.run(f'rsync -a --partial --timeout=40 -e "ssh -o ConnectTimeout=10 -o ServerAliveInterval=10" {JASTOW}:{R}/out.mp4 {final}', shell=True, capture_output=True, text=True)
    if r.returncode == 0: break
else: sys.exit("rendered, but could not fetch it from jastow:\n" + r.stderr[-600:])
print(f"✓ {final}")
