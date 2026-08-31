"""Pastiche: the loner takes, crossfaded onto lonerclub v4pid.

Two vocal passes of the record (63.79s — the second door's click rush
leads straight back into "sitting", so the reel loops musically).
Fast cuts on the 122 BPM bar grid: Camille curled in the grass opens
wordless, the verse trades takes every two bars, pass two cuts every
bar then strobes in half-bar stabs, and the closer returns to her in
the grass over the door. No take with words in frame (the title card,
the captioned fan of notebooks, and the LONER-labeled paper sit out).
Each take carries a cursor so its drawing grows fuller as it runs.

Renders segments to $WORK/segs-<planhash>, chains xfades; run.sh
grades and hands to chrome-reel.mjs.
"""
import hashlib
import json
import os
import subprocess

HERE = os.path.dirname(os.path.abspath(__file__))
LONER = os.path.dirname(os.path.dirname(HERE))
WORK = os.environ.get("PASTICHE_WORK") or os.path.expanduser(
    "~/.cache/ac/pastiche"
)

BAR = 60.0 / 122.0 * 4
B0 = 0.838  # first "sitting" — bar zero of the vocal grid
END = B0 + 32 * BAR  # 63.79 — two passes, cut on the second door
W, H = 720, 1280

GRASS = "6988619239657622790"   # title card, then Camille in the grass
PAD = "6955972523087416582"     # CAMILLE glitter pad
FERAL = "7108062006980201771"   # the Feral File spine
OUTDOOR = "6994920700746206470" # LONER label on sunlit concrete
REDPEN = "7021262898479549702"  # the 13.8M take, red pen and beads
SMUDGE = "7173130377798716714"  # pencil on gray smudge, shallow focus
LACE = "7168939549962308906"    # brown paper over lace
CHARCOAL = "7168612922757877035"  # ruled paper, charcoal shading
NB1 = "7230893600219942186"     # twin notebooks on slate, most motion
NB2 = "7226527805008268586"     # fanned notebooks, "time to pass"
NB3 = "7226114462145695018"     # notebooks, green + black ink
PENS3A = "7226226683349798190"  # three pens at once
PENS3B = "7233886426910330158"  # three pens, other angle
BLUE = "7076361738786213166"    # big blue marker, split frame

DUR = {}
for _s in [GRASS, PAD, FERAL, OUTDOOR, REDPEN, SMUDGE, LACE, CHARCOAL,
           NB1, NB2, NB3, PENS3A, PENS3B, BLUE]:
    DUR[_s] = float(subprocess.run(
        ["ffprobe", "-v", "error", "-show_entries", "format=duration",
         "-of", "default=nw=1:nk=1", f"{LONER}/source/{_s}.mp4"],
        capture_output=True, text=True, check=True).stdout)


# track time -> drawing time (both passes), so segments can source
# their take at the moment the SAME stroke is being drawn on camera
_wc = json.load(open(f"{LONER}/viz/wordclock.json"))
_passes = [[_wc[0]]]
for _e in _wc[1:]:
    if _e["t0"] - _passes[-1][-1]["t1"] > 2.0:
        _passes.append([])
    _passes[-1].append(_e)
V_ANCHORS = []
for _p in _passes:
    V_ANCHORS.append((_p[0]["t0"] - 0.35, 0.0))
    for _e in _p:
        V_ANCHORS.append((_e["t0"], _e["v0"]))
    V_ANCHORS.append((_p[-1]["t1"], _p[-1]["v1"]))


def v_at(t):
    if t <= V_ANCHORS[0][0]:
        return 0.0
    for (t0, v0), (t1, v1) in zip(V_ANCHORS, V_ANCHORS[1:]):
        if t <= t1:
            if t1 <= t0:
                return v1
            return v0 + (v1 - v0) * (t - t0) / (t1 - t0)
    return V_ANCHORS[-1][1]


cursor = {s: 1.5 for s in DUR}
cursor[GRASS] = 0.5


def draw(src, need):
    """Advance a take's cursor so its drawing grows across the piece."""
    t = cursor[src]
    if t + need > DUR[src] - 0.1:
        t = max(0.5, DUR[src] - need - 0.3)
        cursor[src] = t
    cursor[src] = t + need * 0.55
    return t


# assembled as (end_bar | None=END, src, src_t, speed, fade_out, opts)
# Camille alone in the grass is wordless only ~3.55-4.15s (title card
# before, hands after) — the bookends slow that breath to 0.25x and
# clone-hold her last frame under the drifting zoom.
PLAN = []
PLAN.append((2.0, GRASS, 3.55, 0.25, 0.8, "hold:0.40"))

VERSE = [PAD, FERAL, REDPEN, PENS3B, SMUDGE, PAD, FERAL]
for i, b in enumerate([4, 6, 8, 10, 12, 14, 15]):
    src = VERSE[i]
    seg = 2 * BAR * 0.85 + 1.2
    PLAN.append((float(b), src, draw(src, seg), 0.85, 0.6, None))

PLAN.append((16.0, SMUDGE, draw(SMUDGE, 2.5), 0.6, 0.5, "blur"))

PASS2 = [NB1, NB3, PENS3A, LACE, CHARCOAL, PENS3B,
         NB1, NB3, PENS3A, CHARCOAL]
for i, src in enumerate(PASS2):
    b = 17.0 + i
    layered = b >= 23 and i % 3 == 2  # from the bells, every third
    opts = f"layer:{SMUDGE if src != SMUDGE else LACE}" if layered else None
    if opts:
        opts += f":{draw(SMUDGE, 2.5):.2f}"
    PLAN.append((b, src, draw(src, 2.6), 1.0, 0.35, opts))

STROBE = [NB1, PENS3A, BLUE, PENS3B, NB3, LACE, NB1, PENS3A]
for i, src in enumerate(STROBE):
    b = 26.5 + i * 0.5
    opts = "cropx:170" if src == BLUE else None
    if i in (5, 7):
        layer_src = PENS3A if src != PENS3A else SMUDGE
        opts = f"layer:{layer_src}:{draw(layer_src, 1.6):.2f}"
    PLAN.append((b, src, draw(src, 1.6), 1.1, 0.15, opts))

PLAN.append((None, GRASS, 3.55, 0.25, 0.0, "hold:0.40"))

SEGS = f"{WORK}/segs"
os.makedirs(SEGS, exist_ok=True)
print(f"plan: {len(PLAN)} segments")


def bounds():
    ts = [0.0]
    for row in PLAN:
        ts.append(END if row[0] is None else B0 + row[0] * BAR)
    return ts


def chain(speed, dur, cropx, zoom_in, hold=False):
    steps = [
        f"fps=30,scale={W}:{H}:force_original_aspect_ratio=increase",
        f"crop={W}:{H}" + (f":{cropx}:0" if cropx else ""),
        f"setpts=PTS/{speed}",
    ]
    if speed < 0.9:  # optical flow only where slow motion needs it
        steps.append("minterpolate=fps=30:mi_mode=mci:mc_mode=aobmc:vsbmc=1")
    else:
        steps.append("fps=30")
    if hold:  # freeze the last real frame under the zoom drift
        steps.append(f"tpad=stop_mode=clone:stop_duration={dur:.2f}")
    frames = int(dur * 30) + 1
    if zoom_in:
        z = f"min(1+0.06*on/{frames},1.06)"
    else:
        z = f"max(1.06-0.06*on/{frames},1.0)"
    steps.append(
        f"zoompan=z='{z}':x='(iw-iw/zoom)/2':y='(ih-ih/zoom)/2'"
        f":d=1:s={W}x{H}:fps=30"
    )
    return ",".join(steps)


ts = bounds()
segfiles = []
for i, (end_bar, src, src_t, speed, fade, opts) in enumerate(PLAN):
    t0, t1 = ts[i], ts[i + 1]
    fade_in = PLAN[i - 1][4] if i else 0.0
    dur = (t1 - t0) + fade_in / 2 + fade / 2
    key = hashlib.sha1(
        repr((PLAN[i], PLAN[i - 1][4] if i else 0.0, t1 - t0, src_t))
        .encode()).hexdigest()[:12]
    seg = f"{SEGS}/{key}.mp4"
    segfiles.append((seg, dur, fade))
    if os.path.exists(seg):
        continue
    if opts is None or opts.startswith("layer") or opts.startswith("cropx"):
        # sync: source the take where this moment's stroke is drawn
        frac = min(v_at(t0 + 0.2) / 25.0, 0.97)
        need = dur * speed + 0.7
        src_t = round(min(max(frac * (DUR[src] - need), 0.2),
                          DUR[src] - need), 2)
    cropx = None
    layer = None
    blur = False
    hold = False
    if opts:
        for o in opts.split(";"):
            if o == "blur":
                blur = True
            elif o.startswith("hold"):
                # hold:<sec> — how much source to read before the
                # clone-freeze; must stop short of the take's own cut
                hold = float(o.split(":")[1]) if ":" in o else 0.58
            elif o.startswith("cropx:"):
                cropx = int(o.split(":")[1])
            elif o.startswith("layer:"):
                _, lsrc, lt = o.split(":")
                layer = (lsrc, float(lt))
    src_need = hold if hold else dur * speed + 0.5
    assert src_t + src_need <= DUR[src] + 0.1, (
        f"seg {i}: {src} needs {src_t:.2f}+{src_need:.2f}s "
        f"but take is {DUR[src]:.2f}s"
    )
    cmd = ["ffmpeg", "-y", "-v", "error",
           "-ss", str(src_t), "-t", str(src_need),
           "-i", f"{LONER}/source/{src}.mp4"]
    main = chain(speed, dur, cropx, zoom_in=(i % 2 == 0), hold=hold)
    if layer:
        cmd += ["-ss", str(layer[1]), "-t", str(src_need),
                "-i", f"{LONER}/source/{layer[0]}.mp4"]
        over = chain(speed, dur, None, zoom_in=(i % 2 == 1))
        graph = (
            f"[0:v]{main}[a];[1:v]{over}[b];"
            f"[a][b]blend=all_mode=average,eq=contrast=1.12[v]"
        )
    else:
        graph = f"[0:v]{main}[v]"
    if blur:
        graph = graph.replace("[v]", ",gblur=sigma=7[v]")
    cmd += ["-filter_complex", graph, "-map", "[v]",
            "-t", f"{dur:.3f}", "-r", "30",
            "-c:v", "libx264", "-preset", "fast", "-crf", "16",
            "-pix_fmt", "yuv420p", seg]
    subprocess.run(cmd, check=True)
    print(f"seg {i:02d}  {t0:6.2f}-{t1:6.2f}  {src[:6]}… "
          f"{'layered ' if layer else ''}{'blur' if blur else ''}")

# chain the crossfades: each fade straddles its boundary
inputs = []
for seg, _, _ in segfiles:
    inputs += ["-i", seg]
graph = []
label = "[0:v]"
clock = segfiles[0][1]  # running end-time of the assembled chain
for i in range(1, len(segfiles)):
    fade = segfiles[i - 1][2]
    off = clock - fade
    out = f"[x{i}]" if i < len(segfiles) - 1 else "[v]"
    graph.append(
        f"{label}[{i}:v]xfade=transition=fade:"
        f"duration={fade:.3f}:offset={off:.3f}{out}"
    )
    label = out
    clock = off + segfiles[i][1]
print(f"assembled length {clock:.2f}s (target {END})")
subprocess.run(
    ["ffmpeg", "-y", "-v", "error", "-stats"] + inputs
    + ["-filter_complex", ";".join(graph), "-map", "[v]",
       "-r", "30", "-t", str(END),
       "-c:v", "libx264", "-preset", "fast", "-crf", "16",
       "-pix_fmt", "yuv420p", f"{WORK}/collage.mp4"],
    check=True,
)
print(f"pastiche collage -> {WORK}/collage.mp4")
