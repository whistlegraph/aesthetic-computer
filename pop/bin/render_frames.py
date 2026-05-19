#!/usr/bin/env python3
"""
render_frames.py — frame-by-frame TikTok renderer for big-pictures.

Reads a storyboard.json + word images + audio, produces a PNG frame
sequence ready for ffmpeg encode. Per-character glyph extraction + own
compositing lets us:

  - paste extracted character pixels onto programmatic backgrounds
    (no FLUX background artifacts)
  - smooth-gradient between slide colors during transitions
  - per-character vertical bounce driven by audio amplitude
  - loop-closure: last slide's slide-out reveals slide 0 (perfect loop)

Usage:
  render_frames.py --storyboard <path> --img-dir <path> --audio <path>
                   --frames-dir <path> --fps 30
"""
import argparse, json, os, sys
from pathlib import Path
import numpy as np
from PIL import Image, ImageDraw
from scipy.ndimage import binary_dilation, label, find_objects

try:
    import soundfile as sf
    HAS_SF = True
except ImportError:
    HAS_SF = False

# CSS named colors → RGB. We only use what the storyboard's emotional
# arc references; subset of the W3C CSS4 named colors.
CSS_COLORS = {
    "peachpuff": (255, 218, 185),
    "moccasin": (255, 228, 181),
    "wheat": (245, 222, 179),
    "khaki": (240, 230, 140),
    "palegoldenrod": (238, 232, 170),
    "lightyellow": (255, 255, 224),
    "lemonchiffon": (255, 250, 205),
    "burlywood": (222, 184, 135),
    "tan": (210, 180, 140),
    "rosybrown": (188, 143, 143),
    "thistle": (216, 191, 216),
    "lavender": (230, 230, 250),
    "mistyrose": (255, 228, 225),
    "skyblue": (135, 206, 235),
    "lightblue": (173, 216, 230),
    "mediumturquoise": (72, 209, 204),
    "mediumaquamarine": (102, 205, 170),
    "lightseagreen": (32, 178, 170),
    "palegreen": (152, 251, 152),
    "lightgreen": (144, 238, 144),
    "hotpink": (255, 105, 180),
    "deeppink": (255, 20, 147),
    "violet": (238, 130, 238),
    "orchid": (218, 112, 214),
    "salmon": (250, 128, 114),
    "gold": (255, 215, 0),
    "saddlebrown": (139, 69, 19),
    "darkred": (139, 0, 0),
    "indigo": (75, 0, 130),
    "darkolivegreen": (85, 107, 47),
    "maroon": (128, 0, 0),
    "darkgoldenrod": (184, 134, 11),
    "darkslateblue": (72, 61, 139),
    "darkslategray": (47, 79, 79),
    "ivory": (255, 255, 240),
    "white": (255, 255, 255),
    "navy": (0, 0, 128),
    "midnightblue": (25, 25, 112),
    "darkgreen": (0, 100, 0),
    "darkviolet": (148, 0, 211),
    "black": (0, 0, 0),
}

def color_rgb(name, default=(40, 40, 40)):
    return CSS_COLORS.get(name.lower(), default)

# ── Glyph extraction ──────────────────────────────────────────────────
# Connected-component segmentation. Foreground/background by corner
# sampling, then dilate to merge within-letter pixel fragments, then
# scipy.label() to find each letter's connected blob. Returns a list of
# {img, w, h, x0, y0, baseline_y, top_y} sorted left-to-right. Includes
# baseline + top so the renderer can align letters on a common y-line.

def extract_glyphs(word_img_path, foreground_threshold=38, dilate_iters=2,
                   merge_dx_frac=0.20):
    img = Image.open(word_img_path).convert("RGBA")
    arr = np.array(img, dtype=np.int16)
    h, w, _ = arr.shape
    # Background sample from the 4 corners (16×16 each)
    s = 16
    corners = np.concatenate([
        arr[:s, :s, :3].reshape(-1, 3),
        arr[:s, -s:, :3].reshape(-1, 3),
        arr[-s:, :s, :3].reshape(-1, 3),
        arr[-s:, -s:, :3].reshape(-1, 3),
    ])
    bg = np.median(corners, axis=0)
    diff = np.linalg.norm(arr[:, :, :3] - bg, axis=2)
    fg_mask = (diff > foreground_threshold).astype(np.uint8)

    # Dilate so within-letter fragments merge into one component while
    # adjacent letters stay separate.
    dilated = binary_dilation(fg_mask, iterations=dilate_iters)
    labeled, n_components = label(dilated)
    objects = find_objects(labeled)

    raw = []
    for ci, slc in enumerate(objects, 1):
        if slc is None:
            continue
        ys = slc[0]
        xs = slc[1]
        y0, y1 = int(ys.start), int(ys.stop)
        x0, x1 = int(xs.start), int(xs.stop)
        bw, bh = x1 - x0, y1 - y0
        if bw < 5 or bh < 10:
            continue
        # Cut from the ORIGINAL fg_mask (un-dilated) so glyph edges stay sharp
        local_mask = fg_mask[y0:y1, x0:x1]
        # Restrict to the labeled component so neighbor blobs in the same
        # bounding box don't bleed in
        comp_mask = (labeled[y0:y1, x0:x1] == ci).astype(np.uint8)
        cut = (local_mask & comp_mask).astype(np.uint8)
        if cut.sum() < 18:
            continue
        raw.append({"x0": x0, "x1": x1, "y0": y0, "y1": y1,
                    "mask": cut, "rgb": arr[y0:y1, x0:x1, :3].astype(np.uint8)})

    raw.sort(key=lambda g: g["x0"])

    # Merge vertically-stacked components (e.g. dot of i / j) that share
    # an x-range. Merge threshold: x overlap > merge_dx_frac of the
    # smaller component's width.
    merged = []
    for g in raw:
        if merged:
            prev = merged[-1]
            ox = max(0, min(prev["x1"], g["x1"]) - max(prev["x0"], g["x0"]))
            min_w = max(1, min(prev["x1"] - prev["x0"], g["x1"] - g["x0"]))
            if ox / min_w > merge_dx_frac:
                # Merge: union bounding box, OR masks
                nx0 = min(prev["x0"], g["x0"])
                nx1 = max(prev["x1"], g["x1"])
                ny0 = min(prev["y0"], g["y0"])
                ny1 = max(prev["y1"], g["y1"])
                nmask = np.zeros((ny1 - ny0, nx1 - nx0), dtype=np.uint8)
                # Place prev mask
                py = prev["y0"] - ny0
                px = prev["x0"] - nx0
                nmask[py:py + (prev["y1"] - prev["y0"]),
                      px:px + (prev["x1"] - prev["x0"])] |= prev["mask"]
                py2 = g["y0"] - ny0
                px2 = g["x0"] - nx0
                nmask[py2:py2 + (g["y1"] - g["y0"]),
                      px2:px2 + (g["x1"] - g["x0"])] |= g["mask"]
                # Pull RGB from underlying image array via outer indexing
                nrgb = arr[ny0:ny1, nx0:nx1, :3].astype(np.uint8)
                merged[-1] = {"x0": nx0, "x1": nx1, "y0": ny0, "y1": ny1,
                              "mask": nmask, "rgb": nrgb}
                continue
        merged.append(g)

    glyphs = []
    for g in merged:
        bw = g["x1"] - g["x0"]
        bh = g["y1"] - g["y0"]
        rgba = np.zeros((bh, bw, 4), dtype=np.uint8)
        rgba[:, :, :3] = g["rgb"]
        rgba[:, :, 3] = g["mask"] * 255
        glyphs.append({
            "img": Image.fromarray(rgba, "RGBA"),
            "w": int(bw),
            "h": int(bh),
            "x0": int(g["x0"]),
            "y0": int(g["y0"]),
            "y1": int(g["y1"]),
        })
    return glyphs

# ── Audio amplitude curve ─────────────────────────────────────────────
def amplitude_curve(audio_path, total_dur, fps):
    if not HAS_SF:
        return np.zeros(int(total_dur * fps) + 1)
    audio, sr = sf.read(audio_path, dtype="float64")
    if audio.ndim > 1:
        audio = audio.mean(axis=1)
    n_frames = int(total_dur * fps) + 1
    samples_per_frame = sr / fps
    env = np.zeros(n_frames)
    for f in range(n_frames):
        a = int(f * samples_per_frame)
        b = int(min(len(audio), a + samples_per_frame * 1.5))
        if b > a:
            env[f] = np.sqrt(np.mean(audio[a:b] ** 2))
    if env.max() > 0:
        env /= env.max()
    return env

# ── Slide animation: where does word i sit at time t? ────────────────
# Returns (x_offset, alpha) for each slide. x in pixels; alpha 0-1.
# Animation phases (matching tiktok.mjs):
#   t < inStart                 → x = W  (off right)
#   inStart..sStart             → x: W → +CRAWL_AMP  (slide in)
#   sStart..outStart            → x: +CRAWL → -CRAWL  (slow crawl)
#   outStart..sEnd              → x: -CRAWL → -W      (slide out)
#   t ≥ sEnd                    → x = -W

CRAWL_AMP = 35
TRANSITION_DUR_DEFAULT = 0.22

def slide_position(t, slide, W):
    s_start = slide["start"]
    s_end = slide["end"]
    trans_in = slide.get("transitionMs", 220) / 1000.0
    in_start = max(0.0, s_start - trans_in)
    next_trans = slide.get("nextTransitionMs", 220) / 1000.0
    out_start = s_end - next_trans
    if t < in_start:
        return W
    if t < s_start:
        prog = (t - in_start) / max(0.001, trans_in)
        return W - (W - CRAWL_AMP) * prog
    if t < out_start:
        prog = (t - s_start) / max(0.001, out_start - s_start)
        return CRAWL_AMP - 2 * CRAWL_AMP * prog
    if t < s_end:
        prog = (t - out_start) / max(0.001, next_trans)
        return -CRAWL_AMP - (W - CRAWL_AMP) * prog
    return -W

# ── Train camera: continuous global row of variable-width lanes ──────
# Each word gets a lane sized to fit its rendered width + padding.
# The camera pans from one lane center to the next over each slide's
# duration — short words = fast pan, long words = slow pan. All words
# share the same baseline y, all visible at once like one long line of
# text scrolling past.
def lane_center_world(i, lane_starts, lane_widths):
    return lane_starts[i] + lane_widths[i] / 2.0

def cam_at_slide_start(i, lane_starts, lane_widths, screen_w):
    # Camera offset such that lane i is centered on screen.
    return lane_center_world(i, lane_starts, lane_widths) - screen_w / 2.0

# Keyframe camera. Each slide owns TWO keyframes:
#   1. (in_end_t, cs_i)             — slide centered after ramp-in
#   2. (out_start_t, cs_i + PEEK)   — slide drifted slightly so next inches in
# Between adjacent slides, the camera traverses cs_i+PEEK → cs_(i+1)
# over (out_start_i .. in_end_(i+1)) — that's the rapid transition.
#
# Transition duration is CAPPED so very long-held slides don't waste
# motion budget on slow drifts; very short slides still get a usable
# hold window between in/out ramps.
HOLD_FRAC = 0.55
TRANS_CAP_S = 0.55          # longer transitions, more groovy
TRANS_MIN_S = 0.28          # min transition duration in seconds
MAX_HOLD_S = 1.4            # cap centered-hold duration; long sustains
                            # (e.g. 5-beat 'found' = 4.3s) get a slow
                            # drift toward the next slide instead of
                            # 3.6s of dead-center stillness.

def transition_dur(slide_dur):
    return max(TRANS_MIN_S, min(TRANS_CAP_S, slide_dur * (1 - HOLD_FRAC) / 2))

def camera_x(t, slides, lane_starts, lane_widths, screen_w, peek_px,
             loop_end_t=None):
    n = len(slides)
    if n == 0:
        return 0.0
    cs = lambda i: lane_starts[i] + lane_widths[i] / 2.0 - screen_w / 2.0
    LANE_W = lane_widths[0] if lane_widths else screen_w
    # Build keyframe list — slide-in happens DURING the first td of
    # the new word's audio (not before — user feedback: "slides
    # slide before they're spoken" was bad). Camera arrives centered
    # at slide.start + td. Then dead-still hold (no drift) until
    # slide.end. With LANE_W = 1.85*W, the wide lane spacing makes
    # the slide scroll fully off-screen left before the next one
    # enters from the right — no "doubling" two words on screen.
    kfs = []
    for i, s in enumerate(slides):
        d = s["end"] - s["start"]
        td = transition_dur(d)
        in_end = s["start"] + td
        kfs.append((in_end, cs(i)))           # arrival, td after audio start
        kfs.append((s["end"], cs(i)))         # held dead-still until end
    if loop_end_t is not None and loop_end_t > kfs[-1][0]:
        # Loop closure: from cs(n-1) at s_{n-1}.end, slide back to slide_0.
        # cs(0)+n*LANE_W ≡ cs(0) mod n*LANE_W, so the camera lands on the
        # FIRST slide centered at loop_end_t. Frame 0 of the next loop
        # iteration is also at cs(0) — perfect seam, slide_0 is centered
        # on both sides of the wrap. The slide-back motion plays out over
        # the trailing silence, satisfying "loop back to the first frame".
        kfs.append((loop_end_t, cs(0) + n * LANE_W))
    first_t, first_pos = kfs[0]
    last_t, last_pos = kfs[-1]
    if t <= first_t:
        return first_pos  # no pre-roll — slide_0 centered at t=0
    if t >= last_t:
        return last_pos
    # Find bracket — LINEAR interpolation (no cosine ease) for groovy,
    # constant-velocity transitions instead of springy fast-middle.
    for k in range(len(kfs) - 1):
        t0, p0 = kfs[k]
        t1, p1 = kfs[k + 1]
        if t0 <= t < t1:
            prog = (t - t0) / max(0.001, t1 - t0)
            return p0 + prog * (p1 - p0)
    return last_pos

# ── Per-character "currently being sung" bounce + Dock zoom ──────────
# As the slide plays, the character currently being vocalized bounces
# high and zooms up. Neighbors bounce/zoom less. Like macOS dock hover
# but the cursor moves left-to-right through the word over the slide's
# duration.
DOCK_WINDOW = 1.8       # influence each side — wider so neighbors join in
MAX_BOUNCE_PX = 140     # peak vertical bounce (was 64) — way more dramatic
MAX_ZOOM = 1.45         # peak scale-up at the cursor (was 1.0 = no zoom)
MAX_ROTATION_DEG = 14   # peak rotation wiggle, ± degrees
MAX_X_WIGGLE_PX = 22    # peak horizontal wiggle
WIGGLE_HZ = 7.5         # wiggle frequency for rotation+x — fast enough to feel jittery

def char_emphasis(t, slide, char_idx, n_chars, amp_now):
    # Time window over which the singing-cursor traverses the word —
    # the slide's full audible duration (start → end).
    s_start = slide["start"]
    s_end = slide["end"]
    if n_chars <= 0 or t < s_start or t >= s_end:
        return 0.0
    progress = (t - s_start) / max(0.001, s_end - s_start)
    cursor_pos = progress * (n_chars - 1)
    distance = abs(char_idx - cursor_pos)
    if distance > DOCK_WINDOW:
        return 0.0
    weight = 0.5 + 0.5 * np.cos(np.pi * distance / DOCK_WINDOW)
    # Amp-driven response — minimal floor so quiet moments stay quiet,
    # peak moments really pop. Bias toward amp so loud peaks dominate.
    return weight * (0.15 + 0.85 * amp_now)

def char_bounce_y(emphasis):
    return int(emphasis * MAX_BOUNCE_PX * -1)  # negative = up

def char_zoom(emphasis):
    return 1.0 + emphasis * (MAX_ZOOM - 1.0)

def char_rotation(emphasis, t, char_idx):
    # Each char wiggles on its own phase (offset by index) so the word
    # doesn't rock as a single block — letters look alive.
    if emphasis <= 0:
        return 0.0
    phase = 2 * np.pi * (WIGGLE_HZ * t + char_idx * 0.37)
    return float(emphasis * MAX_ROTATION_DEG * np.sin(phase))

def char_x_wiggle(emphasis, t, char_idx):
    if emphasis <= 0:
        return 0
    # Cosine offset against rotation's sine — gives a slight orbital feel
    phase = 2 * np.pi * (WIGGLE_HZ * t + char_idx * 0.37) + np.pi / 2
    return int(emphasis * MAX_X_WIGGLE_PX * np.cos(phase))


# ── Live-waveform connector polyline ─────────────────────────────────
# Renders a single thick scrolling waveform polyline between two words.
# Y-position follows recent audio amplitude (older samples on left,
# newer on right — feels like the audio is flowing through the dash
# left-to-right). Color gradients from cur_color → nxt_color across
# the polyline length, like an em-dash carrying the upcoming word.
DASH_HALF_SPAN_PX = 110   # half-width — short flat punctuation
DASH_AMP_HEIGHT_PX = 0    # FLAT — no audio modulation, just punctuation
DASH_THICKNESS = 16
DASH_N_POINTS = 6         # minimal — just enough for color gradient segments
DASH_FRAMES_PER_POINT = 1
DASH_ALPHA = 130          # mild ambient feel


DASH_LOWRES_DIVISOR = 3   # render at 1/3 res, NEAREST upscale → pixelated

def _draw_waveform_dash(img, gap_screen_x, midline_y,
                        cur_color, nxt_color, amp_curve, frame_idx, fps,
                        W, H):
    # Render the dash at low resolution onto a small RGBA layer, then
    # NEAREST-upscale to give it a chunky pixel-art / staircase look,
    # matching the letterforms' aesthetic.
    div = DASH_LOWRES_DIVISOR
    lo_w = (DASH_HALF_SPAN_PX * 2) // div + 4
    lo_h = (DASH_AMP_HEIGHT_PX * 2 + DASH_THICKNESS * 2) // div + 4
    layer = Image.new("RGBA", (lo_w, lo_h), (0, 0, 0, 0))
    draw = ImageDraw.Draw(layer)
    n = DASH_N_POINTS
    cy = lo_h // 2  # midline within layer
    points = []
    for k in range(n):
        x_lo = (k / (n - 1)) * (lo_w - 4) + 2
        # Flat horizontal line — uniform punctuation between words,
        # no audio modulation. The color gradient still runs across.
        points.append((x_lo, cy))
    # Draw thick polyline at low-res; the upscale's NEAREST gives
    # natural staircases without antialiasing.
    lo_thick = max(1, DASH_THICKNESS // div)
    for k in range(n - 1):
        t = k / (n - 2)
        col = tuple(int(cur_color[c] * (1 - t) + nxt_color[c] * t)
                    for c in range(3)) + (255,)
        draw.line([points[k], points[k + 1]], fill=col, width=lo_thick)
    r = max(1, lo_thick // 2)
    for k, (px, py) in enumerate(points):
        t = k / (n - 1)
        col = tuple(int(cur_color[c] * (1 - t) + nxt_color[c] * t)
                    for c in range(3)) + (255,)
        draw.ellipse([px - r, py - r, px + r, py + r], fill=col)
    # NEAREST upscale → pixelated, then dim alpha for ambient feel
    up = layer.resize((lo_w * div, lo_h * div), Image.NEAREST)
    up_arr = np.array(up)
    up_arr[:, :, 3] = (up_arr[:, :, 3].astype(np.float32) * (DASH_ALPHA / 255.0)).astype(np.uint8)
    up = Image.fromarray(up_arr, "RGBA")
    paste_x = int(gap_screen_x - up.width // 2)
    paste_y = int(midline_y - up.height // 2)
    img.paste(up, (paste_x, paste_y), up)

# ── Main ─────────────────────────────────────────────────────────────
def main():
    p = argparse.ArgumentParser()
    p.add_argument("--storyboard", required=True)
    p.add_argument("--img-dir", required=True)
    p.add_argument("--audio", required=True)
    p.add_argument("--frames-dir", required=True)
    p.add_argument("--fps", type=int, default=30)
    args = p.parse_args()

    sb = json.load(open(args.storyboard))
    slides = sb["slides"]
    W = sb["resolution"]["w"]
    H = sb["resolution"]["h"]
    total_dur = sb["duration"]
    fps = args.fps

    # Compute next-transition for each slide (used for slide-out)
    for i, s in enumerate(slides):
        nxt = slides[i + 1] if i + 1 < len(slides) else slides[0]  # loop
        s["nextTransitionMs"] = nxt["transitionMs"]

    # Extract glyphs once per slide (cached in memory).
    # Dedup against expected character count: count [a-z] in the slide
    # text; if FLUX produced more glyphs than expected, drop the
    # smallest ones (typically apostrophe ghosts / texture artifacts).
    print(f"→ extracting glyphs from {len(slides)} word images…")
    glyph_cache = []
    for i, slide in enumerate(slides):
        path = os.path.join(args.img_dir, slide["image"])
        try:
            glyphs = extract_glyphs(path)
        except Exception as e:
            print(f"  ⚠ slide {i} '{slide['text']}' glyph extract failed: {e}")
            glyphs = []
        expected_n = len("".join(c for c in slide["text"].lower() if c.isalpha()))
        if expected_n > 0 and len(glyphs) > expected_n:
            # Sort by area, keep the LARGEST `expected_n` (preserve order)
            sized = [(idx, g, g["w"] * g["h"]) for idx, g in enumerate(glyphs)]
            sized.sort(key=lambda x: -x[2])
            keep_idxs = sorted(s[0] for s in sized[:expected_n])
            glyphs = [glyphs[k] for k in keep_idxs]
        target_rgb = color_rgb(slide.get("letterColor", "black"))
        # Texture-preserving recolor: blend FLUX's source RGB toward
        # target_rgb at 70% — letters end up predominantly the slide's
        # letter color but retain ~30% of the source's intensity/hue
        # variation, so internal grayscale texture survives instead of
        # being flattened to a solid block.
        BLEND_TO_TARGET = 0.70
        for g in glyphs:
            arr = np.array(g["img"])
            mask = arr[:, :, 3]
            src = arr[:, :, :3].astype(np.float32)
            arr[:, :, 0] = (src[:, :, 0] * (1 - BLEND_TO_TARGET) + target_rgb[0] * BLEND_TO_TARGET).astype(np.uint8)
            arr[:, :, 1] = (src[:, :, 1] * (1 - BLEND_TO_TARGET) + target_rgb[1] * BLEND_TO_TARGET).astype(np.uint8)
            arr[:, :, 2] = (src[:, :, 2] * (1 - BLEND_TO_TARGET) + target_rgb[2] * BLEND_TO_TARGET).astype(np.uint8)
            arr[:, :, 3] = mask
            g["img"] = Image.fromarray(arr, "RGBA")
        glyph_cache.append(glyphs)
        print(f"  slide {i:2d} '{slide['text']}' → {len(glyphs)} glyphs (expected {expected_n})")

    # Audio amplitude
    print("→ extracting audio amplitude…")
    amp = amplitude_curve(args.audio, total_dur, fps)

    # Render frames
    Path(args.frames_dir).mkdir(parents=True, exist_ok=True)
    n_frames = int(total_dur * fps)
    print(f"→ rendering {n_frames} frames @ {fps}fps to {args.frames_dir}…")

    KERN_SPACING = 4       # air between letters within a word
    BASE_SCALE = 0.85      # bigger now since one word per screen
    FOCUS_BOOST = 1.18     # spotlight only mildly above general
    LANE_W = int(W * 1.85) # wider lane spacing so during slide-in
                            # transitions, slide_i scrolls fully off
                            # left BEFORE slide_(i+1) enters from
                            # right — eliminates the "two words at
                            # once" doubling. Was W (one word per
                            # screen width) which guaranteed overlap
                            # at the transition midpoint.
    MIN_PAD = 80           # generous bg-color padding around each word
    # MAX_WORD_W must account for FOCUS_BOOST so even 7-letter words
    # fit within the viewport at peak spotlight scale, with comfortable
    # air on both sides for the dash + dynamic edges.
    SIDE_AIR_PX = 200
    MAX_WORD_W = int((LANE_W - 2 * SIDE_AIR_PX) / FOCUS_BOOST)
    TARGET_WORD_H = int(H * 0.10)    # ~192px tall — one-word focus
    PEEK_PX = int(W * 0.14)          # ~150px of next word peeks in by hold-end
    CONSUME_ZONE = 0.13              # just the leftmost ~13% — only letters
                                     # actually about to scroll off the
                                     # screen disintegrate. Anything wider
                                     # eats into the active word's first
                                     # letter during slide-in transitions
                                     # and makes the active syllable look
                                     # faded/glitched while still being sung.
    LOOKAHEAD_ZONE = 0.82

    # Per-word display scale — height-normalized, then capped to fit
    # MAX_WORD_W so every word sits comfortably inside its fixed lane.
    per_word_scales = []
    per_word_widths = []
    per_word_baselines = []  # source-image y position of each word's baseline
    for glyphs in glyph_cache:
        if not glyphs:
            per_word_scales.append(BASE_SCALE)
            per_word_widths.append(180)
            per_word_baselines.append(0)
            continue
        max_h = max(g["h"] for g in glyphs)
        h_norm = TARGET_WORD_H / max(1, max_h)
        s_h = h_norm * BASE_SCALE
        natural_w = sum(g["w"] for g in glyphs) + KERN_SPACING * max(0, len(glyphs) - 1)
        s_w = MAX_WORD_W / max(1, natural_w)
        s = min(s_h, s_w)
        per_word_scales.append(s)
        per_word_widths.append(int(natural_w * s))
        # Baseline = bottom of the tallest glyph (in source-image coords).
        # Used for vertical alignment so letters share a real baseline.
        per_word_baselines.append(max(g["y1"] for g in glyphs))

    # Lane geometry: fixed lane width = W / LANES_VISIBLE so the
    # viewport always shows ~3 full lanes (4 momentarily during transitions).
    lane_widths = [LANE_W] * len(slides)
    lane_starts = [0.0]
    for lw in lane_widths:
        lane_starts.append(lane_starts[-1] + lw)
    total_lane_w = lane_starts[-1]

    # Spotlight peak: only the lane within ±half a lane-width of screen
    # center boosts above 1.0; everything else stays at uniform general
    # (BASE) scale. Sharper peak so the focused word is unambiguously
    # the spotlight.
    def focus_factor(lane_center_screen, screen_center_x, falloff_px):
        d = abs(lane_center_screen - screen_center_x)
        if d >= falloff_px:
            return 1.0
        ratio = d / falloff_px
        return 1.0 + (FOCUS_BOOST - 1.0) * (0.5 + 0.5 * np.cos(np.pi * ratio))

    # Entry scale: when a lane is past the LOOKAHEAD_ZONE on the right,
    # render it smaller to telegraph "this word is on deck". As it slides
    # in, it grows to general scale before reaching the spotlight peak.
    def entry_scale(lane_center_screen, screen_w):
        threshold = screen_w * LOOKAHEAD_ZONE
        if lane_center_screen <= threshold:
            return 1.0
        out_x = screen_w * 1.05
        if lane_center_screen >= out_x:
            return 0.45
        ratio = (lane_center_screen - threshold) / max(1.0, out_x - threshold)
        eased = 0.5 - 0.5 * np.cos(np.pi * ratio)
        return 1.0 - 0.55 * eased

    # Demoscene consume effect: when a glyph drifts into the leftmost
    # CONSUME_ZONE of the screen, slice it into horizontal raster bands,
    # offset each band chaotically, fade alpha, chromatic-aberrate the
    # color channels. consume_factor 0..1 (0 = intact, 1 = gone).
    def consume_factor_for(x_screen, screen_w):
        threshold = screen_w * CONSUME_ZONE
        if x_screen >= threshold:
            return 0.0
        if x_screen <= -screen_w * 0.05:
            return 1.0
        # Smooth ramp 0..1 as x moves from threshold → -W*0.05
        return min(1.0, max(0.0,
                            (threshold - x_screen) / (threshold + screen_w * 0.05)))

    consume_rng = np.random.default_rng(0xACE51)
    def apply_consume(glyph_img, cf, seed):
        if cf <= 0.001:
            return glyph_img
        if cf >= 0.999:
            return None  # fully consumed
        arr = np.array(glyph_img)
        h, w, _ = arr.shape
        out = np.zeros_like(arr)
        slice_h = max(2, h // 18)
        max_offset = int(w * cf * 1.6 + 12)
        # Per-slice deterministic offsets (seeded by glyph index + slice y)
        for y in range(0, h, slice_h):
            ye = min(h, y + slice_h)
            srng = np.random.default_rng((seed * 9176 + y * 7919) & 0xFFFFFFFF)
            offset = int(srng.uniform(-1, 1) * max_offset)
            offset = max(-w + 1, min(w - 1, offset))
            if offset >= 0:
                out[y:ye, offset:w, :] = arr[y:ye, 0:w - offset, :]
            else:
                out[y:ye, 0:w + offset, :] = arr[y:ye, -offset:w, :]
        # Chromatic aberration: shift R left, B right by cf*4 px
        if cf > 0.15:
            shift = max(1, int(cf * 6))
            r = np.zeros((h, w), dtype=np.uint8)
            b = np.zeros((h, w), dtype=np.uint8)
            r[:, :w - shift] = out[:, shift:w, 0]
            b[:, shift:w] = out[:, :w - shift, 2]
            out[:, :, 0] = r
            out[:, :, 2] = b
        # Alpha fade
        fade = max(0.0, 1.0 - cf)
        out[:, :, 3] = (out[:, :, 3].astype(np.float32) * fade).astype(np.uint8)
        return Image.fromarray(out, "RGBA")

    def fill_lane_bg(img, slide, lane_screen_x, lane_w):
        # Now a no-op — the gradient bg strip computed once per frame
        # supersedes per-lane fills (so lane boundaries blend smoothly
        # rather than hard-cutting between bg colors).
        pass

    BG_BLEND_PX = 240  # width of the transition zone between adjacent lanes
    # Pre-compute lane bg colors once (immutable across frames)
    bg_colors_arr = np.array(
        [color_rgb(s.get("bgColor", "black")) for s in slides],
        dtype=np.float32,
    )

    def compute_bg_strip(x_cam_v):
        """Return a (W, 3) uint8 array — the bg color for each viewport
        column, with cosine-eased gradients across each lane boundary."""
        n = len(slides)
        half = BG_BLEND_PX / 2.0
        xs = np.arange(W)
        world_xs = xs + x_cam_v
        rel_xs = world_xs % total_lane_w
        lane_idx = (rel_xs // LANE_W).astype(np.int64) % n
        in_lane_x = rel_xs - lane_idx * LANE_W
        cur_c = bg_colors_arr[lane_idx]
        prev_c = bg_colors_arr[(lane_idx - 1) % n]
        next_c = bg_colors_arr[(lane_idx + 1) % n]
        bg_out = cur_c.copy()
        # Right blend: in_lane_x in [LANE_W - half, LANE_W)
        rmask = in_lane_x >= (LANE_W - half)
        if rmask.any():
            t = (in_lane_x[rmask] - (LANE_W - half)) / BG_BLEND_PX  # 0..0.5
            eased = (0.5 - 0.5 * np.cos(np.pi * t))[:, np.newaxis]
            bg_out[rmask] = (1 - eased) * cur_c[rmask] + eased * next_c[rmask]
        # Left blend: in_lane_x in [0, half)
        lmask = in_lane_x < half
        if lmask.any():
            t = 0.5 + in_lane_x[lmask] / BG_BLEND_PX  # 0.5..1.0
            eased = (0.5 - 0.5 * np.cos(np.pi * t))[:, np.newaxis]
            bg_out[lmask] = (1 - eased) * prev_c[lmask] + eased * cur_c[lmask]
        return bg_out.clip(0, 255).astype(np.uint8)

    def paint_lane_glyphs(img, glyphs, lane_screen_x, lane_w,
                          word_scale, focus_mult, baseline_src,
                          per_char_zoom=None, per_char_bounce=None,
                          slide_idx=0, suppress_consume=False,
                          per_char_rotation=None, per_char_x_wiggle=None):
        if not glyphs:
            return
        n_chars = len(glyphs)
        s = word_scale * focus_mult
        scaled_widths = []
        scaled_heights = []
        # below_baseline_src[i] = how far below baseline this glyph extends
        # in the SOURCE image (0 for letters touching baseline; positive
        # for descenders). top_offset_src = how high above baseline.
        below_src = []
        top_offset_src = []
        for ci, g in enumerate(glyphs):
            cz = per_char_zoom[ci] if per_char_zoom else 1.0
            scaled_widths.append(max(1, int(g["w"] * s * cz)))
            scaled_heights.append(max(1, int(g["h"] * s * cz)))
            below_src.append(max(0, baseline_src - g["y1"]))
            top_offset_src.append(max(0, g["y1"] - g["y0"]))
        total_w = sum(scaled_widths) + KERN_SPACING * max(0, n_chars - 1)
        lane_center_screen = lane_screen_x + lane_w / 2.0
        base_x = int(lane_center_screen - total_w / 2)
        # Baseline: place the row near vertical center of screen.
        baseline_y = H // 2 + scaled_heights[0] // 4  # nudge so top isn't clipped
        x_cur = base_x
        for ci, g in enumerate(glyphs):
            gw = scaled_widths[ci]
            gh = scaled_heights[ci]
            # LANCZOS gives clean letterforms; with MAX_ZOOM=1.0 the
            # scale is constant per slide so there's no frame-to-frame
            # shimmer. We then HARD-BINARIZE the alpha because LANCZOS
            # turns the binary mask into fractional values, which would
            # otherwise let bg color bleed through letter centers.
            if (gw, gh) != (g["w"], g["h"]):
                glyph_img = g["img"].resize((gw, gh), Image.LANCZOS)
                arr_g = np.array(glyph_img)
                arr_g[:, :, 3] = np.where(arr_g[:, :, 3] > 128, 255, 0).astype(np.uint8)
                glyph_img = Image.fromarray(arr_g, "RGBA")
            else:
                glyph_img = g["img"]
            bnc = per_char_bounce[ci] if per_char_bounce else 0
            xwig = per_char_x_wiggle[ci] if per_char_x_wiggle else 0
            rot_deg = per_char_rotation[ci] if per_char_rotation else 0
            # Per-character rotation. PIL rotates around the image
            # center, so the bbox grows; expand=True so corners aren't
            # clipped, then we recenter on the original glyph anchor.
            if abs(rot_deg) > 0.1:
                pre_w, pre_h = glyph_img.size
                glyph_img = glyph_img.rotate(rot_deg, resample=Image.BICUBIC, expand=True)
                # Re-binarize alpha after bicubic to keep edges crisp.
                arr_g = np.array(glyph_img)
                arr_g[:, :, 3] = np.where(arr_g[:, :, 3] > 96, 255, 0).astype(np.uint8)
                glyph_img = Image.fromarray(arr_g, "RGBA")
                rot_dx = (glyph_img.size[0] - pre_w) // 2
                rot_dy = (glyph_img.size[1] - pre_h) // 2
            else:
                rot_dx = rot_dy = 0
            # Glyph bottom relative to baseline = below_src * s (descender)
            # so glyph's bottom in screen = baseline_y + below_src*s.
            # For most letters below_src = 0, so bottom = baseline_y.
            glyph_bottom = baseline_y - int((baseline_src - g["y1"]) * s) + bnc
            gy = glyph_bottom - gh - rot_dy
            # Per-glyph consume effect: only fires once a slide is in
            # the past (its word being sung is over). Suppressed on the
            # current/active slide so words don't break up while sung.
            if not suppress_consume:
                cf = consume_factor_for(x_cur, W)
                if cf > 0.001:
                    glyph_img = apply_consume(glyph_img, cf, slide_idx * 1000 + ci)
                    if glyph_img is None:
                        x_cur += gw + KERN_SPACING
                        continue
            img.paste(glyph_img, (int(x_cur + xwig - rot_dx), gy), glyph_img)
            x_cur += gw + KERN_SPACING

    for f in range(n_frames):
        t = f / fps
        amp_now = amp[min(f, len(amp) - 1)]

        cur_idx = 0
        for i, s in enumerate(slides):
            if s["start"] <= t < s["end"]:
                cur_idx = i
                break

        x_cam = camera_x(t, slides, lane_starts, lane_widths, W, PEEK_PX,
                         loop_end_t=(n_frames - 1) / fps)
        screen_center = W / 2.0
        # Tight falloff: spotlight only the lane near center; words
        # peeking in from the right stay at uniform general scale.
        falloff = LANE_W * 0.30

        # Build the gradient bg strip (W×3) for this frame and tile
        # it vertically to fill the canvas — replaces per-lane fills.
        bg_strip = compute_bg_strip(x_cam)
        img_arr = np.broadcast_to(bg_strip[np.newaxis, :, :],
                                  (H, W, 3)).copy()
        img = Image.fromarray(img_arr, "RGB")

        # Two-pass: bgs first (all colored strips), then glyphs (so the
        # focused word's overflow into neighbor lanes stays visible
        # instead of being painted over by the next lane's bg).
        n = len(slides)
        visible = []
        for loop_off in (-1, 0, 1):
            world_shift = loop_off * total_lane_w
            for i in range(n):
                lane_screen_x = lane_starts[i] + world_shift - x_cam
                if lane_screen_x >= W or lane_screen_x + lane_widths[i] <= 0:
                    continue
                visible.append((i, loop_off, lane_screen_x))

        # Pass 1: all bg strips
        for i, _, lane_screen_x in visible:
            fill_lane_bg(img, slides[i], lane_screen_x, lane_widths[i])

        # Pass 2: all glyphs, focused word last so it sits on top
        # if it overlaps neighbors. Sort: non-active first, active last.
        visible_glyph_pass = sorted(
            visible,
            key=lambda v: 1 if (v[0] == cur_idx and v[1] == 0) else 0,
        )
        for i, loop_off, lane_screen_x in visible_glyph_pass:
            slide_real = slides[i]
            glyphs = glyph_cache[i]
            lane_center_screen = lane_screen_x + lane_widths[i] / 2.0
            f_mult = focus_factor(lane_center_screen, screen_center, falloff)
            e_mult = entry_scale(lane_center_screen, W)
            scale_mult = f_mult * e_mult
            baseline_src = per_word_baselines[i]
            # Suppress consume on the CURRENT slide (still being sung)
            # AND on the next slide on its way in. Past slides (i < cur)
            # are the only ones eligible to break apart on their way out.
            is_past = (i < cur_idx and loop_off == 0) or loop_off < 0
            suppress = not is_past
            if i == cur_idx and loop_off == 0 and glyphs:
                n_chars = len(glyphs)
                zooms, bounces, rots, xwigs = [], [], [], []
                for ci in range(n_chars):
                    em = char_emphasis(t, slide_real, ci, n_chars, amp_now)
                    zooms.append(char_zoom(em))
                    bounces.append(char_bounce_y(em))
                    rots.append(char_rotation(em, t, ci))
                    xwigs.append(char_x_wiggle(em, t, ci))
                paint_lane_glyphs(img, glyphs, lane_screen_x, lane_widths[i],
                                  per_word_scales[i], scale_mult, baseline_src,
                                  zooms, bounces, slide_idx=i,
                                  suppress_consume=suppress,
                                  per_char_rotation=rots, per_char_x_wiggle=xwigs)
            else:
                paint_lane_glyphs(img, glyphs, lane_screen_x, lane_widths[i],
                                  per_word_scales[i], scale_mult, baseline_src,
                                  slide_idx=i + loop_off * 1000,
                                  suppress_consume=suppress)

        # ── Em-dash waveform connectors at every gap ────────────────
        # Each gap (between consecutive slides) has its own dash at a
        # fixed WORLD position (lane_starts[i+1]) — so as the camera
        # scrolls, the dashes scroll with it, sliding all the way off
        # screen left rather than disappearing at the edge. At any
        # frame, the viewport-clip filter naturally hides off-screen
        # gaps; usually one dash is visible (the gap to the right of
        # the centered word) with another sometimes entering/leaving.
        n_slides = len(slides)
        midline_y = H // 2 - int(TARGET_WORD_H * 0.30)
        for i in range(n_slides):
            for loop_off in (-1, 0, 1):
                gap_world_x = (lane_starts[i + 1]
                               + loop_off * total_lane_w)
                gap_screen_x = gap_world_x - x_cam
                if gap_screen_x < -DASH_HALF_SPAN_PX or gap_screen_x > W + DASH_HALF_SPAN_PX:
                    continue
                cur_letter = color_rgb(slides[i].get("letterColor", "white"))
                nxt = slides[(i + 1) % n_slides]
                nxt_letter = color_rgb(nxt.get("letterColor", "white"))
                _draw_waveform_dash(
                    img, gap_screen_x, midline_y,
                    cur_letter, nxt_letter,
                    amp, f, fps, W, H,
                )

        # ── Whole-screen-left disintegration ────────────────────────
        # Glitchy demoscene-style fade in the leftmost CONSUME_ZONE:
        #   1. Pixelation (aggressive downscale + NEAREST upscale)
        #   2. Horizontal raster slice break (per-band x-shift)
        #   3. Chromatic aberration (R/B channels split)
        #   4. Cosine-eased alpha fade to bg gradient
        zone_w = int(W * CONSUME_ZONE)
        # PIX_DIV-snap: round UP to a multiple of PIX_DIV so the
        # downscale-then-upscale always lands at exactly zone_w (the
        # pixelation step otherwise gives pw*PIX_DIV which can be
        # smaller than zone_w, creating shape mismatches downstream).
        PIX_DIV = 6
        if zone_w > 0:
            zone_w = ((zone_w + PIX_DIV - 1) // PIX_DIV) * PIX_DIV
            arr = np.array(img)
            arr_orig = arr[:, :zone_w, :].copy()  # untouched left zone
            zone = arr_orig.copy()

            # 1. PIXELATION: downscale via BOX (anti-aliased average)
            #    then NEAREST upscale for chunky pixels.
            # PIX_DIV is defined above (before zone_w snap).
            pw, ph = max(1, zone_w // PIX_DIV), max(1, H // PIX_DIV)
            zone_img = Image.fromarray(zone)
            small = zone_img.resize((pw, ph), Image.BOX)
            big = small.resize((pw * PIX_DIV, ph * PIX_DIV), Image.NEAREST)
            zone = np.array(big)[:H, :zone_w].copy()

            # 2. RASTER SLICE BREAK: per 14-row band, random horizontal
            #    shift up to ±zone_w*0.12. Frame-deterministic seed so
            #    glitch evolves chaotically across frames.
            slice_h = 14
            rng = np.random.default_rng((f * 9173) & 0xFFFFFFFF)
            max_shift = int(zone_w * 0.12)
            out_zone = zone.copy()
            for y in range(0, H, slice_h):
                ye = min(H, y + slice_h)
                shift = int(rng.uniform(-1, 1) * max_shift)
                if shift > 0 and shift < zone_w:
                    out_zone[y:ye, shift:, :] = zone[y:ye, :zone_w - shift, :]
                    out_zone[y:ye, :shift, :] = bg_strip[np.newaxis, :shift, :]
                elif shift < 0 and -shift < zone_w:
                    out_zone[y:ye, :zone_w + shift, :] = zone[y:ye, -shift:, :]
                    fill_x_start = zone_w + shift
                    if fill_x_start < zone_w:
                        out_zone[y:ye, fill_x_start:, :] = (
                            bg_strip[np.newaxis, fill_x_start:zone_w, :]
                        )
            zone = out_zone

            # 3. CHROMATIC ABERRATION: shift R left, B right by 5px
            aberr = 5
            if zone_w > aberr * 2:
                chroma = zone.copy()
                chroma[:, :zone_w - aberr, 0] = zone[:, aberr:, 0]
                chroma[:, aberr:, 2] = zone[:, :zone_w - aberr, 2]
                zone = chroma

            # ── Smooth ramp from clean → glitched → bg ─────────────
            # cf: 0 at right edge of zone, 1 at far left
            cf = 1.0 - (np.arange(zone_w) / zone_w)
            # Glitch ramp: cubic so glitch ramps in slowly near the
            # right edge of the zone (cf small → cubic small) and only
            # fully kicks in toward the far left. Eliminates the
            # "harsh start" the user pointed out.
            glitch_ramp = (cf ** 2.2)[np.newaxis, :, np.newaxis]
            # Blend ORIGINAL ↔ GLITCHED:
            blended = (
                arr_orig.astype(np.float32) * (1 - glitch_ramp)
                + zone.astype(np.float32) * glitch_ramp
            )
            # Bg fade: cosine-eased, separate ramp (less aggressive
            # since glitch already does most of the visual work)
            bg_ramp = (0.5 - 0.5 * np.cos(np.pi * cf)) * 0.85
            bg_ramp = bg_ramp[np.newaxis, :, np.newaxis]
            bg_zone = bg_strip[:zone_w][np.newaxis, :, :].astype(np.float32)
            arr[:, :zone_w, :] = (
                blended * (1 - bg_ramp) + bg_zone * bg_ramp
            ).astype(np.uint8)
            img = Image.fromarray(arr, "RGB")

        img.save(os.path.join(args.frames_dir, f"f{f:05d}.png"))
        if f % 120 == 0:
            print(f"  {f}/{n_frames} ({100*f/n_frames:.0f}%)")

    print(f"✓ {n_frames} frames written")

if __name__ == "__main__":
    main()
