#!/opt/homebrew/Cellar/instaloader/4.15.1_1/libexec/bin/python3
"""
Generate "jeffrey + art collectors" marketing images via OpenAI gpt-image-2,
conditioned on the full 8-ref jeffrey identity set (3 shoot hero shots + 5 IG
selfies across 2017..2025) so identity holds across the batch.

Eight distinct scenes — peer-horizontal, real-photo iphone-snapshot tone, no
cult-leader framing, plain green Citrus MacBook Neo or plain ThinkPad only,
shirt prints are valid AC piece names. Saves all PNGs to ~/Desktop.

Run:
  ./generate-collectors.py            # all 8 variants
  ./generate-collectors.py --variant art-fair-booth
"""

from __future__ import annotations

import argparse
import base64
import io
import sys
import tempfile
from datetime import datetime
from pathlib import Path

from openai import OpenAI
from PIL import Image

# OpenAI image-edit endpoint limit is ~25MB/image and the SDK's default httpx
# timeout chokes well before that on multi-image edits. Cap each ref at 1280px
# on the long edge, JPEG q=90 — plenty for identity preservation.
MAX_REF_EDGE = 1280
MAX_REF_BYTES = 1_500_000  # only resize if larger than this

REPO_ROOT = Path(__file__).resolve().parent.parent.parent.parent
VAULT_ENV = REPO_ROOT / "aesthetic-computer-vault" / ".devcontainer" / "envs" / "devcontainer.env"
SHOOT_DIR = REPO_ROOT / "portraits" / "jeffrey" / "corpus" / "shoot"
ARCHIVE_DIR = REPO_ROOT / "portraits" / "jeffrey" / "ig-archive" / "whistlegraph"
DESKTOP = Path.home() / "Desktop"

# 8 references → strong identity preservation across eras + angles.
REFS = [
    SHOOT_DIR / "jeffery-av--07.jpg",
    SHOOT_DIR / "jeffery-av--01.jpg",
    SHOOT_DIR / "jeffery-av--04.jpg",
    ARCHIVE_DIR / "2017-04-10_BStid5yjTHq.jpg",
    ARCHIVE_DIR / "2018-12-02_Bq4ckGFFNtW.jpg",
    ARCHIVE_DIR / "2020-09-02_CEpxlO2FOvD.jpg",
    ARCHIVE_DIR / "2021-07-10_CRI095Vl7AO_1.jpg",
    ARCHIVE_DIR / "2025-01-25_DFQ2lHPzN_W.jpg",
]

IDENTITY = (
    "the man in the reference photos: medium-length brown hair, thin frame, "
    "real skin texture, the same face across all eight refs. recognizably him. "
    "do not youthify, smooth, or ai-prettify. he wears a real outfit — a "
    "button-down, a printed tee, a cardigan, a sweater, or a flannel. never a "
    "tank top, never a costume."
)

PEER_RULE = (
    "peer-horizontal composition. jeffrey is one figure among equals — the "
    "collectors stand or sit at the same eye-line. nobody is centered as the "
    "leader. ensemble framing, like a candid photo a friend took mid-conversation. "
    "do not stage him as a guru, lecturer, or sole subject. one focal arrangement, "
    "not multiple tables."
)

LAPTOP_RULE = (
    "if a laptop appears it is either a plain citrus chartreuse green macbook "
    "neo (the regular apple logo, no stickers, no butterfly, no cards pinned to "
    "the lid) or a plain matte black thinkpad. the back of the lid is plain — "
    "matte chartreuse with the small standing apple logo, or matte black with "
    "only the small lenovo wordmark and thinkpad logo. no textures, no faint "
    "imagery, no decals on the lid back. and screen physics is coherent: if the "
    "camera sees the screen content, the back of the lid is hidden behind the "
    "device; if the camera sees the back of the lid, the screen content is not "
    "visible (only its glow). never both at once. the screen shows real varied "
    "content — a terminal, a paint app, code, audio scopes — never a thumbnail "
    "of the surrounding scene."
)

VOICE = (
    "real photograph, iphone-snapshot candid, deadpan-warm. not cinematic, not "
    "neon-noir, not glossy, not corporate stock. light comes from a real source "
    "in the scene — a window, an overhead bulb, a desk lamp, the laptop screen. "
    "leave the top quarter of the frame quiet so a title can sit there."
)

VARIANTS = [
    {
        "name": "collectors-studio-visit",
        "prompt": f"""\
candid iphone photo of {IDENTITY} in his small home studio with two art collectors visiting — \
a woman in her 50s in a neat linen blazer and a man in his 40s in a charcoal sweater. they are \
standing together in front of a wall of hand-printed aesthetic.computer pixel-art prints taped \
up with masking tape. jeffrey wears a faded blue button-down with the chest-print "stamp" in \
small clean type. he is mid-sentence, gesturing at one print with an open hand, eyebrows up in \
that "look at this one" expression. the woman leans in, the man laughs. a folding table beside \
them holds a stack of papers and a single coffee mug. afternoon window light from camera-left.

{PEER_RULE}
{LAPTOP_RULE}
{VOICE}
framing: tall vertical 9:16 instagram story, all three figures fully in frame, full-figure standing-room above and below.""",
    },
    {
        "name": "collectors-wall-hang",
        "prompt": f"""\
candid iphone photo of {IDENTITY} standing on a low step stool inside a collector's apartment, \
adjusting a small framed aesthetic.computer print on a pale plaster wall. a woman in her 60s — \
the collector — stands two steps back with her arms folded, head tilted, smiling. jeffrey wears \
a soft mustard cardigan over a tee that prints the word "line" across the chest. he holds a \
small bubble level against the frame, looking down at it with squinted concentration. the room \
is warm and lived-in: a worn velvet couch, a side table with a glass of water, a low brass lamp \
casting orange light up the wall.

{PEER_RULE}
{LAPTOP_RULE}
{VOICE}
framing: tall vertical 9:16 instagram story, both figures full-figure in frame with breathing room above the head.""",
    },
    {
        "name": "collectors-flat-files",
        "prompt": f"""\
candid iphone photo of {IDENTITY} in a small print storage room, leaning against a wide metal \
flat-file cabinet with the top drawer pulled open. across from him, two art collectors — a \
40-something man in a navy field jacket and a 30-something woman in a plaid flannel — flip \
through a stack of aesthetic.computer prints in the open drawer. jeffrey holds a paper cup of \
coffee, eyebrows raised in an open "go ahead, take your time" face. he wears a pale blue \
oxford shirt, sleeves rolled, the chest-print reading "paste" in small type. an overhead \
warehouse bulb throws soft yellow light. concrete floor, cardboard tubes in a corner.

{PEER_RULE}
{LAPTOP_RULE}
{VOICE}
framing: tall vertical 9:16 instagram story, all three figures full-figure in frame.""",
    },
    {
        "name": "collectors-art-fair-booth",
        "prompt": f"""\
candid iphone photo of {IDENTITY} behind a small folding table at a low-key art fair, with two \
art collectors in front of the table browsing. on the table: neat stacks of aesthetic.computer \
papers, a small wooden crate of zines, a tape dispenser, a cup of pens. a hand-lettered card on \
the table reads "aesthetic.computer". jeffrey wears a dark green flannel over a tee printing \
"chord". he is laughing — head tilted slightly back, real laugh, eyes crinkled — at something \
the older collector (a woman in a wide-brim felt hat) just said. her partner, a tall man in a \
denim jacket, smiles and picks up a paper. fluorescent ceiling light, polished concrete floor, \
neighboring booths blurred behind.

{PEER_RULE}
{LAPTOP_RULE}
{VOICE}
framing: tall vertical 9:16 instagram story, table and all three figures full-figure in frame.""",
    },
    {
        "name": "collectors-cafe-laptop",
        "prompt": f"""\
candid iphone photo of {IDENTITY} sitting across a small cafe table from a single art collector \
— a 50-something man in a worn corduroy blazer, gray beard. between them, a plain citrus \
chartreuse green macbook neo (the regular apple logo, no stickers) is open and turned slightly \
toward the collector; the screen shows an aesthetic.computer paint piece in progress with a \
green-on-black terminal pane on the right. two espresso cups, a saucer with a half-eaten \
biscotti. jeffrey is mid-explanation, one hand hovering over the trackpad, eyebrows up in the \
"watch this" expression. he wears a navy-and-white striped button-down, chest-print "tape" in \
small letters. the collector leans in, eyes on the screen, half-smiling.

{PEER_RULE}
{LAPTOP_RULE}
{VOICE}
framing: tall vertical 9:16 instagram story, both figures and the laptop fully in frame from the side, \
warm window light from camera-left.""",
    },
    {
        "name": "collectors-catalog-session",
        "prompt": f"""\
candid iphone photo of {IDENTITY} kneeling on a clean white tarp on the floor of a small \
studio, photographing a flat aesthetic.computer print with his iphone held in both hands, head \
down, concentrating with that flat deadpan focus face. an art collector — a woman in her 40s in \
a black turtleneck and round glasses — sits cross-legged on the floor across from him with a \
clipboard, mid-laugh at something he just said while still focused on his shot. jeffrey wears a \
mustard-and-cream printed tee with the word "arena" in big bold lowercase type. natural \
overhead daylight from a skylight, soft shadows.

{PEER_RULE}
{LAPTOP_RULE}
{VOICE}
framing: tall vertical 9:16 instagram story, high three-quarter overhead, both figures and the print on the floor fully in frame.""",
    },
    {
        "name": "collectors-doorway-delivery",
        "prompt": f"""\
candid iphone photo of {IDENTITY} on a brownstone doorstep handing off a brown-paper-wrapped \
flat package — clearly a wrapped art print — to an art collector who has just opened the front \
door. the collector is a middle-aged man in a soft navy wool sweater, beaming with that real \
"oh excellent, finally" look, both hands reaching for the package. jeffrey wears a charcoal \
hoodie over a tee that prints "notepat" across the chest. he is mid-handoff, leaning slightly \
forward, smiling a small private smile. afternoon sun from camera-right, long warm shadow on \
the porch wood. a canvas tote of more papers slung over his shoulder.

{PEER_RULE}
{LAPTOP_RULE}
{VOICE}
framing: tall vertical 9:16 instagram story, both figures and the package full-figure in frame.""",
    },
    {
        "name": "collectors-gallery-opening",
        "prompt": f"""\
candid iphone photo of {IDENTITY} at a small relaxed gallery opening, standing in a casual \
loose group of three art collectors with plastic cups of wine. behind them, a single \
aesthetic.computer pixel-art print is mounted on a clean white wall, modestly lit by a single \
track-light. jeffrey wears a cream cable-knit sweater over a tee printing "sotce" in tiny \
letters; he is mid-conversation with his hands in his pockets, doing a small smug-detective \
half-smile at something the collector across from him just said. the collectors — a young \
woman in a black blazer, an older man with silver glasses, and a 30-something in a brown \
corduroy jacket — are mid-laugh, mid-sip. background is softly out of focus: a few other \
clusters of opening-night people. warm tungsten light overhead.

{PEER_RULE}
{LAPTOP_RULE}
{VOICE}
framing: tall vertical 9:16 instagram story, all four figures full-figure in frame including jeffrey, \
the print partly visible behind.""",
    },
    {
        "name": "collectors-aoc-thinkpad",
        "prompt": f"""\
candid press-pool iphone photo of {IDENTITY} standing one step behind u.s. representative \
alexandria ocasio-cortez at a small podium-style press event — she is at the lectern \
mid-sentence, one hand gesturing, recognizable appearance: dark wavy hair pulled back, gold \
hoop earrings, deep-red blazer with a white collar peeking through. jeffrey is half a step \
behind her right shoulder, standing at a small chest-high side-table with an open plain \
matte-black thinkpad sitting flat on it. jeffrey is leaning over the laptop, both hands on \
the keyboard, mid-type, deadpan-focused. CRITICAL ORIENTATION: the camera is in front of \
jeffrey, slightly off to one side. the laptop is open on the table with the screen tilted up \
TOWARD JEFFREY (away from the camera). the camera therefore sees: jeffrey's chest and face, \
his hands on the keyboard, and the OUTER BACK of the screen lid as a thin foreshortened \
matte-black rectangle (just a flat dark slab, with at most a tiny lenovo wordmark in one \
corner). NO text on the camera-facing back-of-screen surface. NO patterns, NO code visible, \
NO decals — it is a plain matte-black slab. the screen content is hidden from the camera, on \
the side facing jeffrey; we see only its faint cool glow on jeffrey's chin and the underside \
of his nose. jeffrey wears a plain charcoal blazer over a plain charcoal t-shirt (NO chest \
print, NO logo, NO words — leave the shirt completely blank so no text gets echoed elsewhere \
in the frame). jeffrey \
wears a charcoal blazer over a tee that prints "circle" across the chest. two press \
microphones at the lectern, a draped american flag in soft focus behind, warm overhead \
news-stage light.

{PEER_RULE}
{LAPTOP_RULE}
{VOICE}
framing: tall vertical 9:16 instagram story, both figures full-figure in frame, jeffrey one \
step back so they share roughly the same eye-line plane.""",
    },
    {
        "name": "collectors-trump-answering-machine",
        "prompt": f"""\
candid iphone photo of {IDENTITY} sitting at a polished wooden desk next to donald trump — \
recognizable appearance: orange-blonde hair swept back, navy suit, red satin tie, white \
shirt, slight furrow — both leaning forward over a vintage 1990s panasonic answering machine. \
the answering machine is a beige plastic deck with a small cassette-tape window, a tiny lcd \
screen, and chunky labeled rectangular buttons (PLAY, REC, GREETING). jeffrey is calmly \
pointing with his right index finger at the REC button, deadpan patient-teacher expression, \
eyebrows neutral, eyes on the device. trump is leaning closer to the machine, eyes narrowed \
in concentration, mouth pursed in a "now i see" expression. the cassette is half-ejected from \
the tape window. jeffrey wears a soft beige cardigan over a tee printing "tape" in small \
lowercase letters across the chest. on the desk: a yellow legal pad with handwritten notes, \
a black corded office phone with the handset off the cradle, two coffee mugs. warm afternoon \
light from a window with sheer white curtains.

{PEER_RULE}
{VOICE}
no laptop in this scene — just the vintage answering machine on the desk.
framing: tall vertical 9:16 instagram story, both figures full-figure seated side-by-side at \
the desk, the answering machine clearly visible between them, slight 3/4 angle from in front.""",
    },
    {
        "name": "collectors-reas-show-and-tell",
        "prompt": f"""\
candid iphone photo of {IDENTITY} sitting next to casey reas (the artist and co-founder of \
processing — recognizable: shaved head, round wire-rim glasses, plain dark button-down or \
sweater, calm professorial bearing) at a small wooden studio table. reas is the one with the \
laptop — a thin silver laptop turned slightly toward jeffrey — leaning forward and gesturing \
at his own screen mid-explanation. on reas's screen: a generative pattern in plain black ink \
on white, processing-style, hand-drawn-looking lines. jeffrey is surprised — eyebrows shot up \
high, mouth slightly open, eyes wide on reas's screen, leaning forward in his chair, real \
hyperbolic "oh whoa, wait" expression. he wears a soft red flannel over a tee printing \
"paint" in small letters. afternoon studio daylight from a window camera-left, soft shadows. \
two ceramic mugs, an open sketchbook, two pens on the table.

{PEER_RULE}
{VOICE}
the laptop here belongs to reas, not jeffrey — it is a plain silver thin laptop, no stickers, \
no jeffrey-branded chartreuse green. the screen shows real processing-style content, never a \
thumbnail of the surrounding scene.
framing: tall vertical 9:16 instagram story, both figures and the table full-figure in frame, \
slight 3/4 angle, equal eye-line.""",
    },
]


def pad_to_story(png_path: Path) -> None:
    """Pad PNG in-place to true 9:16 1080x1920 with letterbox bars matching
    the image's average edge color. Preserves the full source content."""
    img = Image.open(png_path).convert("RGB")
    w, h = img.size
    target_w, target_h = 1080, 1920
    # Scale to fit the 1080x1920 box, preserving source aspect.
    scale = min(target_w / w, target_h / h)
    new_w, new_h = int(w * scale), int(h * scale)
    img = img.resize((new_w, new_h), Image.LANCZOS)
    # Sample edge color for letterbox bars.
    edge = img.crop((0, 0, new_w, 1)).resize((1, 1)).getpixel((0, 0))
    canvas = Image.new("RGB", (target_w, target_h), edge)
    canvas.paste(img, ((target_w - new_w) // 2, (target_h - new_h) // 2))
    canvas.save(png_path, "PNG")


def shrink_ref(src: Path, work_dir: Path) -> Path:
    """Return src as-is if small; otherwise a downsampled JPEG copy in work_dir."""
    if src.stat().st_size <= MAX_REF_BYTES:
        return src
    img = Image.open(src)
    img = img.convert("RGB")
    w, h = img.size
    long_edge = max(w, h)
    if long_edge > MAX_REF_EDGE:
        scale = MAX_REF_EDGE / long_edge
        img = img.resize((int(w * scale), int(h * scale)), Image.LANCZOS)
    dst = work_dir / (src.stem + "_resized.jpg")
    img.save(dst, "JPEG", quality=90, optimize=True)
    return dst


def load_openai_key() -> str:
    import os
    if "OPENAI_API_KEY" in os.environ:
        return os.environ["OPENAI_API_KEY"]
    if VAULT_ENV.exists():
        for line in VAULT_ENV.read_text().splitlines():
            if line.startswith("OPENAI_API_KEY="):
                return line.split("=", 1)[1].strip().strip('"').strip("'")
    sys.exit("OPENAI_API_KEY not set and not in vault devcontainer.env")


def main() -> int:
    p = argparse.ArgumentParser()
    p.add_argument("--out", default=str(DESKTOP), help="output directory")
    p.add_argument("--size", default="1024x1536",
                   help="1024x1536 (default portrait, 9:16-padded post) / 1024x1024 / 1536x1024 / auto")
    p.add_argument("--story-pad", action="store_true", default=True,
                   help="post-pad each PNG to 1080x1920 (true 9:16 instagram story)")
    p.add_argument("--quality", default="high", choices=["low", "medium", "high", "auto"])
    p.add_argument("--variant",
                   choices=[v["name"] for v in VARIANTS] + ["all"], default="all")
    p.add_argument("--n", type=int, default=1, help="variants per prompt")
    p.add_argument("--model", default="gpt-image-2",
                   help="gpt-image-2 (default) | gpt-image-1.5 | gpt-image-1")
    args = p.parse_args()

    refs = [Path(r).expanduser().resolve() for r in REFS]
    for r in refs:
        if not r.exists():
            sys.exit(f"reference not found: {r}")
    out_dir = Path(args.out).expanduser().resolve()
    out_dir.mkdir(parents=True, exist_ok=True)

    work_dir = Path(tempfile.mkdtemp(prefix="collector-refs-"))
    refs = [shrink_ref(r, work_dir) for r in refs]
    total_kb = sum(r.stat().st_size for r in refs) // 1024
    print(f"upload payload: {total_kb} KB across {len(refs)} refs", file=sys.stderr)

    selected = (VARIANTS if args.variant == "all"
                else [v for v in VARIANTS if v["name"] == args.variant])
    timestamp = datetime.now().strftime("%Y-%m-%d_%H%M%S")

    client = OpenAI(api_key=load_openai_key(), timeout=600.0, max_retries=2)
    print(f"references ({len(refs)}): {', '.join(r.name for r in refs)}", file=sys.stderr)
    print(f"out: {out_dir}", file=sys.stderr)
    print(f"settings: size={args.size} quality={args.quality} n={args.n} model={args.model}",
          file=sys.stderr)

    for variant in selected:
        print(f"\ngenerating {variant['name']}…", file=sys.stderr)
        try:
            files = [open(r, "rb") for r in refs]
            try:
                response = client.images.edit(
                    model=args.model,
                    image=files,
                    prompt=variant["prompt"],
                    size=args.size,
                    quality=args.quality,
                    n=args.n,
                )
            finally:
                for f in files:
                    f.close()

            for i, item in enumerate(response.data):
                if not item.b64_json:
                    print(f"  ERROR: variant {i} no image returned", file=sys.stderr)
                    continue
                suffix = f"_{i+1}" if args.n > 1 else ""
                out_name = f"{variant['name']}{suffix}_{timestamp}.png"
                out_path = out_dir / out_name
                out_path.write_bytes(base64.b64decode(item.b64_json))
                if args.story_pad:
                    pad_to_story(out_path)
                print(f"  → {out_path}", file=sys.stderr)

            usage = getattr(response, "usage", None)
            if usage:
                ip = getattr(usage, "input_tokens", 0)
                op = getattr(usage, "output_tokens", 0)
                print(f"  tokens: input={ip} output={op}", file=sys.stderr)
        except Exception as e:
            print(f"  ERROR: {type(e).__name__}: {e}", file=sys.stderr)

    return 0


if __name__ == "__main__":
    sys.exit(main())
