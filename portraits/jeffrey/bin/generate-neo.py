#!/opt/homebrew/Cellar/instaloader/4.15.1_1/libexec/bin/python3
"""
Generate stylized "neo-jeffrey" portraits using OpenAI gpt-image-1, conditioned
on a reference photo from the platter for identity. Saves results to ~/Desktop.

Two variants by default:
  1. neo-jeffrey-pixel   — chunky-pixel digital character art, AC palette
  2. neo-jeffrey-paint   — textured digital painting, AC palette

This is the first hands-on rendering of the "scene-graph + AC brand vocabulary"
direction from the rev2 feasibility doc — a cheap exercise to feel out tone
before committing to gen.mjs / gen.js / Gemini.
"""

from __future__ import annotations

import argparse
import base64
import os
import sys
import threading
import time
from datetime import datetime
from pathlib import Path

from openai import OpenAI

# Progress heartbeat for the Slab menubar (shows a live "image" bar while a
# gen is in flight). Optional — degrade to a no-op if the helper is absent.
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
try:
    from ac_heartbeat import Heartbeat
except Exception:  # pragma: no cover
    class Heartbeat:  # no-op fallback
        def __init__(self, *a, **k): pass
        def begin(self, *a, **k): return None
        def update(self, *a, **k): pass
        def end(self): pass

REPO_ROOT = Path(__file__).resolve().parent.parent.parent.parent
VAULT_ENV = REPO_ROOT / "aesthetic-computer-vault" / ".devcontainer" / "envs" / "devcontainer.env"
SHOOT_DIR = REPO_ROOT / "portraits" / "jeffrey" / "corpus" / "shoot"
DESKTOP = Path.home() / "Desktop"

# Multiple references → stronger identity preservation. The default set is the
# canonical AV-shoot hero shots; --use-selfies adds varied IG-platter selfies
# from across the years (2015..2025) to teach the model his casual look across
# eras. gpt-image-1.5 / 2 accept up to 16 reference images per call.
SHOOT_REFS = [
    SHOOT_DIR / "jeffery-av--07.jpg",
    SHOOT_DIR / "jeffery-av--01.jpg",
    SHOOT_DIR / "jeffery-av--04.jpg",
]
ARCHIVE_DIR = REPO_ROOT / "portraits" / "jeffrey" / "ig-archive" / "whistlegraph"
SELFIE_REFS = [
    ARCHIVE_DIR / "2018-12-02_Bq4ckGFFNtW.jpg",
    ARCHIVE_DIR / "2020-09-02_CEpxlO2FOvD.jpg",
    ARCHIVE_DIR / "2021-07-10_CRI095Vl7AO_1.jpg",
    ARCHIVE_DIR / "2025-01-25_DFQ2lHPzN_W.jpg",
    ARCHIVE_DIR / "2017-04-10_BStid5yjTHq.jpg",
]
DEFAULT_REFS = SHOOT_REFS

VARIANTS = [
    {
        "name": "neo-jeffrey-editorial",
        "prompt": """\
Photographic portrait of the man in the reference photo, shot for aesthetic.computer — a creative-coding \
platform. This is a real photograph, not an illustration. Photo-realistic.

Identity: same face, same medium-length brown hair, same overall bearing as the reference. Soft warm \
smile, relaxed eyes, eye-contact with the camera. He is recognizably him. Do NOT youthify, smooth, or \
prettify the skin — keep real texture, keep his actual features.

Setting and styling: clean editorial portrait, studio shot. Cream off-white seamless backdrop \
(#f5f5f5) with a single soft prop or accent in one of these colors: hot pink #ff6b9d, cyan #4ecdc4, \
gold #ffd93d. He could be wearing a simple tee or sweatshirt in one of those colors. Casual, lived-in, \
not-glossy. Slight visible film grain ok.

Lighting: soft natural daylight from one side, gentle fill, warm white balance. The kind of friendly \
welcome-portrait you'd see on the home page of a beloved indie creative-coding website.

Framing: medium-close shot, head and shoulders, square 1:1 aspect ratio. Eye-level, slight 3/4 angle. \
Photographic, NOT illustrated, NOT painted, NOT AI-poster-glossy.\
""",
    },
    {
        "name": "neo-jeffrey-cyberpunk-swords",
        "prompt": """\
Photographic cyberpunk-noir portrait of the man in the reference photos. Real photograph, \
photo-realistic, cinematic — NOT illustrated, NOT painted, NOT a video-game render.

Identity: same face, same medium-length brown hair, recognizable as him across the various \
references. Intense fierce battle-ready expression — mouth half-open in a guttural ready-yell, \
eyes hard and locked forward, brow lowered. Defiant, fearsome, NOT joyful, NOT smiling.

Styling: dark techwear / cyberpunk fashion — black leather or matte tactical jacket with subtle \
luminescent piping along the seams, high collar, fingerless gloves, combat boots, maybe a worn \
bandana around the neck. Damp skin catching the neon light.

Setting: a neon-soaked alley somewhere in 2049 — wet asphalt reflecting hot pink (#ff6b9d) and \
cyan (#4ecdc4) holographic signage in a foreign script, distant gold (#ffd93d) advertisement glow. \
Heavy volumetric atmosphere — light rain, mist, deep shadows broken by vivid practical neons. \
Blade Runner 2049 / Cyberpunk 2077 / Ghost in the Shell mood. Hard rim light from a magenta sign \
behind him.

Action and props: he is wielding TWO glowing energy katanas — one in each hand, blades crossed in \
a defensive ready stance just below face level. The katanas are sci-fi weapons: matte-black hilts \
with chrome accents, blades made of shimmering plasma in saturated neon colors — the right blade \
glows hot pink (#ff6b9d), the left blade glows cyan (#4ecdc4). The plasma blades cast colored \
light onto his face from below, painting half his face pink and half cyan, illuminating his \
features dramatically. Wisps of glowing plasma trail off the blade edges.

Mood: deadly cyberpunk samurai energy. Unmistakably scary. The aesthetic.computer absurdist edge \
appears in the over-the-top neon palette of the blades, not in any cuteness — this version is \
purely menacing.

Framing: full-body shot, vertical 1024x1536. Slight low angle from waist height looking up — he \
fills the frame head to boots in a wide planted stance, with the alley extending behind him. \
Dramatic, heroic-villain composition. Shallow depth of field, crisp on him, blurred neon bokeh \
behind. Photographic.\
""",
    },
    {
        "name": "neo-jeffrey-cyberpunk",
        "prompt": """\
Photographic cyberpunk-noir portrait of the man in the reference photo. Real photograph, \
photo-realistic, cinematic — NOT illustrated, NOT painted, NOT a still from a video game.

Identity: same face, same medium-length brown hair, recognizable as him. Intense fierce expression — \
mouth open in a guttural, defiant yell, eyes hard and locked on the camera, brow lowered. \
Menacing energy — he looks like he means it. NOT joyful, NOT smiling.

Styling: dark techwear / cyberpunk fashion — black leather or matte tactical jacket with subtle \
luminescent piping, high collar, maybe a worn bandana around the neck. Maybe wet hair. Damp skin \
catching the neon. Single small chrome accent earring or eyebrow ring.

Setting: a neon-soaked alley somewhere in 2049 — wet asphalt reflecting hot pink (#ff6b9d) and cyan \
(#4ecdc4) holographic signage in a foreign script, distant gold (#ffd93d) advertisement. Heavy \
volumetric atmosphere — light rain, mist, deep shadows broken by vivid practical neons. Blade Runner \
2049 / Cyberpunk 2077 / Akira urban-noir mood. Hard rim light from a magenta sign behind him.

Action and prop: he holds up — toward the camera — a CLEARLY-TOY plastic bubble gun. The toy is \
manifestly a kids' toy: bright translucent neon-pink plastic body, oversized cartoonish trigger, \
visible see-through bubble mixture reservoir on top, rounded chunky kid-friendly proportions. The \
absurd pink-translucent plastic toy in his angry tactical cyberpunk grip is the whole joke of the \
photo. Just three or four lone soap bubbles drift lazily out of the muzzle and float into the rainy \
neon air — sparse, not a swarm.

Mood: serious cyberpunk menace, deadly tactical pose, undercut by the manifestly-silly translucent \
plastic toy and the few soft bubbles. Tonal contradiction. The aesthetic.computer absurdist energy.

Framing: full-body shot, vertical 1024x1536. Slight low angle from waist height looking up — he \
fills the frame head to boots, with the alley extending behind him. Wide enough to show the full \
toy in his hand, his stance (planted, aggressive), his whole outfit, and the neon environment \
above and around. Photographic, shallow depth of field, gentle motion blur on the bubbles.\
""",
    },
    {
        "name": "neo-jeffrey-lifestyle",
        "prompt": """\
Photographic candid lifestyle portrait of the man in the reference photo, shot for aesthetic.computer. \
This is a real photograph, not an illustration. Photo-realistic.

Identity: same face, same medium-length brown hair, same actual features as the reference. Soft warm \
smile, eyes alive, looking gently at or just past the camera. He is recognizably him. Keep real skin \
texture and his actual face — do NOT smooth or AI-prettify.

Setting: at home in a relaxed creative workspace — a wooden desk with a laptop and a few sketchbooks \
or a sketchpad and pencils, a window throwing warm afternoon light, plants. Subtle pops of \
aesthetic.computer brand color woven into the scene: a hot-pink mug (#ff6b9d), a cyan post-it (#4ecdc4), \
or a small gold object (#ffd93d). The colors are accents, not dominant.

Mood: warm, candid, off-guard, approachable. Like a friendly photo a friend would take of him while \
he's mid-thought. Slight shallow depth of field with gentle bokeh.

Framing: medium shot waist-up, square 1:1, slight 3/4 angle. Photographic, NOT illustrated, NOT \
painted, NOT corporate-headshot, NOT AI-glossy.\
""",
    },
]


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
    p.add_argument("--refs", nargs="+", default=None,
                   help="reference photo(s); multiple = stronger identity. "
                        "Default: SHOOT_REFS (3 staged headshots).")
    p.add_argument("--use-selfies", action="store_true",
                   help="also include 5 IG-platter selfies as refs (2015..2025) "
                        "for varied identity grounding across eras.")
    p.add_argument("--out", default=str(DESKTOP), help="output directory")
    p.add_argument("--size", default="1024x1536",
                   help="1024x1024 / 1024x1536 (portrait) / 1536x1024 (landscape) / auto")
    p.add_argument("--quality", default="high", choices=["low", "medium", "high", "auto"])
    p.add_argument("--input-fidelity", default="high", choices=["low", "high"],
                   help="how closely to preserve identity from refs; high = stronger")
    p.add_argument("--variant", choices=[v["name"] for v in VARIANTS] + ["all"], default="all")
    p.add_argument("--prompt", default=None,
                   help="custom prompt; overrides --variant for one-off gens")
    p.add_argument("--name", default="neo-jeffrey-custom",
                   help="output filename stem when --prompt is used")
    p.add_argument("--n", type=int, default=1, help="variants per prompt")
    p.add_argument("--model", default="gpt-image-2",
                   help="gpt-image-2 (default, latest) | gpt-image-1.5 | gpt-image-1")
    args = p.parse_args()

    if args.refs:
        refs = [Path(r).expanduser().resolve() for r in args.refs]
    else:
        refs = list(SHOOT_REFS)
        if args.use_selfies:
            refs.extend(SELFIE_REFS)
    refs = [Path(r).expanduser().resolve() for r in refs]
    for r in refs:
        if not r.exists():
            sys.exit(f"reference not found: {r}")
    out_dir = Path(args.out).expanduser().resolve()
    out_dir.mkdir(parents=True, exist_ok=True)

    if args.prompt:
        selected = [{"name": args.name, "prompt": args.prompt}]
    else:
        selected = VARIANTS if args.variant == "all" else [v for v in VARIANTS if v["name"] == args.variant]
    timestamp = datetime.now().strftime("%Y-%m-%d_%H%M%S")

    client = OpenAI(api_key=load_openai_key())
    print(f"references ({len(refs)}): {', '.join(r.name for r in refs)}", file=sys.stderr)
    print(f"out: {out_dir}", file=sys.stderr)
    print(f"settings: size={args.size} quality={args.quality} fidelity={args.input_fidelity} n={args.n}",
          file=sys.stderr)

    # Slab menubar progress: one indeterminate "image" bar, per-variant counts.
    # A keepalive thread refreshes updatedAt so the menubar's 120s staleness
    # sweep never drops a bar mid-render (a single gpt-image call can be slow).
    total = len(selected)
    hb = Heartbeat(type="image", label=f"neo-jeffrey ×{total}")
    hb.begin(pct=None)
    _alive = {"on": True, "done": 0}
    def _keepalive():
        while _alive["on"]:
            hb.update(pct=None, done=_alive["done"], total=total)
            time.sleep(5)
    _t = threading.Thread(target=_keepalive, daemon=True)
    _t.start()

    for vi, variant in enumerate(selected):
        hb.update(pct=None, done=vi, total=total)
        print(f"\ngenerating {variant['name']}…", file=sys.stderr)
        try:
            files = [open(r, "rb") for r in refs]
            try:
                edit_kwargs = dict(
                    model=args.model,
                    image=files,
                    prompt=variant["prompt"],
                    size=args.size,
                    quality=args.quality,
                    n=args.n,
                )
                # input_fidelity is gpt-image-1 only; gpt-image-2 rejects it.
                if not args.model.startswith("gpt-image-2"):
                    edit_kwargs["input_fidelity"] = args.input_fidelity
                response = client.images.edit(**edit_kwargs)
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
                print(f"  → {out_path}", file=sys.stderr)

            usage = getattr(response, "usage", None)
            if usage:
                ip = getattr(usage, "input_tokens", 0)
                op = getattr(usage, "output_tokens", 0)
                print(f"  tokens: input={ip} output={op}", file=sys.stderr)
        except Exception as e:
            print(f"  ERROR: {type(e).__name__}: {e}", file=sys.stderr)
        _alive["done"] = vi + 1

    _alive["on"] = False
    hb.end()
    return 0


if __name__ == "__main__":
    sys.exit(main())
