#!/usr/bin/env python3
# spatial-subs.py — whistlegraph reels with words that sit on the paper.
#
#   python3 spatial-subs.py subs/pump.json            # → downloads/<code>-subs.mp4 (gitignored)
#   python3 spatial-subs.py subs/pump.json --sheet    # + contact sheet
#   python3 spatial-subs.py subs/pump.json --out x.mp4 --audio master.wav
#
# The Scared of Stairs trial (10-20) set the look: each sung word appears
# beside a drawing in that pen's ink colour, the phrase builds word by word,
# and every word rides up or down with its sung pitch. A spec holds the
# clip, the phrases (words with whisper times), each phrase's ink and its
# anchor on the 1080×1920 frame, as keyframes when the camera drifts. Word
# times come from `whisper-cli -ml 1 -sow -oj`; pitch is a numpy YIN over
# the clip's own audio, so a spec never carries notes.
#
# "mode": "single" shows one word at a time instead: it appears on its note,
# holds until the next word's note and fades as that one arrives (a phrase's
# last word holds to its "end"), cream with a soft shadow at "place", riding
# the melody against the song's median pitch — quiet enough not to compete
# with the drawing.
#
# The base is the trial-reel remaster (1080×1920, 30fps, 48k), or the spec's
# "grade" filter chain; --audio swaps in a mastered track (master-audio.mjs).
# The words are composited in the same ffmpeg pass, so the clip is encoded once.

import json
import subprocess
import sys
from pathlib import Path

import numpy as np
from PIL import Image, ImageDraw, ImageFilter, ImageFont

W, H, FPS = 1080, 1920, 30
HERE = Path(__file__).resolve().parent
FONT = "/System/Library/Fonts/Supplemental/Arial Rounded Bold.ttf"


def load_audio(src, sr=16000):
    raw = subprocess.run(
        ["ffmpeg", "-v", "error", "-i", src, "-ac", "1", "-ar", str(sr), "-f", "f32le", "-"],
        capture_output=True, check=True).stdout
    return np.frombuffer(raw, np.float32), sr


def yin_f0(x, sr, lo=80, hi=900, thresh=0.15):
    # One frame's fundamental, or None when it is unvoiced.
    tmin, tmax = sr // hi, sr // lo
    if len(x) < 2 * tmax:
        return None
    n = len(x) - tmax
    d = np.array([np.sum((x[:n] - x[t:t + n]) ** 2) for t in range(tmax)])
    cmnd = np.ones_like(d)
    cmnd[1:] = d[1:] * np.arange(1, tmax) / np.maximum(np.cumsum(d[1:]), 1e-9)
    for t in range(tmin, tmax):
        if cmnd[t] < thresh:
            while t + 1 < tmax and cmnd[t + 1] < cmnd[t]:
                t += 1
            return sr / t
    return None


def word_pitch(audio, sr, start, end):
    # Median MIDI pitch across a word, from 40ms frames every 20ms.
    a, b = int(start * sr), int(end * sr)
    hop, win = int(0.02 * sr), int(0.04 * sr)
    f0s = [f for i in range(a, max(a + 1, b - win), hop)
           if (f := yin_f0(audio[i:i + win], sr)) is not None]
    if not f0s:
        return None
    return float(np.median(69 + 12 * np.log2(np.array(f0s) / 440)))


def anchor_at(phrase, t):
    keys = phrase["anchor"]
    if isinstance(keys[0], (int, float)):
        return keys
    keys = sorted(keys, key=lambda k: k[0])
    if t <= keys[0][0]:
        return keys[0][1:]
    for (t0, x0, y0), (t1, x1, y1) in zip(keys, keys[1:]):
        if t <= t1:
            u = (t - t0) / max(t1 - t0, 1e-6)
            return x0 + (x1 - x0) * u, y0 + (y1 - y0) * u
    return keys[-1][1:]


def layout(phrase, font, size, maxw, gap):
    # Wrap the phrase into centred lines; return each word's offset from the anchor.
    space = font.getlength(" ") + gap
    lines, cur, curw = [], [], 0
    for w in phrase["words"]:
        ww = font.getlength(w["text"])
        if cur and curw + space + ww > maxw:
            lines.append((cur, curw))
            cur, curw = [], 0
        curw += (space if cur else 0) + ww
        cur.append((w, ww))
    lines.append((cur, curw))
    lh = size * 1.45
    top = -lh * (len(lines) - 1) / 2
    out = []
    for li, (ws, lw) in enumerate(lines):
        x = -lw / 2
        for w, ww in ws:
            out.append((w, x, top + li * lh))
            x += ww + space
    return out


def main():
    spec_path = Path(sys.argv[1]).resolve()
    spec = json.loads(spec_path.read_text())
    sheet = "--sheet" in sys.argv
    src = spec["source"]
    if src.startswith("http"):
        cached = HERE / "downloads" / Path(src).name
        if not cached.exists():
            subprocess.run(["curl", "-sfo", str(cached), src], check=True)
        src = str(cached)
    elif not Path(src).exists():
        src = str((spec_path.parent / src).resolve())
    out = Path(sys.argv[sys.argv.index("--out") + 1] if "--out" in sys.argv
               else HERE / "downloads" / f"{spec['code']}-subs.mp4")
    out.parent.mkdir(parents=True, exist_ok=True)

    size = spec.get("size", 58)
    stroke = spec.get("stroke", 7)
    maxw = spec.get("maxWidth", 620)
    bounce = spec.get("pitchPx", 9)  # pixels per semitone off the phrase median
    font = ImageFont.truetype(FONT, size)
    sized = {}

    def font_at(px):
        if px not in sized:
            sized[px] = ImageFont.truetype(FONT, px)
        return sized[px]

    audio, sr = load_audio(src)
    dur = len(audio) / sr
    phrases = spec["phrases"]
    single = spec.get("mode") == "single"
    if single:
        everything = [w for p in phrases for w in p["words"]]
        notes = [word_pitch(audio, sr, w["start"], w.get("end", w["start"] + 0.4)) for w in everything]
        voiced = [n for n in notes if n is not None]
        med = float(np.median(voiced))
        for i, (w, n) in enumerate(zip(everything, notes)):
            w["lift"] = 0 if n is None else float(np.clip((n - med) * bounce, -60, 60))
            nxt = everything[i + 1]["start"] if i + 1 < len(everything) else dur
            last = any(w is p["words"][-1] for p in phrases)
            w["off"] = min(w["end"], nxt) if last and "end" in w else nxt
            w["fade"] = 0.5 if last else 0.3
        print(" ".join(f"{w['text']}@{w['start']:.2f}{w['lift']:+.0f}" for w in everything))
    for p in (() if single else phrases):
        notes = [word_pitch(audio, sr, w["start"], w["end"]) for w in p["words"]]
        voiced = [n for n in notes if n is not None]
        med = float(np.median(voiced)) if voiced else 0
        for w, n in zip(p["words"], notes):
            w["lift"] = 0 if n is None else float(np.clip((n - med) * bounce, -24, 24))
        p["placed"] = layout(p, font, size, p.get("maxWidth", maxw), stroke * 2)
        p["until"] = p.get("until", p["words"][-1]["end"] + spec.get("hold", 0.6))
        print(p["ink"], " ".join(f"{w['text']}{w['lift']:+.0f}" for w in p["words"]))

    grade = spec.get("grade", f"scale={W}:{H}:flags=lanczos,unsharp=5:5:0.4:5:5:0.0")
    audio_in = sys.argv[sys.argv.index("--audio") + 1] if "--audio" in sys.argv else None
    ff = subprocess.Popen(
        ["ffmpeg", "-v", "error", "-y",
         "-i", src,
         *(["-i", audio_in] if audio_in else []),
         "-f", "rawvideo", "-pix_fmt", "rgba", "-s", f"{W}x{H}", "-r", str(FPS), "-i", "pipe:0",
         "-filter_complex",
         f"[0:v]{grade},fps={FPS}[b];"
         f"[b][{2 if audio_in else 1}:v]overlay=shortest=1,format=yuv420p[v]",
         "-map", "[v]", "-map", "1:a" if audio_in else "0:a",
         "-c:v", "libx264", "-profile:v", "high", "-preset", "slow", "-crf", "16",
         "-movflags", "+faststart", "-c:a", "aac", "-b:a", "256k", "-ar", "48000", "-ac", "2",
         str(out)],
        stdin=subprocess.PIPE)

    pop = 0.09
    cream = tuple(int(spec.get("cream", "#fff4e2")[i:i + 2], 16) for i in (1, 3, 5))

    def draw_single(img, t):
        for w in (w for p in phrases for w in p["words"]):
            if not (w["start"] <= t < w["off"] + w["fade"]):
                continue
            k = min(1, (t - w["start"]) / pop)
            a = min(1, k * 1.5) * (1 if t < w["off"] else 1 - (t - w["off"]) / w["fade"])
            fnt = font_at(max(8, int(size * (0.8 + 0.2 * k))))
            x0, y0 = spec["place"]
            y0 -= w["lift"]
            l, tp, r, b = fnt.getbbox(w["text"], anchor="ms")
            pad = 24
            box = Image.new("L", (r - l + pad * 2, b - tp + pad * 2), 0)
            ImageDraw.Draw(box).text((pad - l, pad - tp), w["text"], font=fnt, fill=255, anchor="ms")
            shadow = box.filter(ImageFilter.GaussianBlur(7)).point(lambda v: int(v * 0.6 * a))
            ox, oy = int(x0 + l - pad), int(y0 + tp - pad)
            img.paste((40, 22, 12, 255), (ox + 2, oy + 5), shadow)
            img.paste(cream + (255,), (ox, oy), box.point(lambda v: int(v * a)))

    for f in range(int(dur * FPS)):
        t = f / FPS
        img = Image.new("RGBA", (W, H), (0, 0, 0, 0))
        d = ImageDraw.Draw(img)
        if single:
            draw_single(img, t)
        for p in (() if single else phrases):
            if not (p["words"][0]["start"] <= t < p["until"]):
                continue
            ax, ay = anchor_at(p, t)
            fade = min(1, (p["until"] - t) / 0.25)
            for w, ox, oy in p["placed"]:
                if t < w["start"]:
                    continue
                k = min(1, (t - w["start"]) / pop)
                s = 0.55 + 0.45 * k
                fnt = font if s >= 0.999 else font_at(max(8, int(size * s)))
                ww = fnt.getlength(w["text"])
                full = font.getlength(w["text"])
                x = ax + ox + (full - ww) / 2
                y = ay + oy - w["lift"] + size * (1 - s) / 2
                a = int(255 * fade * min(1, k * 1.5))
                ink = tuple(int(p["ink"][i:i + 2], 16) for i in (1, 3, 5)) + (a,)
                d.text((x, y), w["text"], font=fnt, fill=ink,
                       stroke_width=stroke, stroke_fill=(255, 255, 255, a), anchor="ls")
        ff.stdin.write(img.tobytes())
    ff.stdin.close()
    ff.wait()
    print("✓", out)

    if sheet:
        times = spec.get("sheetTimes") or [w["start"] + 0.3 for p in phrases for w in p["words"][-1:]]
        cells = []
        for t in times:
            png = subprocess.run(
                ["ffmpeg", "-v", "error", "-ss", f"{t:.2f}", "-i", str(out), "-frames:v", "1",
                 "-vf", "scale=360:-1", "-f", "image2pipe", "-vcodec", "png", "-"],
                capture_output=True, check=True).stdout
            cells.append(Image.open(__import__("io").BytesIO(png)).convert("RGB"))
        cols = min(len(cells), 6)
        rows = -(-len(cells) // cols)
        sh = Image.new("RGB", (360 * cols, 640 * rows))
        for i, c in enumerate(cells):
            sh.paste(c, ((i % cols) * 360, (i // cols) * 640))
        sp = out.with_suffix(".sheet.jpg")
        sh.save(sp, quality=85)
        print("✓", sp)


if __name__ == "__main__":
    main()
