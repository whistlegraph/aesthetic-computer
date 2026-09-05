#!/usr/bin/env python3
# lyricvideo.py — the common chassis for utterance-clocked lyric/timing
# videos (@jeffrey 2026-09-03: "we should have a common codebase for
# these kinds of videos"). Descended from loner's timeline.py → imab's
# aesthetivox-alignment-video.py → lyricscroll.py; the wannadash IG reel
# (pop/viz/lyric-reel.py) is the vertical cousin and can adopt the
# runner as it lands. Lanes compose the draw layers they want; nothing
# here knows about any one song.
#
# The timing interchange format is lyrictrack's JSON: objects with
# fromMs/toMs (+ label/text), absolute or stem-relative — seconds are
# the API here, callers convert.

import datetime, math, os, subprocess
import numpy as np
from PIL import Image, ImageDraw, ImageFont

# ── theme ─────────────────────────────────────────────────────────────
def theme(name=None):
    name = name or os.environ.get("SCORE_THEME") or (
        "light" if 7 <= datetime.datetime.now().hour < 19 else "dark")
    if name == "light":
        return dict(name="light",
            CREAM=(255, 253, 246), INK=(26, 26, 34), PINK=(255, 82, 156), BLUE=(72, 100, 172),
            PINK_HOT=(188, 30, 104), BLOCK=(255, 166, 202, 96), GLOW=(255, 82, 156, 70),
            KICK=(72, 100, 172, 70),
            BEAT_TINT=[(255, 82, 156, 30), (72, 100, 172, 14), (26, 26, 34, 10), (72, 100, 172, 14)],
            WAVE=(26, 26, 34, 185))
    return dict(name="dark",
        CREAM=(14, 13, 20), INK=(238, 234, 230), PINK=(255, 92, 162), BLUE=(132, 158, 236),
        PINK_HOT=(255, 176, 214), BLOCK=(255, 92, 162, 52), GLOW=(255, 92, 162, 64),
        KICK=(132, 158, 236, 66),
        BEAT_TINT=[(255, 92, 162, 34), (132, 158, 236, 14), (238, 234, 230, 10), (132, 158, 236, 14)],
        WAVE=(238, 234, 230, 200))

def font(size, face="/System/Library/Fonts/Helvetica.ttc"):
    return ImageFont.truetype(face, size)

# ── audio ─────────────────────────────────────────────────────────────
def mono(path, sr=8000):
    """Decode any audio file to mono float32 at sr (envelope resolution)."""
    r = subprocess.run(["ffmpeg", "-v", "error", "-i", path, "-ac", "1",
                        "-ar", str(sr), "-f", "f32le", "-"], capture_output=True)
    return np.frombuffer(r.stdout, np.float32)

def duration(path):
    return float(subprocess.run(["ffprobe", "-v", "quiet", "-show_entries", "format=duration",
                                 "-of", "default=nw=1:nk=1", path],
                                capture_output=True, text=True).stdout.strip())

def f0_trace(path, fmin=70, fmax=700, cache=True):
    """Extract the sung pitch of a vocal file as (times, midi) with NaN
    where unvoiced — the curve the eye follows through the note blocks.
    Cached beside the audio as <file>.f0.npz (invalidated on mtime)."""
    npz = path + ".f0.npz"
    if cache and os.path.exists(npz) and os.path.getmtime(npz) >= os.path.getmtime(path):
        z = np.load(npz)
        return z["t"], z["m"]
    import librosa
    y, sr = librosa.load(path, sr=22050, mono=True)
    f0, voiced, vprob = librosa.pyin(y, sr=sr, fmin=fmin, fmax=fmax,
                                     frame_length=2048, hop_length=256)
    t = librosa.times_like(f0, sr=sr, hop_length=256)
    m = np.where(np.isfinite(f0) & voiced & (vprob > 0.3),
                 69 + 12 * np.log2(np.maximum(f0, 1e-9) / 440.0), np.nan)
    if cache:
        np.savez(npz, t=t, m=m)
    return t, m

# ── note names ────────────────────────────────────────────────────────
NAMES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
BLACK_PC = {1, 3, 6, 8, 10}

def to_midi(n):
    import re
    m = re.match(r"^([A-G]#?)(-?\d)$", n)
    return (int(m.group(2)) + 1) * 12 + NAMES.index(m.group(1))

# ── the scroll ────────────────────────────────────────────────────────
class Scroll:
    """Fixed playhead, roll sliding under it. All layers share one
    time→x mapping; a lane builds one per frame via at(t)."""
    def __init__(self, playhead_x=560, px_per_beat=96, spb=0.5):
        self.PLAYHEAD_X, self.PXB, self.SPB = playhead_x, px_per_beat, spb
        self.PXS = px_per_beat / spb
    def at(self, t):
        return lambda te: self.PLAYHEAD_X + round((te - t) * self.PXS)

def ribbon_layout(entries, lyric_font, pxs, gap=22, rows=2):
    """Timestamp-TRUE ribbon: every word draws at its real time. Words
    that would overprint drop to the next row instead of sliding right
    — the old rightward dodge made the ribbon lie as a timeline
    (@jeffrey read the shift as clips misplaced, 2026-09-05)."""
    _m = ImageDraw.Draw(Image.new("RGB", (8, 8)))
    ends = [None] * rows
    for wd in entries:
        wd["w"] = _m.textlength(wd["text"], font=lyric_font)
        wd["td"] = wd["t"]
        for r in range(rows):
            if ends[r] is None or wd["t"] * pxs >= ends[r]:
                wd["row"] = r
                break
        else:
            wd["row"] = min(range(rows), key=lambda r: ends[r])
        ends[wd["row"]] = wd["t"] * pxs + wd["w"] + gap
    return entries

# ── draw layers (each takes the frame's draw ctx + x_of mapping) ──────
def piano_rows(d, W, x_of, y_of, lo, hi, rowh, TH, f_note, label_all=True):
    """Rows tinted like a piano roll; note names climb the left edge —
    every row named when label_all (C rows louder)."""
    for m in range(lo, hi + 1):
        ry = y_of(m)
        if (m % 12) in BLACK_PC:
            d.rectangle([0, ry - rowh / 2, W, ry + rowh / 2], fill=(*TH["INK"], 12))
        if m % 12 == 0:
            d.line([0, ry + rowh / 2, W, ry + rowh / 2], fill=(*TH["INK"], 60), width=1)
        name = NAMES[m % 12] + str(m // 12 - 1)
        if label_all or m % 12 == 0:
            d.text((8, ry - rowh / 2 + max(0, rowh / 2 - 12)), name, font=f_note,
                   fill=(*TH["INK"], 190 if m % 12 == 0 else 80))

def beat_columns(d, t, x_of, scroll, W, y0, y1, total_beats, TH, f_bar, hot_bars=(), line_y1=None,
                 beat_label=None, f_beat=None):
    """beat_label(b) → a short address for beat index b (e.g. "A3"),
    drawn small inside the top of every beat column; None skips it."""
    line_y1 = line_y1 if line_y1 is not None else y1
    b0 = int(max(0, (t - scroll.PLAYHEAD_X / scroll.PXS) / scroll.SPB) - 1)
    b1 = int((t + (W - scroll.PLAYHEAD_X) / scroll.PXS) / scroll.SPB) + 1
    for b in range(max(0, b0), min(total_beats, b1)):
        x = x_of(b * scroll.SPB)
        d.rectangle([x, y0, x + scroll.PXB, y1], fill=TH["BEAT_TINT"][b % 4])
        d.line([x, y0, x, line_y1], fill=(*TH["INK"], 90 if b % 4 == 0 else 30), width=2 if b % 4 == 0 else 1)
        if b % 4 == 0:
            bar = b // 4
            d.text((x + 8, y0 - 36), str(bar), font=f_bar,
                   fill=(*TH["PINK"], 255) if bar in hot_bars else (*TH["INK"], 200))
        if beat_label:
            lbl = beat_label(b)
            if lbl:
                d.text((x + 6, y0 + 6), lbl, font=f_beat or f_bar,
                       fill=(*TH["INK"], 170 if b % 4 == 0 else 110))

def blocks(d, t, x_of, words, y_of, rowh, W, TH, f_word, f_off, voxsamp, sr=8000, accent=None):
    """Syllable/note blocks with the vocal's own envelope inside.
    words: t, dur, midi, label, stem_t (offset into voxsamp), off (ms).
    accent: {HOT, BLOCK, GLOW} per-take color theme override."""
    HOT = (accent or {}).get("HOT", TH["PINK_HOT"])
    BLK = (accent or {}).get("BLOCK", TH["BLOCK"])
    GLW = (accent or {}).get("GLOW", TH["GLOW"])
    hh = rowh / 2 - 1
    for w in words:
        x0, x1 = x_of(w["t"]), x_of(w["t"] + w["dur"])
        if x1 < -50 or x0 > W + 50: continue
        y = y_of(w["midi"])
        active = w["t"] <= t <= w["t"] + w["dur"]
        d.rectangle([x0, y - hh, x1, y + hh], fill=GLW if active else BLK,
                    outline=(*HOT, 255) if active else (*HOT, 140), width=2)
        v0 = w["stem_t"] * sr
        vspan = w["dur"] * sr
        for bx in range(int(x0) + 2, int(x1) - 1, 2):
            fa = int(v0 + (bx - x0) / max(1, x1 - x0) * vspan)
            fb = fa + max(2, int(vspan / max(1, x1 - x0) * 2))
            seg = voxsamp[max(0, fa):max(0, fb)]
            if not len(seg): continue
            amp = min(1.0, float(np.sqrt((seg * seg).mean())) * 7)
            wh = amp * max(2.0, hh - 3)
            d.rectangle([bx, y - wh, bx + 2, y + wh], fill=(*TH["BLUE"], 230 if active else 150))
        d.text((x0 + 5, y - hh - 34), w["label"], font=f_word,
               fill=TH["INK"] if active else HOT)
        sub = " · ".join(filter(None, [w.get("note"),
                                       f'{w["off"]:+d}ms' if "off" in w else None]))
        if sub:
            d.text((x0 + 5, y + hh + 3), sub, font=f_off,
                   fill=(*TH["INK"], 200) if abs(w.get("off", 0)) > 45 else (*TH["INK"], 90))

def pitch_trace(d, t, x_of, ft, fm, y_of, TH, W, offset=0.0, lo=None, hi=None, color=None):
    """The sung f0 curve on the roll: dots per voiced frame at their
    pitch row, brighter behind the playhead. offset places the trace's
    zero at an absolute track time (e.g. a pass door)."""
    col = color or TH["BLUE"]
    px_prev = None
    for k in range(0, len(ft), 2):
        m = fm[k]
        if not np.isfinite(m): px_prev = None; continue
        if lo is not None and (m < lo - 1 or m > (hi or 999) + 1): continue
        x = x_of(offset + ft[k])
        if x < -10 or x > W + 10: px_prev = None; continue
        y = y_of(m)
        past = offset + ft[k] <= t
        if px_prev is not None and abs(x - px_prev[0]) <= 6 and abs(y - px_prev[1]) < 40:
            d.line([px_prev[0], px_prev[1], x, y],
                   fill=(*col, 235 if past else 130), width=3)
        else:
            d.ellipse([x - 1.5, y - 1.5, x + 1.5, y + 1.5],
                      fill=(*col, 235 if past else 130))
        px_prev = (x, y)

def kick_floor(d, x_of, spb, total_beats, y0, y1, TH, W):
    for b in range(0, total_beats):
        x = x_of(b * spb)
        if x < -20 or x > W + 20: continue
        d.rectangle([x + 2, y0, x + 26, y1], fill=TH["KICK"])

def bar_wave(d, t, samples, scroll, W, y0, y1, TH, sr=8000, full_bright=False):
    spx = sr / scroll.PXS
    mid = (y0 + y1) / 2
    for px_ in range(0, W, 2):
        ts = (px_ - scroll.PLAYHEAD_X) / scroll.PXS + t
        a = int(ts * sr)
        if a < 0 or a >= len(samples): continue
        seg = samples[a:a + max(2, int(spx * 2))]
        if not len(seg): continue
        amp = float(np.sqrt((seg * seg).mean())) * 4
        h = min(1.0, amp) * (y1 - y0) / 2
        d.rectangle([px_, mid - h, px_ + 2, mid + h],
                    fill=TH["WAVE"] if (full_bright or px_ < scroll.PLAYHEAD_X) else (*TH["INK"], 70))

def lyric_ribbon(d, t, x_of, entries, y, TH, f_lyric, W, rowdy=54):
    """The lyrics under, scrolling by as they play — each word at its
    TRUE time; crowded words stagger down a row (ribbon_layout)."""
    for wd in entries:
        x = x_of(wd["td"])
        if x < -260 or x > W + 60: continue
        active = wd["t"] <= t <= wd["t1"]
        col = wd.get("accent", TH["PINK"])
        d.text((x, y + wd.get("row", 0) * rowdy), wd["text"], font=f_lyric,
               fill=(*col, 255) if active else (*TH["INK"], 110))

def playhead(d, x, y0, y1, TH):
    d.rectangle([x - 2, y0, x + 2, y1], fill=(*TH["PINK"], 235))

# ── the runner ────────────────────────────────────────────────────────
def render(out_path, audio_path, draw_frame, start=0.0, end=None,
           w=1920, h=1080, fps=60, sync_ms=None, progress=600):
    """Pipe raw frames to ffmpeg, muxing audio_path windowed to
    [start, end]. draw_frame(t) → PIL RGB image; t is absolute
    track time (sync offset already applied)."""
    if sync_ms is None:
        sync_ms = float(os.environ.get("SYNC_MS", "0"))
    end = end if end is not None else duration(audio_path)
    frames = int(math.ceil((end - start) * fps))
    proc = subprocess.Popen(["ffmpeg", "-hide_banner", "-loglevel", "error", "-y",
        "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", f"{w}x{h}", "-r", str(fps), "-i", "-",
        "-ss", f"{start:.6f}", "-t", f"{end - start:.6f}", "-i", audio_path,
        "-map", "0:v", "-map", "1:a",
        "-c:v", "libx264", "-preset", "fast", "-crf", "19", "-c:a", "aac", "-b:a", "192k",
        "-shortest", out_path], stdin=subprocess.PIPE)
    for i in range(frames):
        t = start + (i + 0.5) / fps - sync_ms / 1000.0
        proc.stdin.write(draw_frame(t).tobytes())
        if progress and i % progress == 0:
            print(f"  {i}/{frames}", flush=True)
    proc.stdin.close(); proc.wait()
    print(f"✓ {out_path}")
