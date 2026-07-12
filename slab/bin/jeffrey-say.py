#!/usr/bin/env python3
"""Generic Jeffrey-voice stinger.

Routes a short phrase through the AC `say` endpoint (ElevenLabs PVC clone)
and plays it locally with a cosine fade-out. Used by every slab event
sound except the lid-transition chimes and the ambient bed.

Usage:
    jeffrey-say.py done            # phrase from the 'done' category (random)
    jeffrey-say.py count 3         # specific count number
    jeffrey-say.py --phrase "hi"   # speak a specific phrase verbatim
    jeffrey-say.py --prewarm       # fetch + cache every phrase in every
                                   # category so first plays are instant

Categories live in CATEGORIES below — edit there to tune the lexicon.
"""

import json
import os
import random
import re
import subprocess
import sys
import tempfile
import urllib.error
import urllib.request
import wave
from pathlib import Path

import numpy as np
import sounddevice as sd

# Count words for the "active prompts remaining" cue. Capped at 8 to match
# the original beep_1..8 ladder.
COUNT_WORDS = ["one", "two", "three", "four", "five", "six", "seven", "eight"]

CATEGORIES = {
    # "all done" — lid OPEN, every active prompt finished, machine stays awake.
    "done": ["i'm all done"],

    # Sleep stinger — lid CLOSED, every active prompt finished, about to
    # pmset sleepnow. Slower/sleepier than 'done'.
    "tired": ["i'm tired"],

    # Pre-sleep auditory cue, played after the grace delay before pmset.
    "sleep": ["sleeping now"],

    # One subagent (Task) just finished. Fires frequently → keep variants
    # short and varied.
    "subagent": ["one down", "got one", "check", "next", "okay"],

    # Periodic cue while the lid is closed and work is in flight. Fires
    # every 30s — keep variants short and varied so the loop doesn't grate.
    "ping": ["still here", "still going", "still working", "hold on",
             "thinking", "almost there", "soon"],

    # Active-prompt count remaining on Stop. Pick via subcategory "count N"
    # where N is the number; see resolve_phrase below.
    "count": COUNT_WORDS,
}

HOLD_FRAC = 0.55
TAIL_SILENCE_S = 0.2

SAY_ENDPOINT = "https://aesthetic.computer/api/say"
USER_AGENT = "slab-menubar/1.0 (jeffrey-say)"
SLAB_HOME = Path(os.environ.get("SLAB_HOME", Path.home() / ".local/share/slab"))
CACHE_DIR = SLAB_HOME / "sounds" / "jeffrey"

FALLBACK_RATE = 175


def slug(phrase):
    return re.sub(r"[^a-z0-9]+", "-", phrase.lower()).strip("-") or "phrase"


def cache_path(phrase):
    return CACHE_DIR / f"{slug(phrase)}.mp3"


class _NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        return None


def fetch_jeffrey_mp3(phrase):
    dest = cache_path(phrase)
    if dest.exists() and dest.stat().st_size > 0:
        return dest
    dest.parent.mkdir(parents=True, exist_ok=True)
    body = json.dumps({"from": phrase, "provider": "jeffrey"}).encode("utf-8")
    opener = urllib.request.build_opener(_NoRedirect)
    req = urllib.request.Request(
        SAY_ENDPOINT,
        data=body,
        headers={"Content-Type": "application/json", "User-Agent": USER_AGENT},
        method="POST",
    )
    cdn_url = None
    data = None
    try:
        with opener.open(req, timeout=15) as resp:
            if resp.status == 200:
                data = resp.read()
            else:
                raise RuntimeError(f"unexpected status {resp.status}")
    except urllib.error.HTTPError as e:
        if e.code in (301, 302, 303, 307, 308):
            cdn_url = e.headers.get("Location")
        else:
            raise
    if cdn_url:
        cdn_req = urllib.request.Request(cdn_url, headers={"User-Agent": USER_AGENT})
        with urllib.request.urlopen(cdn_req, timeout=15) as resp2:
            data = resp2.read()
    if not data or (not data.startswith(b"ID3") and b"\xff\xfb" not in data[:64] and b"\xff\xf3" not in data[:64]):
        raise RuntimeError("response is not an mp3")
    tmp_path = dest.with_suffix(".mp3.partial")
    tmp_path.write_bytes(data)
    tmp_path.replace(dest)
    return dest


def decode_to_audio(audio_path):
    with tempfile.TemporaryDirectory() as tmp:
        wav = os.path.join(tmp, "out.wav")
        subprocess.run(
            ["/usr/bin/afconvert", "-f", "WAVE", "-d", "LEI16@22050",
             str(audio_path), wav],
            check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
        )
        with wave.open(wav, "rb") as wf:
            sr = wf.getframerate()
            nch = wf.getnchannels()
            raw = wf.readframes(wf.getnframes())
    audio = np.frombuffer(raw, dtype=np.int16).astype(np.float32) / 32768.0
    if nch > 1:
        audio = audio.reshape(-1, nch).mean(axis=1)
    return audio, sr


def synth_fallback(phrase):
    with tempfile.TemporaryDirectory() as tmp:
        aiff = os.path.join(tmp, "out.aiff")
        subprocess.run(
            ["/usr/bin/say", "-r", str(FALLBACK_RATE), "-o", aiff, phrase],
            check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
        )
        return decode_to_audio(aiff)


def envelope(audio, sr, hold_frac=HOLD_FRAC, tail_s=TAIL_SILENCE_S):
    n = audio.size
    if n == 0:
        return audio
    hold = int(n * hold_frac)
    env = np.ones(n, dtype=np.float32)
    fade_n = n - hold
    if fade_n > 0:
        t = np.linspace(0.0, 1.0, fade_n, dtype=np.float32)
        env[hold:] = np.cos(t * np.pi * 0.5) ** 2
    tail = np.zeros(int(sr * tail_s), dtype=np.float32)
    return np.concatenate([audio * env, tail])


# Softness/echo tuning — used by callers that want a gentler, roomier delivery
# (e.g. the jas-nzxt guest connect/disconnect announcer) rather than the brisk
# default stinger.
SOFT_GAIN = 0.5          # scale amplitude down so the phrase doesn't bark
SOFT_ATTACK_S = 0.04     # short fade-in to remove the hard consonant onset
ECHO_DELAY_S = 0.19      # spacing between echo taps
ECHO_DECAY = 0.42        # per-tap amplitude falloff
ECHO_TAPS = 3            # number of trailing echoes


def soften(audio, sr, gain=SOFT_GAIN, attack_s=SOFT_ATTACK_S):
    if audio.size == 0:
        return audio
    out = audio * gain
    a = min(int(sr * attack_s), out.size)
    if a > 0:
        t = np.linspace(0.0, 1.0, a, dtype=np.float32)
        out[:a] *= np.sin(t * np.pi * 0.5) ** 2   # equal-power fade-in
    return out


def add_echo(audio, sr, delay_s=ECHO_DELAY_S, decay=ECHO_DECAY, taps=ECHO_TAPS):
    if audio.size == 0:
        return audio
    d = int(sr * delay_s)
    buf = np.zeros(audio.size + d * taps, dtype=np.float32)
    buf[:audio.size] = audio
    for k in range(1, taps + 1):
        start = d * k
        buf[start:start + audio.size] += audio * (decay ** k)
    peak = float(np.max(np.abs(buf)))
    if peak > 1.0:                 # guard against summed-tap clipping
        buf /= peak
    return buf


def all_phrases():
    seen = set()
    for phrases in CATEGORIES.values():
        for p in phrases:
            if p not in seen:
                seen.add(p)
                yield p


def prewarm():
    failures = []
    for phrase in all_phrases():
        try:
            path = fetch_jeffrey_mp3(phrase)
            print(f"  ✓ {phrase!r:24s} → {path}")
        except Exception as e:
            print(f"  ✗ {phrase!r:24s} → {e}", file=sys.stderr)
            failures.append(phrase)
    return 1 if failures else 0


def resolve_phrase(args):
    """Return the phrase to speak from CLI args, or None to show usage."""
    if not args:
        return None
    # Explicit verbatim phrase.
    if args[0] == "--phrase" and len(args) >= 2:
        return " ".join(args[1:])
    # Category lookup.
    category = args[0]
    if category not in CATEGORIES:
        return None
    # Count is special: optional integer arg picks a specific number, else
    # random — but pinging "count" without a number doesn't make sense, so
    # require the integer.
    if category == "count":
        if len(args) < 2:
            return None
        try:
            n = int(args[1])
        except ValueError:
            return None
        if 1 <= n <= len(COUNT_WORDS):
            return COUNT_WORDS[n - 1]
        # Out of range: clamp to last word (matches the original beep cap).
        return COUNT_WORDS[-1]
    return random.choice(CATEGORIES[category])


USAGE = (
    "usage: jeffrey-say.py <category> [N] | --phrase <text> | --prewarm\n"
    "  categories: " + ", ".join(sorted(CATEGORIES)) + "\n"
    "  delivery:   --soft (quieter, gentle attack)  --echo (trailing echoes)\n"
    "              --gain <f> (explicit amplitude scale, overrides --soft)\n"
)


def main(argv):
    if "--prewarm" in argv:
        return prewarm()
    # Pull delivery flags out before phrase resolution so they don't get
    # mistaken for a category or verbatim words.
    want_soft = "--soft" in argv
    want_echo = "--echo" in argv
    gain = None
    if "--gain" in argv:
        i = argv.index("--gain")
        try:
            gain = float(argv[i + 1])
            del argv[i:i + 2]
        except (IndexError, ValueError):
            sys.stderr.write(USAGE)
            return 2
    argv = [a for a in argv if a not in ("--soft", "--echo")]
    phrase = resolve_phrase(argv)
    if phrase is None:
        sys.stderr.write(USAGE)
        return 2
    try:
        mp3 = fetch_jeffrey_mp3(phrase)
        audio, sr = decode_to_audio(mp3)
    except Exception as e:
        print(f"jeffrey-say: jeffrey voice unavailable ({e}); falling back to /usr/bin/say", file=sys.stderr)
        try:
            audio, sr = synth_fallback(phrase)
        except (subprocess.CalledProcessError, FileNotFoundError) as e2:
            print(f"jeffrey-say: fallback tts failed: {e2}", file=sys.stderr)
            return 1
    if gain is not None:
        audio = audio * gain
    elif want_soft:
        audio = soften(audio, sr)
    audio = envelope(audio, sr)
    if want_echo:
        audio = add_echo(audio, sr)
    sd.play(audio, sr, blocking=True)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
