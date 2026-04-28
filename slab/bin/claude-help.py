#!/usr/bin/env python3
"""Notification stinger: speak a short attention cue with a cosine fade-out.

Invoked from claude-notify.sh when Claude Code emits a Notification hook —
i.e. the agent has paused and is waiting on user feedback (permission prompt
or idle input). Unlike claude-tired.py, this is an attention cue, not a
lullaby: the delivery is brisker and the tail silence is small. Ambient has
already been asked to fade via SIGTERM by the caller, so this phrase plays
over the dissolving bed.

Voice: Jeffrey ElevenLabs PVC (same clone used in the LACMA pitch video),
served via the AC `say` endpoint and cached locally after the first fetch.
A phrase is chosen at random from PHRASES on each invocation; each variant
is cached as its own mp3 under ~/.local/share/slab/sounds/jeffrey/.
Falls back to macOS `/usr/bin/say` (with the same chosen phrase) if the
network or API is unreachable.

Run with `--prewarm` to fetch every phrase upfront so the first stinger of
each variant is instant.
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

# Edit this list to tune the variations. Keep them short — these play over
# a fading ambient bed and need to land in well under a second.
PHRASES = [
    "help me",
    "your turn",
    "psst",
    "ahem",
    "hey jeffrey",
    "knock knock",
    "you there",
    "i need you",
]

HOLD_FRAC = 0.55         # hold more of the phrase at full volume before tapering
TAIL_SILENCE_S = 0.2     # short pad; we're not about to sleep the machine

SAY_ENDPOINT = "https://aesthetic.computer/api/say"
# Cloudflare blocks the default `Python-urllib/X.Y` UA on art.aesthetic.computer.
USER_AGENT = "slab-menubar/1.0 (claude-help)"
SLAB_HOME = Path(os.environ.get("SLAB_HOME", Path.home() / ".local/share/slab"))
CACHE_DIR = SLAB_HOME / "sounds" / "jeffrey"

FALLBACK_RATE = 175      # macOS `say` rate when the Jeffrey voice is unreachable


def slug(phrase):
    return re.sub(r"[^a-z0-9]+", "-", phrase.lower()).strip("-") or "phrase"


def cache_path(phrase):
    return CACHE_DIR / f"{slug(phrase)}.mp3"


class _NoRedirect(urllib.request.HTTPRedirectHandler):
    # The `say` endpoint 302s to a Spaces CDN URL. urllib's default handler
    # would re-issue the request and may forward POST headers/body, which DO
    # rejects with 403. Capture the Location and follow it ourselves with a
    # clean GET.
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


def envelope(audio, sr):
    n = audio.size
    if n == 0:
        return audio
    hold = int(n * HOLD_FRAC)
    env = np.ones(n, dtype=np.float32)
    fade_n = n - hold
    if fade_n > 0:
        t = np.linspace(0.0, 1.0, fade_n, dtype=np.float32)
        env[hold:] = np.cos(t * np.pi * 0.5) ** 2
    tail = np.zeros(int(sr * TAIL_SILENCE_S), dtype=np.float32)
    return np.concatenate([audio * env, tail])


def prewarm():
    failures = []
    for phrase in PHRASES:
        try:
            path = fetch_jeffrey_mp3(phrase)
            print(f"  ✓ {phrase!r:24s} → {path}")
        except Exception as e:
            print(f"  ✗ {phrase!r:24s} → {e}", file=sys.stderr)
            failures.append(phrase)
    return 1 if failures else 0


def main(argv):
    if "--prewarm" in argv:
        return prewarm()

    phrase = random.choice(PHRASES)
    try:
        mp3 = fetch_jeffrey_mp3(phrase)
        audio, sr = decode_to_audio(mp3)
    except Exception as e:
        print(f"claude-help: jeffrey voice unavailable ({e}); falling back to /usr/bin/say", file=sys.stderr)
        try:
            audio, sr = synth_fallback(phrase)
        except (subprocess.CalledProcessError, FileNotFoundError) as e2:
            print(f"claude-help: fallback tts failed: {e2}", file=sys.stderr)
            return 1
    sd.play(envelope(audio, sr), sr, blocking=True)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
