#!/usr/bin/env python3
"""Notification stinger: speak "help me" with a short cosine fade-out.

Invoked from claude-notify.sh when Claude Code emits a Notification hook —
i.e. the agent has paused and is waiting on user feedback (permission prompt
or idle input). Unlike claude-tired.py, this is an attention cue, not a
lullaby: the delivery is brisker and the tail silence is small. Ambient has
already been asked to fade via SIGTERM by the caller, so this phrase plays
over the dissolving bed.
"""

import os
import subprocess
import sys
import tempfile
import wave

import numpy as np
import sounddevice as sd

TEXT = "help me"
RATE = 175               # brisker than the sleep stinger — it's a request, not a sigh
HOLD_FRAC = 0.55         # hold more of the phrase at full volume before tapering
TAIL_SILENCE_S = 0.2     # short pad; we're not about to sleep the machine


def synth():
    with tempfile.TemporaryDirectory() as tmp:
        aiff = os.path.join(tmp, 'help.aiff')
        wav = os.path.join(tmp, 'help.wav')
        subprocess.run(
            ['/usr/bin/say', '-r', str(RATE), '-o', aiff, TEXT],
            check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
        )
        subprocess.run(
            ['/usr/bin/afconvert', '-f', 'WAVE', '-d', 'LEI16@22050', aiff, wav],
            check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
        )
        with wave.open(wav, 'rb') as wf:
            sr = wf.getframerate()
            nch = wf.getnchannels()
            raw = wf.readframes(wf.getnframes())
    audio = np.frombuffer(raw, dtype=np.int16).astype(np.float32) / 32768.0
    if nch > 1:
        audio = audio.reshape(-1, nch).mean(axis=1)
    return audio, sr


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


def main():
    try:
        audio, sr = synth()
    except (subprocess.CalledProcessError, FileNotFoundError) as e:
        print(f"claude-help: tts failed: {e}", file=sys.stderr)
        return 1
    sd.play(envelope(audio, sr), sr, blocking=True)
    return 0


if __name__ == '__main__':
    sys.exit(main())
