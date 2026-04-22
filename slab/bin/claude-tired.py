#!/usr/bin/env python3
"""Sleep stinger: speak "i'm tired" with a cosine fade-out tail.

Invoked from claude-stop.sh in the lid-closed "all work done" branch, right
before `claude-sleep now`. The voice starts at full amplitude, then the tail
of the phrase tapers smoothly to silence and a short silence pad follows, so
`pmset sleepnow` fires after the audio has already faded — a gentle dissolve
instead of an abrupt cut.
"""

import os
import subprocess
import sys
import tempfile
import wave

import numpy as np
import sounddevice as sd

TEXT = "i'm tired"
RATE = 150               # slightly slower than default for a sleepier delivery
HOLD_FRAC = 0.35         # first chunk of the phrase held at full volume
TAIL_SILENCE_S = 0.7     # silence pad appended after the faded phrase


def synth():
    with tempfile.TemporaryDirectory() as tmp:
        aiff = os.path.join(tmp, 'tired.aiff')
        wav = os.path.join(tmp, 'tired.wav')
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
        env[hold:] = np.cos(t * np.pi * 0.5) ** 2  # equal-power taper
    tail = np.zeros(int(sr * TAIL_SILENCE_S), dtype=np.float32)
    return np.concatenate([audio * env, tail])


def main():
    try:
        audio, sr = synth()
    except (subprocess.CalledProcessError, FileNotFoundError) as e:
        print(f"claude-tired: tts failed: {e}", file=sys.stderr)
        return 1
    sd.play(envelope(audio, sr), sr, blocking=True)
    return 0


if __name__ == '__main__':
    sys.exit(main())
