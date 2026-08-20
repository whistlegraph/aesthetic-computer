"""nervox — make a regulated voice sound nervous again.

@jeffrey, 2026-08-19, on the loner lane: "i think wavering notes too /
flanging and wiggling the pitches · would be nice · so the voice sounds
more nervous · lets call this 'nervox' technique · and canonize it in our
/pop tooling setup".

THE PROBLEM IT SOLVES. Every WORLD lane here ends up snapping the singer
onto a grid — cult's sing.py, loner's halo3, factory's chart.py. The snap
is what makes her notes read as NOTES against a strict beat, and it is
also what makes her sound like a machine: a held tone whose f0 is a
straight line does not occur in a human throat. Turning the snap down
gets the humanity back by giving up the regulation, which is the wrong
trade. nervox keeps the regulation and puts the tremor back on top.

TWO PARTS, and they are different instruments:

  waver()   f0 modulation. NOT vibrato — vibrato is periodic and
            confident. This is three incommensurate rates beating
            against each other plus a smoothed random walk, so the pitch
            never repeats its own wobble. That irregularity is the whole
            effect; a single clean LFO reads as an opera singer, and
            these read as someone whose voice is not quite steady.

  flange()  a modulated short delay through the voice. Sweeping comb
            notches make the timbre itself unstable, which is the part
            you hear as nerves rather than as tuning.

TWO RULES, both learned the hard way on loner:

  1. Waver HELD notes only. A slide is already moving; wobbling it makes
     mush. Pass `rate` (semitones/second, smoothed) and frames above
     GLIDE_ST_S are left alone — the same test that decides where the
     pitch snap lets go, so the two agree by construction.
  2. Ramp in. A tremor present at the attack sounds like a broken
     sample; one that grows over ~120 ms sounds like a held note going
     unsteady, which is what a nervous singer actually does.

Deterministic: everything is seeded, so a render is reproducible and two
runs of the same score are byte-comparable.

    from nervox import waver, flange
    f0 = waver(f0, frame_s, rate=rate)          # cents of tremor on f0
    x  = flange(x, fs)                          # comb sweep on the audio
"""

import numpy as np

# the same threshold the pitch snap uses to decide she is sliding
GLIDE_ST_S = 18.0

WAVER_CENTS = 22.0      # depth of the tremor, cents peak
WAVER_RATES = (4.3, 5.9, 7.1)   # incommensurate, so the wobble never repeats
WAVER_DRIFT_HZ = 0.7    # how fast the random walk under it moves
WAVER_RAMP_S = 0.120    # a tremor at the attack reads as a broken sample

FLANGE_MS = (0.6, 4.5)  # sweep range of the delay
FLANGE_HZ = 0.23        # the sweep
FLANGE_DRIFT_HZ = 0.07  # …and a slower drift under it, so it never cycles
FLANGE_FB = 0.35
FLANGE_MIX = 0.38


def _walk(n, rng, hz, frame_s):
    """A smoothed random walk in [-1, 1] — the part that will not repeat."""
    if n <= 0:
        return np.zeros(0)
    step = max(1, int(round(1.0 / max(hz, 1e-6) / frame_s)))
    knots = rng.uniform(-1.0, 1.0, size=n // step + 2)
    w = np.interp(np.arange(n), np.arange(len(knots)) * step, knots)
    k = max(1, step // 2)
    return np.convolve(w, np.ones(k) / k, mode="same")


def waver(f0, frame_s, rate=None, cents=WAVER_CENTS, seed=0x10AE,
          voiced=None, ramp_s=WAVER_RAMP_S):
    """Put an unsteady tremor on a corrected f0 contour.

    f0      Hz per frame, 0 where unvoiced
    rate    |df0/dt| in semitones/second per frame; frames above
            GLIDE_ST_S are left alone (she is sliding, not holding)
    """
    f0 = np.asarray(f0, dtype=float)
    n = len(f0)
    if n == 0:
        return f0
    v = (f0 > 0) if voiced is None else np.asarray(voiced, dtype=bool)[:n]
    rng = np.random.default_rng(seed)
    t = np.arange(n) * frame_s

    # three rates beating, each with its own slowly wandering depth
    trem = np.zeros(n)
    for i, hz in enumerate(WAVER_RATES):
        depth = 0.55 + 0.45 * _walk(n, rng, WAVER_DRIFT_HZ, frame_s)
        trem += depth * np.sin(2 * np.pi * hz * t + rng.uniform(0, 2 * np.pi))
    trem /= len(WAVER_RATES)
    trem = 0.75 * trem + 0.25 * _walk(n, rng, WAVER_DRIFT_HZ * 1.6, frame_s)

    # held notes only — a slide is already moving
    hold = np.ones(n)
    if rate is not None:
        r = np.asarray(rate, dtype=float)[:n]
        hold = np.clip(1.0 - (r - GLIDE_ST_S) / GLIDE_ST_S, 0.0, 1.0)

    # …and ramp in from each voiced onset
    ramp = np.zeros(n)
    k = max(1, int(round(ramp_s / frame_s)))
    run = 0
    for i in range(n):
        run = run + 1 if v[i] else 0
        ramp[i] = min(1.0, run / k)

    out = f0.copy()
    out[v] = f0[v] * 2.0 ** ((cents * trem * hold * ramp)[v] / 1200.0)
    return out


def flange(x, fs, depth_ms=FLANGE_MS, hz=FLANGE_HZ, fb=FLANGE_FB,
           mix=FLANGE_MIX, seed=0x11AE):
    """A modulated short delay — the timbre goes unstable, not the tuning."""
    x = np.asarray(x, dtype=float)
    n = len(x)
    if n == 0:
        return x
    t = np.arange(n) / float(fs)
    lo, hi = depth_ms[0] / 1000.0, depth_ms[1] / 1000.0
    rng = np.random.default_rng(seed)
    ph = rng.uniform(0, 2 * np.pi)
    sweep = 0.5 - 0.5 * np.cos(2 * np.pi * hz * t + ph)
    sweep = 0.78 * sweep + 0.22 * (0.5 - 0.5 * np.cos(2 * np.pi * FLANGE_DRIFT_HZ * t))
    d = (lo + (hi - lo) * sweep) * fs

    # linear-interpolated delay line with feedback, written in place so the
    # feedback path hears its own output the way a real flanger does
    y = np.zeros(n)
    buf = np.zeros(n)
    for i in range(n):
        di = d[i]
        j = i - di
        if j < 1:
            v = 0.0
        else:
            j0 = int(j)
            fr = j - j0
            v = buf[j0] * (1 - fr) + buf[j0 + 1] * fr if j0 + 1 < n else buf[j0]
        buf[i] = x[i] + fb * v
        y[i] = v
    return (1.0 - mix) * x + mix * y
