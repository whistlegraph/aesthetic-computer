// bus.mjs — buffer-mock-synth that implements the AC `sound.synth(...)`
// contract over a Float32Array buffer. Lets server-side renderers drive
// the same synthesis calls that the web AudioContext + fedac/native
// audio.c implement, so a single voice description plays identically
// everywhere.
//
// Contract (matches the AC web/native runtime):
//   sound.synth({
//     type:     "sine" | "square" | "triangle" | "sawtooth" | "noise"
//     tone:     Hz
//     duration: seconds
//     volume:   scalar (0..1+)
//     attack:   seconds (linear)
//     decay:    seconds (exponential tail after attack)
//     pan:      -1..1 (ignored in mono out)
//     phase:    optional 0..1 initial phase (additive AC extension —
//               native runtimes that don't honor it default to 0)
//   })
//
// Total event length = max(duration, attack + decay).

const DEFAULT_SAMPLE_RATE = 48_000;

export function makeBufferSynth(out, startSec, sampleRate = DEFAULT_SAMPLE_RATE, noiseRng = Math.random) {
  return {
    synth: (params) => {
      const type     = params?.type || "sine";
      const tone     = Number(params?.tone) || 440;
      const duration = Number(params?.duration);
      const volume   = Number(params?.volume ?? 1);
      const attack   = Math.max(0, Number(params?.attack ?? 0.001));
      const decay    = Number.isFinite(params?.decay)
        ? Math.max(0, Number(params.decay))
        : Math.max(0.001, (Number.isFinite(duration) ? duration : 0.05) - attack);
      const initPhase = Number.isFinite(params?.phase) ? params.phase : 0;
      if (!Number.isFinite(volume) || volume === 0) return null;
      if (!Number.isFinite(duration) || duration <= 0) return null;

      const durSamples = Math.ceil(duration * sampleRate);
      const attS       = Math.max(1, Math.floor(attack * sampleRate));
      const decS       = Math.max(1, Math.floor(decay  * sampleRate));
      // Match fedac/native/src/audio.c compute_envelope():
      //   linear attack (0 → 1 over `attack` seconds)
      //   hold at 1 until `duration - decay`
      //   linear decay (1 → 0 over `decay` seconds ending at duration)
      // When attack + decay > duration, native clamps decay_start to 0
      // (so the decay starts immediately after attack, truncated at
      // duration). We instead AUTO-EXTEND the window to attack+decay so
      // the envelope completes to true silence without clicks at end —
      // matches the AC web AudioContext + ADSR-style releases.
      const ns         = Math.max(durSamples, attS + decS);
      const decayStart = ns - decS; // >= attS by construction (attS + decS <= ns)
      const startIdx   = Math.floor(startSec * sampleRate);
      const omega      = (2 * Math.PI * tone) / sampleRate;
      const phaseInc   = tone / sampleRate;

      // SVF bandpass for noise voices (matches trap.mjs / percussion fan-out).
      const fcSafe   = Math.min(Math.max(40, tone), sampleRate / 6);
      const fParam   = 2 * Math.sin(Math.PI * fcSafe / sampleRate);
      const damp     = 1 / 4;
      let svfLow = 0, svfBand = 0;

      let phaseAcc = initPhase;

      for (let i = 0; i < ns; i++) {
        const dst = startIdx + i;
        if (dst < 0 || dst >= out.length) continue;

        // Native-portable envelope: linear attack → hold → linear decay.
        let env;
        if (i < attS) {
          env = i / attS;
        } else if (i < decayStart) {
          env = 1;
        } else {
          env = 1 - (i - decayStart) / decS;
          if (env <= 0) continue;
        }

        let s;
        switch (type) {
          case "sine":     s = Math.sin(omega * i + 2 * Math.PI * initPhase); break;
          case "square":   s = Math.sin(omega * i + 2 * Math.PI * initPhase) >= 0 ? 1 : -1; break;
          case "triangle": { const ph = (phaseAcc + i * phaseInc) % 1; s = ph < 0.5 ? 4 * ph - 1 : 3 - 4 * ph; break; }
          case "sawtooth": { const ph = (phaseAcc + i * phaseInc) % 1; s = 2 * ph - 1; break; }
          case "noise":
          case "noise-white": {
            const w = noiseRng() * 2 - 1;
            const high = w - svfLow - damp * svfBand;
            svfBand += fParam * high;
            svfLow  += fParam * svfBand;
            s = svfBand * 1.5;
            break;
          }
          default: s = 0;
        }
        out[dst] += s * env * volume;
      }
      return null;
    },
  };
}
