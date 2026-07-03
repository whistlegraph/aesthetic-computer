// smoketest.c — harness verification voice, not a novelty candidate.
// Plain sine with an AD envelope; exists to prove the contract compiles,
// renders, tunes, and analyzes before real voices are built against it.

#include "../novelizer.h"

static void render(const NvMelody *m, float *out, int nframes) {
  for (int j = 0; j < m->count; j++) {
    const NvNote *n = &m->notes[j];
    if (n->freq <= 0) continue;
    int s0 = (int)(n->start * NV_SR);
    int len = (int)((n->dur + 0.3) * NV_SR); /* 0.3s release tail */
    double phase = 0.0, inc = n->freq / NV_SR;
    for (int i = 0; i < len && s0 + i < nframes; i++) {
      double t = (double)i / NV_SR;
      double env = (t < 0.01) ? t / 0.01 : exp(-3.0 * (t - 0.01) / (n->dur + 0.29));
      out[s0 + i] += (float)(sin(NV_TAU * phase) * env * n->vel * 0.5);
      phase += inc;
      if (phase >= 1.0) phase -= 1.0;
    }
  }
}

int main(int argc, char **argv) { return nv_main(argc, argv, "smoketest", render); }
