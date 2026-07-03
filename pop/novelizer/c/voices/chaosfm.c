// chaosfm.c — single-operator feedback FM played at the edge of chaos.
//
// TECHNIQUE
//   One sine operator whose own output is fed back into its phase:
//
//       y[n] = sin(2π·phase[n] + β · fb[n]),   fb[n] = (y[n-1] + y[n-2]) / 2
//
//   The two-sample average is Yamaha's trick for taming the period-2
//   parasitic oscillation that raw one-sample feedback develops. Below
//   β ≈ 1.5 the operator is a smoothly brightening quasi-sawtooth; pushed
//   further it period-doubles and shatters into band-limited chaos while
//   the fundamental stays pinned to the note frequency.
//
// CITATION / LINEAGE
//   - Norio Tomisawa, "Tone production method for an electronic musical
//     instrument", US Patent 4,249,447 (1981) — Yamaha DX7 operator
//     self-feedback, including the averaged-feedback stabilizer.
//   - Dan Slater, "Chaotic Sound Synthesis", Computer Music Journal 22(2),
//     1998 — driving feedback FM past its bifurcation point as a musical
//     resource.
//
// WHAT MAKES IT NOVEL HERE
//   The AC stable already has pure sines (hellsine), stacked saws
//   (supersaw/hoover), formant FM (skrill), plucked strings (zitar),
//   carved noise (nullnoise) and waveguides (gm_synth). None of them play
//   a BIFURCATION as the expressive axis. chaosfm envelopes β per note so
//   every note is a journey: sine → saw → controlled chaos → back. A
//   second feedback operator detuned a few cents runs the same journey
//   with slightly less β, so in the chaotic bloom the two attractors beat
//   against each other. Velocity pushes β deeper toward chaos; pitch stays
//   centered because feedback FM never detunes its fundamental (a quiet
//   sine anchor at f0 underlines that).
//
// PARAMETER MAP
//   BETA_FLOOR   0.35              tone at note edges (nearly pure sine)
//   beta peak    1.0 + 2.0·vel     velocity rides the bifurcation depth
//   register     ×1/(1+f/2600)     high notes bloom a little shallower
//   beta shape   sin(π·t/dur)^1.1  bloom mid-note, return before release
//   op2          +3.6 cents, β×0.85, mix 0.45 — beating chaos
//   anchor       0.12 · sin(2πf t) — pitch center reference
//   fb filter    avg(y1,y2) → one-pole LP (k=0.5, ~5.3 kHz): the average
//                is Tomisawa's one-zero at fs/2; the pole also damps the
//                period-3 parasite that otherwise whistles at fs/3
//   amp env      5 ms attack, gentle exp decay, 60 ms cosine release
//   DC blocker   y = x - x' + 0.9995·y'  (chaos is asymmetric; kill drift)

#include "../novelizer.h"

#define BETA_FLOOR 0.35
#define ATTACK_SEC 0.005
#define RELEASE_SEC 0.060
#define OP2_DETUNE 1.00208 /* ~ +3.6 cents */
#define OP2_MIX 0.45
#define ANCHOR_MIX 0.12

/* One self-feedback sine operator (Tomisawa averaged feedback). */
#define FB_LP_K 0.5 /* one-pole in the loop, ~5.3 kHz at 48k */

typedef struct {
  double phase; /* normalized 0..1, advanced by freq / NV_SR */
  double inc;
  double y1, y2; /* previous two outputs for the averaged feedback */
  double lp;     /* one-pole state smoothing the feedback path */
} FbOp;

static void fbop_init(FbOp *o, double freq) {
  o->phase = 0.0;
  o->inc = freq / NV_SR;
  o->y1 = o->y2 = 0.0;
  o->lp = 0.0;
}

static double fbop_tick(FbOp *o, double beta) {
  o->lp += FB_LP_K * (0.5 * (o->y1 + o->y2) - o->lp);
  double y = sin(NV_TAU * o->phase + beta * o->lp);
  o->y2 = o->y1;
  o->y1 = y;
  o->phase += o->inc;
  if (o->phase >= 1.0) o->phase -= 1.0;
  return y;
}

static void render_note(const NvNote *n, float *out, int nframes) {
  int s0 = (int)(n->start * NV_SR);
  int hold = (int)(n->dur * NV_SR);
  int rel = (int)(RELEASE_SEC * NV_SR);
  int len = hold + rel;
  int atk = (int)(ATTACK_SEC * NV_SR);
  if (atk < 1) atk = 1;

  /* velocity → loudness and bifurcation depth; register tames the top */
  double amp = pow(n->vel, 1.3);
  double beta_peak = (1.0 + 2.0 * n->vel) / (1.0 + n->freq / 2600.0);
  if (beta_peak < BETA_FLOOR) beta_peak = BETA_FLOOR;

  FbOp op1, op2, anchor;
  fbop_init(&op1, n->freq);
  fbop_init(&op2, n->freq * OP2_DETUNE);
  fbop_init(&anchor, n->freq);

  double dc_x1 = 0.0, dc_y1 = 0.0; /* DC blocker state */

  for (int i = 0; i < len && s0 + i < nframes; i++) {
    double t = (double)i / NV_SR;

    /* β journey: bloom from tone into chaos and back across the hold */
    double u = t / n->dur;
    if (u > 1.0) u = 1.0;
    double s = pow(sin(M_PI * u), 1.1);
    double beta = BETA_FLOOR + (beta_peak - BETA_FLOOR) * s;

    double x = fbop_tick(&op1, beta) + OP2_MIX * fbop_tick(&op2, beta * 0.85);
    x /= (1.0 + OP2_MIX);
    x += ANCHOR_MIX * sin(NV_TAU * anchor.phase);
    anchor.phase += anchor.inc;
    if (anchor.phase >= 1.0) anchor.phase -= 1.0;

    /* DC blocker (chaotic regime is asymmetric) */
    double y = x - dc_x1 + 0.9995 * dc_y1;
    dc_x1 = x;
    dc_y1 = y;

    /* amplitude: linear attack, gentle decay over the hold, cos release */
    double env;
    if (i < atk) env = (double)i / atk;
    else env = exp(-1.2 * (t - ATTACK_SEC) / (n->dur + RELEASE_SEC));
    if (i >= hold) {
      double r = (double)(i - hold) / rel; /* 0..1 */
      env *= 0.5 * (1.0 + cos(M_PI * r));
    }

    out[s0 + i] += (float)(y * env * amp * 0.5);
  }
}

static void render(const NvMelody *m, float *out, int nframes) {
  for (int j = 0; j < m->count; j++) {
    const NvNote *n = &m->notes[j];
    if (n->freq <= 0) continue;
    render_note(n, out, nframes);
  }
}

int main(int argc, char **argv) { return nv_main(argc, argv, "chaosfm", render); }
