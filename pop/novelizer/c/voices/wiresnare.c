// wiresnare.c — coupled membrane + snare-wire contact model.
//
// TECHNIQUE
//   A modal batter head (7 inharmonic circular-membrane modes, fundamental
//   at the note frequency octave-folded into snare register) coupled to a
//   bank of 12 light mass-spring snare WIRES resting just below the head.
//   The coupling is a one-sided (unilateral) contact nonlinearity: when a
//   wire's displacement crosses the head's while approaching, it reflects
//   off the moving head with restitution e = 0.5 — a single-sample impulse
//   (broadband by construction) that is also injected into every head mode.
//   The buzz is EMERGENT: the stick rings the head, the head throws the
//   wires, the wires chatter back with velocity-dependent density and a
//   raggedy natural decay — there is no noise generator anywhere here.
//   (A Hertzian k·p^1.5 penalty contact was tried first; under explicit
//   per-sample integration it pumps energy and the buzz self-sustains.
//   The impulsive restitution form is strictly dissipative, so the rattle
//   is guaranteed to die with the head.)
//
// CITATION / LINEAGE
//   - T. D. Rossing, I. Bork, H. Zhao, D. O. Fystrom, "Acoustics of snare
//     drums", J. Acoust. Soc. Am. 92(1), 1992 — two-membrane snare physics;
//     snare-wire rattle as repeated unilateral head/wire collisions.
//   - S. Bilbao, A. Torin, V. Chatziioannou, "Numerical modeling of
//     collisions in musical instruments", Acta Acustica 101(1), 2015 —
//     unilateral head/wire collision modeling incl. the snare-drum example;
//     their core demand (the contact scheme must not create energy) is met
//     here with an impulsive restitution resolution instead of their
//     penalty potential, which is not stable under naive explicit Euler.
//
// WHAT MAKES IT NOVEL HERE
//   Every existing AC snare-ish voice makes its "snap" from a noise source:
//   nullnoise carves filtered pink/white/velvet bursts, percussion.mjs and
//   gm_synth shape noise through envelopes/filters, hellsine has no snare at
//   all. wiresnare has NO noise generator — its rattle is a deterministic
//   chaotic contact process, so the tail audibly granulates (discrete click
//   striations that thin out as the head settles) instead of hissing, and
//   velocity buys buzz DENSITY and DURATION, not just level. Vs batch 1:
//   twomass is also a collision model, but of two masses inside one
//   oscillator; here collision is the COUPLING between a resonant body and
//   a 12-element rattle bank (declared lineage: same penalty-force family).
//
// PARAMETER MAP
//   fold           note freq octave-folded up to >= 120 Hz (head fundamental)
//   MODE ratios    1, 1.593, 2.135, 2.295, 2.653, 2.917, 3.155 (+/-0.6% hash)
//                  ideal circular-membrane ratios (Rossing)
//   mode T (amp e-fold)  90 ms fundamental, ~ratio^-0.6 above
//   strike         1.0 ms raised-cosine push, force = vel^1.1, downward
//   WIRES (12)     natural freq 320..1750 Hz (hash log-spread), ~60 ms damp
//   gap_i          0.04..0.30 head units (hash) — sets buzz threshold, so
//                  soft hits only reach the closest wires (fewer, sparser
//                  contacts) and hard hits engage the whole bank
//   contact        impulse j = (1+e)·v_rel, e = 0.5, clamp 1.0; head
//                  injection 0.02·j (well below regeneration threshold)
//   output         0.70*head + 4.0*highpassed contact clicks, DC-blocked
//   note window    900 ms per hit (additive overlap, 10 ms end fade)

#include "../novelizer.h"

#define N_MODES 7
#define N_WIRES 12
#define NOTE_SEC 0.90
#define STRIKE_SEC 0.001
#define FOLD_MIN 120.0

/* ideal circular membrane mode ratios: (01)(11)(21)(02)(31)(12)(41) */
static const double mode_ratio[N_MODES] = {
  1.0, 1.593, 2.135, 2.295, 2.653, 2.917, 3.155
};
static const double mode_gain[N_MODES] = {
  1.0, 0.62, 0.46, 0.40, 0.31, 0.26, 0.21
};
static const double mode_strike[N_MODES] = {
  1.0, 0.92, 0.85, 0.80, 0.74, 0.66, 0.58
};

/* contact: impulsive unilateral collision with restitution < 1, so every
   wire/head impact strictly LOSES energy — the buzz must decay with the
   head. Each impact is a single-sample impulse: broadband by construction. */
#define RES 0.50       /* coefficient of restitution */
#define J_MAX 1.0      /* impulse clamp (stability) */
#define INJ 0.02       /* impact impulse -> head-mode injection (kept well
                          below the regeneration threshold: the wire->head->
                          wire loop must have gain < 1 or the buzz self-
                          sustains — measured, not guessed) */

/* mix */
#define HEAD_MIX 0.70
#define CLICK_MIX 4.0
#define CLICK_HP_K 0.12 /* one-pole HP on the click bus (~1 kHz) */

/* ── deterministic per-note hash (splitmix64) ─────────────────────── */
static uint64_t sm64(uint64_t x) {
  x += 0x9E3779B97F4A7C15ULL;
  x = (x ^ (x >> 30)) * 0xBF58476D1CE4E5B9ULL;
  x = (x ^ (x >> 27)) * 0x94D049BB133111EBULL;
  return x ^ (x >> 31);
}
static double h01(uint64_t seed, int slot) { /* uniform 0..1 */
  return (double)(sm64(seed + (uint64_t)slot * 0x632BE59BD9B4E019ULL) >> 11)
         / 9007199254740992.0;
}

typedef struct { /* complex-rotation resonator (one head mode) */
  double cr, ci;   /* state */
  double rw, iw;   /* rotation r*e^{jw} */
} Mode;

typedef struct { /* one snare wire: light damped mass-spring */
  double pos, vel;
  double k2;    /* (2*pi*fw/SR)^2 spring constant */
  double damp;  /* velocity retention per sample */
  double rest;  /* equilibrium = -gap below the head */
} Wire;

static void render_note(const NvNote *n, int idx, float *out, int nframes) {
  int s0 = (int)(n->start * NV_SR);
  if (s0 >= nframes) return;
  int len = (int)(NOTE_SEC * NV_SR);
  if (s0 + len > nframes) len = nframes - s0;
  int fade = (int)(0.010 * NV_SR);

  /* octave-fold the note fundamental up into drum-head register */
  double f0 = n->freq;
  while (f0 < FOLD_MIN) f0 *= 2.0;

  uint64_t seed = sm64((uint64_t)idx * 0x100000001B3ULL
                       ^ (uint64_t)(n->freq * 1024.0));

  /* head modes */
  Mode md[N_MODES];
  for (int k = 0; k < N_MODES; k++) {
    double jit = 1.0 + 0.012 * (h01(seed, 100 + k) - 0.5); /* +/-0.6% */
    double fk = f0 * mode_ratio[k] * jit;
    if (fk > 0.45 * NV_SR) fk = 0.45 * NV_SR;
    double tau = 0.090 * pow(mode_ratio[k], -0.6); /* amp e-fold, sec */
    double r = exp(-1.0 / (tau * NV_SR));
    double w = NV_TAU * fk / NV_SR;
    md[k].cr = md[k].ci = 0.0;
    md[k].rw = r * cos(w);
    md[k].iw = r * sin(w);
  }

  /* snare wires: rest just below the head, hash-spread freq + gap */
  Wire wr[N_WIRES];
  for (int i = 0; i < N_WIRES; i++) {
    double u = ((double)i + h01(seed, 200 + i)) / N_WIRES;   /* stratified */
    double fw = 320.0 * pow(1750.0 / 320.0, u);              /* log spread */
    double ph = NV_TAU * fw / NV_SR;
    wr[i].k2 = ph * ph;
    wr[i].damp = exp(-1.0 / (0.060 * NV_SR)); /* ~60 ms free e-fold */
    wr[i].rest = -(0.04 + 0.26 * h01(seed, 300 + i));
    wr[i].pos = wr[i].rest;
    wr[i].vel = 0.0;
  }

  /* strike: raised-cosine downward push, unit-area normalized */
  int sl = (int)(STRIKE_SEC * NV_SR);
  if (sl < 4) sl = 4;
  double force = pow(n->vel, 1.1);
  double amp = pow(n->vel, 1.15); /* output loudness */

  double inj = 0.0;           /* wire->head injection (1-sample delay) */
  double hp_lp = 0.0;         /* click-bus highpass state */
  double dc_x1 = 0.0, dc_y1 = 0.0;
  double hprev = 0.0;         /* head displacement history for head velocity */

  for (int i = 0; i < len; i++) {
    double strike = 0.0;
    if (i < sl)
      strike = -force * (2.0 / sl) * 0.5 * (1.0 - cos(NV_TAU * i / sl));

    /* head modes: rotate, then take input (strike + wire kicks) */
    double h = 0.0;
    for (int k = 0; k < N_MODES; k++) {
      double in = strike * mode_strike[k] + inj * INJ;
      double nr = md[k].cr * md[k].rw - md[k].ci * md[k].iw;
      double ni = md[k].cr * md[k].iw + md[k].ci * md[k].rw;
      md[k].cr = nr;
      md[k].ci = ni + in;
      h += mode_gain[k] * md[k].ci;
    }

    /* wires: free oscillation + unilateral impulsive collision against
       the head (Rossing's rattle; restitution < 1 keeps it dissipative) */
    double hvel = h - hprev;
    hprev = h;
    inj = 0.0;
    double click = 0.0;
    for (int w = 0; w < N_WIRES; w++) {
      wr[w].vel += -wr[w].k2 * (wr[w].pos - wr[w].rest);
      wr[w].vel *= wr[w].damp;
      wr[w].pos += wr[w].vel;
      double p = wr[w].pos - h; /* head swings down into the wire */
      if (p > 0.0) {
        double vrel = wr[w].vel - hvel; /* approach speed */
        if (vrel > 0.0) {
          double j = (1.0 + RES) * vrel; /* impulse magnitude */
          if (j > J_MAX) j = J_MAX;
          wr[w].vel = hvel - RES * vrel; /* reflect off the moving head */
          inj += j;    /* head hears the impact next sample */
          click += j;
        }
        wr[w].pos = h; /* resolve penetration */
      }
      /* stability clamps (never expected to trigger) */
      if (wr[w].pos > 5.0) wr[w].pos = 5.0;
      else if (wr[w].pos < -5.0) wr[w].pos = -5.0;
      if (wr[w].vel > 3.0) wr[w].vel = 3.0;
      else if (wr[w].vel < -3.0) wr[w].vel = -3.0;
    }

    /* click bus: one-pole highpass keeps the snap, drops thump */
    hp_lp += CLICK_HP_K * (click - hp_lp);
    double snap = click - hp_lp;

    double x = HEAD_MIX * h + CLICK_MIX * snap;

    /* DC blocker */
    double y = x - dc_x1 + 0.9995 * dc_y1;
    dc_x1 = x;
    dc_y1 = y;

    double g = amp * 0.35;
    if (i > len - fade) g *= (double)(len - i) / fade; /* end-of-window fade */
    out[s0 + i] += (float)(y * g);
  }
}

static void render(const NvMelody *m, float *out, int nframes) {
  for (int j = 0; j < m->count; j++) {
    const NvNote *n = &m->notes[j];
    if (n->freq <= 0) continue;
    render_note(n, j, out, nframes);
  }
}

int main(int argc, char **argv) { return nv_main(argc, argv, "wiresnare", render); }
