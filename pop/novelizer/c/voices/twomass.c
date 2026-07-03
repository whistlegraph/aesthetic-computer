// twomass.c — a self-oscillating "creature voice" for pop/novelizer.
//
// TECHNIQUE
//   Two-mass vocal-fold model (Ishizaka & Flanagan, "Synthesis of Voiced
//   Sounds from a Two-Mass Model of the Vocal Cords", Bell System Tech. J.
//   51(6), 1972) in the reduced/tuned form of Steinecke & Herzel,
//   "Bifurcations in an asymmetric vocal-fold model", J. Acoust. Soc. Am.
//   97(3), 1995. Two coupled mass-spring-dampers (the lower and upper fold
//   edges) self-oscillate under a subglottal pressure Ps. The glottal areas
//   a1,a2 gate a quasi-steady Bernoulli flow Ug; the intraglottal pressures
//   that drive the masses differ between the convergent (opening) and
//   divergent (closing) fold profiles — that asymmetry is the energy pump
//   that makes the folds oscillate. The pulsed flow Ug excites a short
//   Kelly–Lochbaum waveguide tube (Kelly & Lochbaum, "Speech Synthesis",
//   Proc. 4th ICA, 1962; scattering form after Cook / Thapen "Pink Trombone")
//   whose per-section areas realize a vowel; a small lip-radiation
//   differentiator finishes the voice.
//
//   This is NOT human speech (that is pop/voice/). It is aimed at an
//   ANIMAL/CREATURE singing voice: subglottal-pressure envelopes give
//   growl-to-whimper gestures, a flow-driven chaotic asymmetry adds
//   roughness/subharmonics only on hard-blown notes, and the tract morphs
//   between two vowel poses per note (a small "wa/eee" diphthong glide).
//
// PITCH
//   Fold frequency scales linearly with a tension parameter Q: stiffnesses
//   k -> Q*k and masses m -> m/Q, so omega = sqrt(k/m) scales by Q exactly.
//   Q = noteHz / F0_REF, where F0_REF is the Q=1 oscillation frequency
//   (calibrated against the chromatic melody). This keeps the novel timbre
//   in tune with equal temperament.
//
// PARAMETER MAP (per note, from the harness NvNote)
//   freq -> tension Q (pitch)                    vel -> subglottal Ps & growl
//   dur  -> Ps sustain length (attack/release ramps avoid boundary clicks)
//   pitch-brightness -> vowel pose; morph over the note -> formant glide
//
// WHAT MAKES IT NOVEL (vs. the batch)
//   Unlike additive sine bells, detuned-saw supersaw, the hoover/skrill FM
//   leads, Karplus zitar, carved nullnoise, or the linear gm_synth
//   waveguides, this voice's *source* is a nonlinear self-oscillating
//   physical system: the pitched buzz is an emergent limit cycle of two
//   coupled masses, not a wavetable/oscillator. Its signature is a rich
//   pulse-train glottal spectrum (dense, near-integer harmonics with a
//   spectral tilt) shaped by moving formant bands, plus period-doubling /
//   subharmonic roughness that switches on with blowing pressure — a living,
//   slightly chaotic creature grain.
//
// Self-contained C99, libm only. AC house style: normalized control math,
// state fully owned per-render.

#include "../novelizer.h"

/* ── model constants (CGS: cm, g, dyn, s) — Steinecke–Herzel base set ── */
#define OS        2               /* oversample factor for tract + folds  */
#define TR        (NV_SR * OS)    /* internal (tract) sample rate          */
#define NSEC      24              /* waveguide tube sections (creature len)*/

#define GLOT_L    1.4             /* glottal length (cm)                   */
#define D1        0.25            /* lower mass thickness (cm)             */
#define D2        0.05            /* upper mass thickness (cm)             */
#define M1_0      0.125           /* lower mass (g), scaled by 1/Q         */
#define M2_0      0.025           /* upper mass (g), scaled by 1/Q         */
#define K1_0      80000.0         /* lower stiffness (dyn/cm), scaled by Q */
#define K2_0      8000.0          /* upper stiffness (dyn/cm), scaled by Q */
#define KC_0      25000.0         /* coupling stiffness (dyn/cm), by Q     */
#define X01       0.018           /* lower rest half-displacement (cm)     */
#define X02       0.018           /* upper rest half-displacement (cm)     */
#define ZETA      0.10            /* open-phase damping ratio              */
#define ZETA_COL  0.70            /* extra damping ratio during collision  */
#define COL_K     3.0             /* collision stiffness multiplier        */
#define RHO       0.00114         /* air density (g/cm^3)                  */
#define PS_MAX    9000.0          /* peak subglottal pressure (dyn/cm^2)   */

#define F0_REF    150.3           /* Q=1 fold frequency (Hz), calibrated   */

/* tract boundary reflections */
#define GLOT_REFL 0.75
#define LIP_REFL  (-0.85)
#define SEC_DAMP  0.9985          /* mild per-section loss                 */

/* ── tiny deterministic RNG ──────────────────────────────────────────── */
static uint32_t rng_state = 0x1a2b3c4du;
static inline double frand(void) { /* -1..1 */
  rng_state = rng_state * 1664525u + 1013904223u;
  return (double)(rng_state >> 8) / 8388608.0 - 1.0;
}

/* ── vowel pose: constriction position/diameter -> tube areas ─────────── */
typedef struct { double cp, cd; } Pose; /* position 0(glottis)..1(lip), diam */

/* anchor poses along a brightness line: dark rounded -> open -> front */
static Pose pose_from_bright(double b) {
  static const Pose a0 = { 0.86, 0.45 }; /* "uu" dark, back/lip rounding   */
  static const Pose a1 = { 0.30, 1.75 }; /* "aa" open                      */
  static const Pose a2 = { 0.72, 0.50 }; /* "ii" front, high F2            */
  if (b < 0.0) b = 0.0; else if (b > 1.0) b = 1.0;
  Pose p;
  if (b < 0.5) { double t = b / 0.5;      p.cp = a0.cp + (a1.cp - a0.cp) * t; p.cd = a0.cd + (a1.cd - a0.cd) * t; }
  else         { double t = (b - 0.5) / 0.5; p.cp = a1.cp + (a2.cp - a1.cp) * t; p.cd = a1.cd + (a2.cd - a1.cd) * t; }
  return p;
}

/* fill area[NSEC] from a pose (diameter profile squared) */
static void pose_to_area(Pose p, double *area) {
  const double Drest = 1.9, cw = 0.17;
  for (int i = 0; i < NSEC; i++) {
    double x = (i + 0.5) / NSEC;                 /* 0 glottis .. 1 lip     */
    double g = (x - p.cp) / cw;
    double d = Drest - (Drest - p.cd) * exp(-g * g);
    /* gentle lip flare so radiation end stays open */
    if (x > 0.88) d += (x - 0.88) * 2.0;
    if (d < 0.2) d = 0.2;
    area[i] = d * d;
  }
}

/* ── the renderer ─────────────────────────────────────────────────────── */
static void render(const NvMelody *m, float *out, int nframes) {
  /* --- schedule: which note governs each output frame (monophonic) --- */
  int *gov = (int *)malloc(sizeof(int) * (size_t)nframes);
  for (int i = 0; i < nframes; i++) gov[i] = -1;
  double relSec = 0.055; /* release tail past dur */
  for (int j = 0; j < m->count; j++) {
    const NvNote *n = &m->notes[j];
    if (n->freq <= 0) continue;
    int s0 = (int)(n->start * NV_SR);
    int s1 = (int)((n->start + n->dur + relSec) * NV_SR);
    if (s0 < 0) s0 = 0;
    if (s1 > nframes) s1 = nframes;
    for (int i = s0; i < s1; i++) gov[i] = j; /* later notes overwrite */
  }

  /* --- fold state (seed a small asymmetry so the limit cycle starts;
         at the perfectly symmetric rest the Bernoulli force is zero) --- */
  double x1 = 0.010, v1 = 0, x2 = -0.006, v2 = 0;
  double Q = 1.0, Qs = 1.0;      /* target / smoothed tension            */
  double Ps = 0.0;               /* smoothed subglottal pressure         */
  double growlWalk = 0.0;        /* slow random walk for roughness       */

  /* --- tract state --- */
  double R[NSEC], L[NSEC], area[NSEC], refl[NSEC];
  for (int i = 0; i < NSEC; i++) { R[i] = L[i] = 0.0; area[i] = 1.0; refl[i] = 0.0; }
  double hp_x1 = 0.0, hp_y = 0.0; /* lip-radiation DC-block / diff        */
  double lp = 0.0;                /* mouth/lip lowpass, tames aspiration  */

  const double dt = 1.0 / TR;
  /* Q-independent damping constants (see header): r = 2*zeta*sqrt(m0*k0) */
  const double r1_base = 2.0 * ZETA * sqrt(M1_0 * K1_0);
  const double r2_base = 2.0 * ZETA * sqrt(M2_0 * K2_0);
  const double r1_col  = 2.0 * ZETA_COL * sqrt(M1_0 * K1_0);
  const double r2_col  = 2.0 * ZETA_COL * sqrt(M2_0 * K2_0);

  double vibPhase = 0.0;

  for (int f = 0; f < nframes; f++) {
    double t = (double)f / NV_SR;

    /* --- per-frame control from governing note --- */
    double env = 0.0, vel = 0.0, targetQ = Qs, growlAmt = 0.0;
    Pose poseNow = { 0.5, 1.0 };
    int gi = gov[f];
    if (gi >= 0) {
      const NvNote *n = &m->notes[gi];
      vel = n->vel;
      /* amplitude/pressure envelope: raised-cosine attack + release */
      double atk = n->dur * 0.4; if (atk > 0.02) atk = 0.02; if (atk < 0.002) atk = 0.002;
      double lt = t - n->start;              /* local time in note        */
      double relStart = n->dur;
      if (lt < atk)               env = 0.5 - 0.5 * cos(M_PI * (lt / atk));
      else if (lt < relStart)     env = 1.0;
      else {
        double rr = (lt - relStart) / relSec;
        env = (rr >= 1.0) ? 0.0 : 0.5 + 0.5 * cos(M_PI * rr);
      }
      if (env < 0.0) env = 0.0;

      /* pitch: linear tension map. The fold frequency droops slightly
         sub-linearly at high tension, so pre-emphasize above Q~2.5 to keep
         the upper octave in tune. */
      targetQ = n->freq / F0_REF;
      if (targetQ > 2.5) targetQ *= 1.0 + 0.045 * (targetQ - 2.5);
      double vibOnset = lt - 0.14;
      if (vibOnset > 0.0) {
        double vd = 0.006 * (vibOnset < 0.2 ? vibOnset / 0.2 : 1.0);
        targetQ *= 1.0 + vd * sin(NV_TAU * vibPhase);
      }

      /* vowel: brightness from pitch; morph opens/brightens across note */
      double b = (n->freq - 175.0) / (720.0 - 175.0);
      double mf = (n->dur > 0) ? lt / n->dur : 0.0; if (mf > 1.0) mf = 1.0; if (mf < 0.0) mf = 0.0;
      Pose pS = pose_from_bright(b);
      Pose pE = pose_from_bright(b + 0.28);
      poseNow.cp = pS.cp + (pE.cp - pS.cp) * mf;
      poseNow.cd = pS.cd + (pE.cd - pS.cd) * mf;

      /* growl/roughness engages only on hard-blown notes (vel > ~0.86),
         so the moderate-velocity tuning melodies stay clean-pitched */
      growlAmt = (vel - 0.86) / 0.14; if (growlAmt < 0.0) growlAmt = 0.0; else if (growlAmt > 1.0) growlAmt = 1.0;
    }
    vibPhase += 5.6 / NV_SR; if (vibPhase >= 1.0) vibPhase -= 1.0;

    /* smooth pressure + tension toward targets (control-rate one-poles) */
    double PsTarget = env * PS_MAX * (0.45 + 0.55 * vel);
    Ps  += (PsTarget - Ps) * 0.02;
    Qs  += (targetQ - Qs) * 0.03;
    Q = Qs; if (Q < 0.2) Q = 0.2;

    /* refresh tube areas / reflection coefficients for this frame */
    pose_to_area(poseNow, area);
    for (int i = 1; i < NSEC; i++) {
      double s = area[i - 1] + area[i];
      refl[i] = (s > 1e-9) ? (area[i - 1] - area[i]) / s : 0.0;
    }

    /* Q-scaled effective fold parameters */
    double m1e = M1_0 / Q, m2e = M2_0 / Q;
    double k1e = K1_0 * Q, k2e = K2_0 * Q, kce = KC_0 * Q;

    /* slow random walk drives the chaotic asymmetry when blown hard */
    growlWalk += (frand()) * 0.04 - growlWalk * 0.002;
    if (growlWalk > 1.0) growlWalk = 1.0; if (growlWalk < -1.0) growlWalk = -1.0;
    double asym = 1.0 + growlAmt * 0.16 * growlWalk; /* perturb upper stiffness */

    double frameOut = 0.0;

    for (int os = 0; os < OS; os++) {
      /* glottal areas (cm^2) */
      double a1 = 2.0 * GLOT_L * (X01 + x1);
      double a2 = 2.0 * GLOT_L * (X02 + x2);
      double amin = a1 < a2 ? a1 : a2;

      /* Bernoulli intraglottal pressures (Steinecke–Herzel driving) */
      double P1, P2, Ug;
      if (a1 > 0.0 && a2 > 0.0) {
        Ug = amin * sqrt(2.0 * Ps / RHO);
        if (a1 < a2) {                 /* convergent (opening): P on upper */
          P1 = 0.0;                    /* amin==a1 -> Ps(1-(a1/a1)^2)=0     */
          double rr = a1 / a2;         P2 = Ps * (1.0 - rr * rr);
        } else {                       /* divergent (closing): P on lower  */
          double rr = a2 / a1;         P1 = Ps * (1.0 - rr * rr);
          P2 = 0.0;                    /* jet separates after the minimum  */
        }
      } else {
        Ug = 0.0;                      /* glottis closed: no flow          */
        P1 = Ps;                       /* subglottal pressure reopens folds */
        P2 = 0.0;
      }

      /* driving forces on the two masses; a little pressure-scaled
         turbulence keeps the limit cycle seeded and breathes life in */
      double turb = frand() * Ps * 0.010;
      double F1 = GLOT_L * D1 * P1 + GLOT_L * D1 * turb;
      double F2 = GLOT_L * D2 * P2;

      /* collision (folds touch at midline when area <= 0) */
      double r1 = r1_base, r2 = r2_base;
      double Fc1 = 0.0, Fc2 = 0.0;
      if (a1 < 0.0) { Fc1 = -COL_K * k1e * (X01 + x1); r1 = r1_col; }
      if (a2 < 0.0) { Fc2 = -COL_K * k2e * (X02 + x2); r2 = r2_col; }

      /* equations of motion (upper stiffness perturbed for roughness) */
      double acc1 = (-k1e * x1 - kce * (x1 - x2) - r1 * v1 + F1 + Fc1) / m1e;
      double acc2 = (-k2e * asym * x2 - kce * (x2 - x1) - r2 * v2 + F2 + Fc2) / m2e;

      /* semi-implicit (symplectic) Euler for stability */
      v1 += acc1 * dt; x1 += v1 * dt;
      v2 += acc2 * dt; x2 += v2 * dt;
      /* safety clamps (tame the nonlinearity) */
      if (x1 > 0.4) { x1 = 0.4; if (v1 > 0) v1 = 0; } else if (x1 < -0.4) { x1 = -0.4; if (v1 < 0) v1 = 0; }
      if (x2 > 0.4) { x2 = 0.4; if (v2 > 0) v2 = 0; } else if (x2 < -0.4) { x2 = -0.4; if (v2 < 0) v2 = 0; }

      /* excite tract: pulsed flow + a little flow-dependent aspiration */
      double drive = Ug * 0.0022 + frand() * Ug * 1.3e-5;

      /* Kelly–Lochbaum scattering (pink-trombone form) */
      double jR0 = L[0] * GLOT_REFL + drive;
      double jLN = R[NSEC - 1] * LIP_REFL;
      double newR[NSEC], newL[NSEC];
      newR[0] = jR0;
      for (int i = 1; i < NSEC; i++) {
        double w = refl[i] * (R[i - 1] + L[i]);
        newR[i] = R[i - 1] - w;
        newL[i - 1] = L[i] + w;
      }
      newL[NSEC - 1] = jLN;
      for (int i = 0; i < NSEC; i++) { R[i] = newR[i] * SEC_DAMP; L[i] = newL[i] * SEC_DAMP; }

      /* lip radiation ~ differentiator + DC block */
      double lip = R[NSEC - 1];
      double hp = lip - hp_x1 + 0.995 * hp_y;
      hp_x1 = lip; hp_y = hp;
      lp += (hp - lp) * 0.40;   /* ~6.5 kHz mouth lowpass                 */
      frameOut += lp;
    }

    out[f] = (float)(frameOut / OS * 0.5);
  }

  free(gov);
}

int main(int argc, char **argv) { return nv_main(argc, argv, "twomass", render); }
