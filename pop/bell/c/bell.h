// bell.h — physically-modeled bell voice (axisymmetric thin-shell FEM).
//
// A bell is a surface of revolution, so the 3-D shell problem decouples, by a
// Fourier expansion in the angular coordinate theta, into one 1-D meridian
// problem per circumferential order m. For each m we assemble small stiffness
// (K) and mass (M) matrices along the meridian and solve the generalized
// symmetric eigenproblem  K phi = omega^2 M phi. The eigenvalues are the modal
// frequencies; the eigenvectors are the meridian mode shapes — the same shapes
// the 3-D visualization animates. Material parameters (Young's modulus, density,
// Poisson ratio, wall thickness, damping) enter every term, so the timbre and
// decay genuinely follow the physics.
//
// Zero dependencies beyond libm. See README.md for the formulation + citations.

#ifndef BELL_H
#define BELL_H

#define BELL_MAX_NODES 128 // meridian discretization stations
#define BELL_MAX_MODES 64  // partials retained after the eigensolve

// --- Material ---------------------------------------------------------------
// E in pascals, rho in kg/m^3, nu dimensionless, loss = damping loss factor eta
// (amplitude decay rate delta = pi * f * eta, so tau = 1 / delta).
typedef struct {
  double E;
  double rho;
  double nu;
  double loss;
  char name[32];
} BellMaterial;

// --- Geometry ---------------------------------------------------------------
// A meridian profile: n stations sampled bottom (mouth) to top (crown), each
// with axial coord z, radius r, and wall thickness h — all in metres.
typedef struct {
  int n;
  double z[BELL_MAX_NODES];
  double r[BELL_MAX_NODES];
  double h[BELL_MAX_NODES];
  char name[32];
} BellGeometry;

// --- A single vibrational mode ---------------------------------------------
typedef struct {
  int m;       // circumferential order (number of nodal meridians)
  double freq; // Hz
  double tau;  // amplitude e-fold time (s), from material loss factor
  double part; // strike participation (normal shape sampled at the strike point)
  int nshape;  // number of meridian shape samples (== geometry n)
  double wshape[BELL_MAX_NODES]; // normal (radial) displacement along meridian
  double ushape[BELL_MAX_NODES]; // meridional displacement along meridian
} BellMode;

// --- The full solved mode set ----------------------------------------------
typedef struct {
  int count;
  BellMode mode[BELL_MAX_MODES];
  int strike_index;   // index of the nominal / "strike note" mode
  double strike_freq; // its frequency (Hz)
} BellModes;

// --- Presets ----------------------------------------------------------------
void bell_default_geometry(BellGeometry *g);            // a church-bell profile
int bell_geometry_preset(BellGeometry *g, const char *name);
void bell_default_material(BellMaterial *mat);          // bell bronze
int bell_material_preset(BellMaterial *mat, const char *name);

// --- Solve / tune / render --------------------------------------------------
// Returns mode count (>0) on success, <0 on failure. max_m caps circumferential
// orders examined; max_modes caps retained partials.
int bell_solve_modes(const BellGeometry *g, const BellMaterial *mat, int max_m,
                     int max_modes, BellModes *out);

// Uniformly rescale every modal frequency so the strike note equals target_freq.
// Physical (a uniform geometric scale shifts all modes by the same factor), so
// the inharmonic ratio set is preserved.
void bell_retune(BellModes *modes, double target_freq);

// Render a single strike into stereo float buffers (length nsamp each). Returns
// the number of samples written. strike_vel in [0,1].
long bell_render(const BellModes *modes, double strike_vel, double sr,
                 double dur, float *L, float *R, long nsamp);

// Export geometry + solved modes (incl. mode shapes) as JSON for the viz.
int bell_export_modes_json(const BellGeometry *g, const BellMaterial *mat,
                           const BellModes *modes, const char *path);

// --- Self-test: validates the eigensolver against analytic limits ----------
// Returns 0 if all checks pass within tolerance, nonzero otherwise.
int bell_selftest(int verbose);

#endif // BELL_H
