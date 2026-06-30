// bell.c — physically-modeled bell voice. See bell.h for the overview.
//
// Build: ./build.sh   (cc -O3 -std=c11 -Wall -Wextra -o bell bell.c -lm)
//
// Sections:
//   1. Small dense linear algebra (Cholesky, Jacobi, generalized eigensolve)
//   2. Analytic-limit self-tests (gate correctness before trusting the shell)
//   3. Axisymmetric shell FEM assembly  -> appended in stage 2
//   4. Strike / modal render / export   -> appended in stage 2
//   5. Presets + CLI                    -> appended in stage 2

#include "bell.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define TAU (2.0 * M_PI)

// ===========================================================================
// 1. Linear algebra (row-major flat arrays, A[i*n + j])
// ===========================================================================

// Cholesky factor of SPD matrix M -> lower-triangular L (M = L L^T).
// Returns 0 on success, -1 if not positive-definite.
static int chol(const double *M, double *L, int n) {
  for (int i = 0; i < n * n; i++) L[i] = 0.0;
  for (int j = 0; j < n; j++) {
    double d = M[j * n + j];
    for (int k = 0; k < j; k++) d -= L[j * n + k] * L[j * n + k];
    if (d <= 0.0) return -1;
    L[j * n + j] = sqrt(d);
    for (int i = j + 1; i < n; i++) {
      double s = M[i * n + j];
      for (int k = 0; k < j; k++) s -= L[i * n + k] * L[j * n + k];
      L[i * n + j] = s / L[j * n + j];
    }
  }
  return 0;
}

// Forward solve L y = b (L lower-triangular), in place into y.
static void fwd(const double *L, const double *b, double *y, int n) {
  for (int i = 0; i < n; i++) {
    double s = b[i];
    for (int k = 0; k < i; k++) s -= L[i * n + k] * y[k];
    y[i] = s / L[i * n + i];
  }
}

// Back solve L^T x = b (L lower-triangular so L^T is upper), in place into x.
static void bwd_t(const double *L, const double *b, double *x, int n) {
  for (int i = n - 1; i >= 0; i--) {
    double s = b[i];
    for (int k = i + 1; k < n; k++) s -= L[k * n + i] * x[k];
    x[i] = s / L[i * n + i];
  }
}

// Cyclic Jacobi eigensolve of symmetric A (n x n). Fills eval[n] and evec
// (columns are eigenvectors, evec[i*n + j] = component i of eigenvector j).
static void jacobi(double *A, int n, double *eval, double *evec) {
  for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++) evec[i * n + j] = (i == j) ? 1.0 : 0.0;

  for (int sweep = 0; sweep < 100; sweep++) {
    double off = 0.0;
    for (int p = 0; p < n; p++)
      for (int q = p + 1; q < n; q++) off += A[p * n + q] * A[p * n + q];
    if (off < 1e-30) break;

    for (int p = 0; p < n; p++) {
      for (int q = p + 1; q < n; q++) {
        double apq = A[p * n + q];
        if (fabs(apq) < 1e-300) continue;
        double app = A[p * n + p], aqq = A[q * n + q];
        double phi = 0.5 * atan2(2.0 * apq, aqq - app);
        double c = cos(phi), s = sin(phi);
        for (int k = 0; k < n; k++) {
          double akp = A[k * n + p], akq = A[k * n + q];
          A[k * n + p] = c * akp - s * akq;
          A[k * n + q] = s * akp + c * akq;
        }
        for (int k = 0; k < n; k++) {
          double apk = A[p * n + k], aqk = A[q * n + k];
          A[p * n + k] = c * apk - s * aqk;
          A[q * n + k] = s * apk + c * aqk;
        }
        for (int k = 0; k < n; k++) {
          double vkp = evec[k * n + p], vkq = evec[k * n + q];
          evec[k * n + p] = c * vkp - s * vkq;
          evec[k * n + q] = s * vkp + c * vkq;
        }
      }
    }
  }
  for (int i = 0; i < n; i++) eval[i] = A[i * n + i];
}

// Generalized symmetric eigensolve  K phi = lambda M phi  (M SPD).
// eval[n] sorted ascending; evec columns are the mode shapes. Returns 0 ok.
static int gen_eig(const double *K, const double *M, int n, double *eval,
                   double *evec) {
  double *L = malloc(sizeof(double) * n * n);
  double *Y = malloc(sizeof(double) * n * n);
  double *C = malloc(sizeof(double) * n * n);
  double *W = malloc(sizeof(double) * n * n);
  double *colb = malloc(sizeof(double) * n);
  double *colx = malloc(sizeof(double) * n);
  int rc = -1;
  if (!L || !Y || !C || !W || !colb || !colx) goto done;
  if (chol(M, L, n) != 0) goto done;

  // Y = L^-1 K   (solve L Y = K column by column)
  for (int j = 0; j < n; j++) {
    for (int i = 0; i < n; i++) colb[i] = K[i * n + j];
    fwd(L, colb, colx, n);
    for (int i = 0; i < n; i++) Y[i * n + j] = colx[i];
  }
  // C = L^-1 Y^T = L^-1 K^T L^-T  (= L^-1 K L^-T since K symmetric)
  for (int j = 0; j < n; j++) {
    for (int i = 0; i < n; i++) colb[i] = Y[j * n + i]; // row j of Y -> Y^T col j
    fwd(L, colb, colx, n);
    for (int i = 0; i < n; i++) C[i * n + j] = colx[i];
  }
  // Symmetrize against round-off, then Jacobi.
  for (int i = 0; i < n; i++)
    for (int j = i + 1; j < n; j++) {
      double a = 0.5 * (C[i * n + j] + C[j * n + i]);
      C[i * n + j] = C[j * n + i] = a;
    }
  jacobi(C, n, eval, W);

  // phi = L^-T W  (solve L^T phi = W column by column)
  for (int j = 0; j < n; j++) {
    for (int i = 0; i < n; i++) colb[i] = W[i * n + j];
    bwd_t(L, colb, colx, n);
    for (int i = 0; i < n; i++) evec[i * n + j] = colx[i];
  }

  // Sort ascending by eigenvalue (selection sort; n is small).
  for (int a = 0; a < n; a++) {
    int mn = a;
    for (int b = a + 1; b < n; b++)
      if (eval[b] < eval[mn]) mn = b;
    if (mn != a) {
      double t = eval[a];
      eval[a] = eval[mn];
      eval[mn] = t;
      for (int i = 0; i < n; i++) {
        double tv = evec[i * n + a];
        evec[i * n + a] = evec[i * n + mn];
        evec[i * n + mn] = tv;
      }
    }
  }
  rc = 0;
done:
  free(L);
  free(Y);
  free(C);
  free(W);
  free(colb);
  free(colx);
  return rc;
}

// ===========================================================================
// 2. Analytic-limit self-tests
// ===========================================================================

// Free-free Euler-Bernoulli beam, ne elements, length Ltot, bending EI, line
// mass rhoA. Fills the lowest `want` *nonzero* modal frequencies (Hz) into f[].
static int beam_freefree(int ne, double Ltot, double EI, double rhoA, double *f,
                         int want) {
  int nn = ne + 1, nd = 2 * nn;
  double le = Ltot / ne;
  double *K = calloc((size_t)nd * nd, sizeof(double));
  double *M = calloc((size_t)nd * nd, sizeof(double));
  double *ev = malloc(sizeof(double) * nd);
  double *V = malloc(sizeof(double) * (size_t)nd * nd);
  int rc = -1;
  if (!K || !M || !ev || !V) goto done;

  double k0 = EI / (le * le * le);
  double Ke[4][4] = {{12, 6 * le, -12, 6 * le},
                     {6 * le, 4 * le * le, -6 * le, 2 * le * le},
                     {-12, -6 * le, 12, -6 * le},
                     {6 * le, 2 * le * le, -6 * le, 4 * le * le}};
  double m0 = rhoA * le / 420.0;
  double Me[4][4] = {{156, 22 * le, 54, -13 * le},
                     {22 * le, 4 * le * le, 13 * le, -3 * le * le},
                     {54, 13 * le, 156, -22 * le},
                     {-13 * le, -3 * le * le, -22 * le, 4 * le * le}};
  for (int e = 0; e < ne; e++) {
    int map[4] = {2 * e, 2 * e + 1, 2 * e + 2, 2 * e + 3};
    for (int a = 0; a < 4; a++)
      for (int b = 0; b < 4; b++) {
        K[map[a] * nd + map[b]] += k0 * Ke[a][b];
        M[map[a] * nd + map[b]] += m0 * Me[a][b];
      }
  }
  if (gen_eig(K, M, nd, ev, V) != 0) goto done;
  // Skip the two ~zero rigid-body modes.
  int got = 0;
  for (int i = 0; i < nd && got < want; i++) {
    if (ev[i] < 1e-6) continue;
    f[got++] = sqrt(ev[i]) / TAU;
  }
  rc = (got == want) ? 0 : -1;
done:
  free(K);
  free(M);
  free(ev);
  free(V);
  return rc;
}

int bell_selftest(int verbose) {
  int fails = 0;

  // (a) Jacobi on [[2,1],[1,2]] -> eigenvalues {1,3}.
  {
    double A[4] = {2, 1, 1, 2}, ev[2], V[4];
    jacobi(A, 2, ev, V);
    double lo = ev[0] < ev[1] ? ev[0] : ev[1];
    double hi = ev[0] < ev[1] ? ev[1] : ev[0];
    int ok = fabs(lo - 1.0) < 1e-9 && fabs(hi - 3.0) < 1e-9;
    if (verbose)
      printf("  [%s] Jacobi 2x2: %.6f, %.6f (want 1, 3)\n", ok ? "ok" : "FAIL",
             lo, hi);
    fails += !ok;
  }

  // (b) Generalized eig: K=[[6,2],[2,3]], M=diag(2,1) -> {3-sqrt2, 3+sqrt2}.
  {
    double K[4] = {6, 2, 2, 3}, M[4] = {2, 0, 0, 1}, ev[2], V[4];
    int rc = gen_eig(K, M, 2, ev, V);
    double want0 = 3.0 - sqrt(2.0), want1 = 3.0 + sqrt(2.0);
    int ok = rc == 0 && fabs(ev[0] - want0) < 1e-9 && fabs(ev[1] - want1) < 1e-9;
    if (verbose)
      printf("  [%s] gen-eig 2x2: %.6f, %.6f (want %.6f, %.6f)\n",
             ok ? "ok" : "FAIL", ev[0], ev[1], want0, want1);
    fails += !ok;
  }

  // (c) Free-free Euler-Bernoulli beam vs analytic (beta*L)^2 sqrt(EI/rhoA/L^4).
  // beta*L for free-free: 4.730041, 7.853205, 10.995608.
  {
    double L = 1.0, EI = 1.0, rhoA = 1.0;
    double f[3];
    int rc = beam_freefree(40, L, EI, rhoA, f, 3);
    double bl[3] = {4.730040745, 7.853204624, 10.995607838};
    int ok = rc == 0;
    if (verbose) printf("  [..] free-free beam (40 elems):\n");
    for (int i = 0; i < 3 && rc == 0; i++) {
      double want = bl[i] * bl[i] * sqrt(EI / (rhoA * L * L * L * L)) / TAU;
      double err = fabs(f[i] - want) / want;
      int oki = err < 2e-3; // FEM discretization tolerance
      ok = ok && oki;
      if (verbose)
        printf("      [%s] mode %d: %.5f Hz (want %.5f, err %.2e)\n",
               oki ? "ok" : "FAIL", i + 1, f[i], want, err);
    }
    fails += !ok;
  }

  // (d) Cylinder limit -> analytic in-plane ring flexural series.
  //     f_m = (1/2pi)(m(m^2-1)/sqrt(m^2+1)) (h/a^2) sqrt(E/(12 rho)).
  //     Uses nu=0 so the shell hoop stiffness D = E h^3/12 matches the 1-D ring.
  {
    double a = 0.20, h = 0.004, E = 105e9, rho = 8800.0;
    BellGeometry g;
    g.n = 48;
    snprintf(g.name, sizeof(g.name), "test-cyl");
    for (int i = 0; i < g.n; i++) {
      g.r[i] = a;
      g.z[i] = 0.30 * (double)i / (g.n - 1);
      g.h[i] = h;
    }
    BellMaterial mt = {E, rho, 0.0, 1e-3, "test"};
    BellModes md;
    int rc = bell_solve_modes(&g, &mt, 5, 64, &md);
    double pref = (h / (a * a)) * sqrt(E / (12.0 * rho)) / TAU;
    if (verbose) printf("  [..] cylinder -> ring series (nu=0):\n");
    int ok = rc > 0;
    for (int m = 2; m <= 4; m++) {
      double lo = 1e30;
      for (int k = 0; k < md.count; k++)
        if (md.mode[k].m == m && md.mode[k].freq < lo) lo = md.mode[k].freq;
      double want = pref * (m * (m * m - 1.0) / sqrt(m * m + 1.0));
      double err = fabs(lo - want) / want;
      int oki = lo < 1e29 && err < 0.10; // faceted-conical + Flugge tolerance
      ok = ok && oki;
      if (verbose)
        printf("      [%s] m=%d: %.2f Hz (ring %.2f, err %.1f%%)\n",
               oki ? "ok" : "FAIL", m, lo, want, err * 100.0);
    }
    fails += !ok;
  }

  if (verbose)
    printf("selftest: %s (%d failure%s)\n", fails ? "FAIL" : "PASS", fails,
           fails == 1 ? "" : "s");
  return fails;
}

// ===========================================================================
// 3. Materials & geometry presets
// ===========================================================================

void bell_default_material(BellMaterial *m) {
  bell_material_preset(m, "bronze");
}

int bell_material_preset(BellMaterial *mat, const char *name) {
  // E (Pa), rho (kg/m^3), nu, loss factor eta. Loss tuned for musical decay.
  struct {
    const char *n;
    double E, rho, nu, loss;
  } tbl[] = {
      {"bronze", 105e9, 8800.0, 0.33, 2.0e-4}, // bell bronze (78Cu/22Sn)
      {"brass", 100e9, 8500.0, 0.34, 3.0e-4},
      {"steel", 200e9, 7850.0, 0.30, 1.2e-4},
      {"aluminum", 69e9, 2700.0, 0.33, 8.0e-4},
      {"silver", 83e9, 10490.0, 0.37, 5.0e-4},
      {"glass", 70e9, 2500.0, 0.22, 6.0e-5}, // long shimmering ring
      {"gold", 79e9, 19300.0, 0.42, 9.0e-4},
  };
  int N = (int)(sizeof(tbl) / sizeof(tbl[0]));
  for (int i = 0; i < N; i++) {
    if (strcmp(tbl[i].n, name) == 0) {
      mat->E = tbl[i].E;
      mat->rho = tbl[i].rho;
      mat->nu = tbl[i].nu;
      mat->loss = tbl[i].loss;
      snprintf(mat->name, sizeof(mat->name), "%s", tbl[i].n);
      return 0;
    }
  }
  return -1;
}

// Parametric meridian: crown (top, t=0) -> mouth (bottom, t=1).
// shape: 0 flaring bell, 1 straight tube, 2 hemispherical bowl.
static void build_profile(BellGeometry *g, const char *name, double rMouth,
                          double height, double rCrownFrac, double hMouth,
                          double hCrown, double flare, int shape) {
  int n = 64;
  g->n = n;
  snprintf(g->name, sizeof(g->name), "%s", name);
  for (int i = 0; i < n; i++) {
    double t = (double)i / (n - 1); // 0 crown .. 1 mouth
    double r, z;
    if (shape == 1) { // tube
      r = rMouth;
      z = height * (1.0 - t);
    } else if (shape == 2) { // bowl (hemisphere-ish)
      double ang = t * (M_PI * 0.5);
      r = rMouth * sin(ang);
      z = height * (1.0 - cos(ang));
    } else { // flaring bell
      double base = rCrownFrac + (1.0 - rCrownFrac) * pow(t, flare);
      r = rMouth * base;
      z = height * (1.0 - t);
    }
    if (r < 1e-4) r = 1e-4;
    g->r[i] = r;
    g->z[i] = z;
    // Thicker toward the mouth/soundbow.
    g->h[i] = hCrown + (hMouth - hCrown) * pow(t, 1.3);
  }
}

void bell_default_geometry(BellGeometry *g) {
  bell_geometry_preset(g, "church");
}

int bell_geometry_preset(BellGeometry *g, const char *name) {
  if (strcmp(name, "church") == 0)
    build_profile(g, "church", 0.50, 0.80, 0.34, 0.045, 0.014, 1.7, 0);
  else if (strcmp(name, "handbell") == 0)
    build_profile(g, "handbell", 0.085, 0.13, 0.30, 0.006, 0.002, 1.8, 0);
  else if (strcmp(name, "tubular") == 0)
    build_profile(g, "tubular", 0.0159, 1.40, 1.0, 0.0012, 0.0012, 1.0, 1);
  else if (strcmp(name, "bowl") == 0)
    build_profile(g, "bowl", 0.11, 0.07, 1.0, 0.006, 0.006, 1.0, 2);
  else if (strcmp(name, "glass") == 0)
    build_profile(g, "glass", 0.045, 0.12, 0.55, 0.0018, 0.0014, 1.4, 0);
  else
    return -1;
  return 0;
}

// ===========================================================================
// 4. Axisymmetric shell FEM assembly (conical frustum elements, harmonic m)
// ===========================================================================
//
// DOF per node: U (meridional), V (circumferential), W (normal), beta
// (meridional section rotation). Local element order:
//   [U1 V1 W1 b1 U2 V2 W2 b2]
//
// Strain-displacement (per element, faceted-conical): see README. Validated in
// the cylinder limit against the analytic ring series and in the straight-beam
// limit against free-free Euler-Bernoulli. Selective reduced integration on the
// transverse shear (1-pt) avoids shear locking.

#define DOF_PER_NODE 4

// Assemble global K, M (nd x nd, nd = 4*n) for circumferential order m.
static void shell_assemble(const BellGeometry *g, const BellMaterial *mat, int m,
                           double *K, double *M, int nd) {
  for (int i = 0; i < nd * nd; i++) K[i] = M[i] = 0.0;
  double E = mat->E, nu = mat->nu, rho = mat->rho;
  double G = E / (2.0 * (1.0 + nu));

  // 2-point Gauss on [0,1].
  double gx[2] = {0.5 - 0.5 / sqrt(3.0), 0.5 + 0.5 / sqrt(3.0)};
  double gw[2] = {0.5, 0.5};

  for (int e = 0; e < g->n - 1; e++) {
    int i0 = e, i1 = e + 1;
    double dr = g->r[i1] - g->r[i0];
    double dz = g->z[i1] - g->z[i0];
    double Le = sqrt(dr * dr + dz * dz);
    if (Le < 1e-9) continue;
    double sphi = dr / Le; // dr/ds
    double cphi = dz / Le; // dz/ds
    int map[8] = {DOF_PER_NODE * i0 + 0, DOF_PER_NODE * i0 + 1,
                  DOF_PER_NODE * i0 + 2, DOF_PER_NODE * i0 + 3,
                  DOF_PER_NODE * i1 + 0, DOF_PER_NODE * i1 + 1,
                  DOF_PER_NODE * i1 + 2, DOF_PER_NODE * i1 + 3};
    double Ke[8][8] = {{0}}, Me[8][8] = {{0}};

    // --- membrane + bending (2-pt) ---
    for (int gpt = 0; gpt < 2; gpt++) {
      double xi = gx[gpt], w = gw[gpt];
      double N1 = 1.0 - xi, N2 = xi;
      double dN = 1.0 / Le; // |dN1/ds| = dN, dN2/ds = +dN
      double r = N1 * g->r[i0] + N2 * g->r[i1];
      double h = N1 * g->h[i0] + N2 * g->h[i1];
      if (r < 1e-5) r = 1e-5;
      double Dm0 = E * h / (1.0 - nu * nu);
      double Db0 = E * h * h * h / (12.0 * (1.0 - nu * nu));

      // Membrane B-rows (3 x 8): es, eth, gsth.
      double Bm[3][8] = {{0}};
      Bm[0][0] = -dN; Bm[0][4] = dN;                                   // es=U'
      Bm[1][0] = N1 * sphi / r; Bm[1][4] = N2 * sphi / r;              // eth
      Bm[1][1] = N1 * m / r;    Bm[1][5] = N2 * m / r;
      Bm[1][2] = N1 * cphi / r; Bm[1][6] = N2 * cphi / r;
      Bm[2][1] = -dN - sphi / r * N1; Bm[2][5] = dN - sphi / r * N2;   // gsth
      Bm[2][0] = -(double)m / r * N1; Bm[2][4] = -(double)m / r * N2;

      // Bending B-rows (3 x 8): ks, kth, ksth.
      double Bb[3][8] = {{0}};
      Bb[0][3] = -dN; Bb[0][7] = dN;                                   // ks=beta'
      double mm1 = (double)m * m - 1.0;                                // Flugge-consistent
      Bb[1][2] = N1 * mm1 / (r * r); Bb[1][6] = N2 * mm1 / (r * r);    // kth
      Bb[1][3] = N1 * sphi / r;      Bb[1][7] = N2 * sphi / r;
      Bb[2][3] = N1 * (double)m / r; Bb[2][7] = N2 * (double)m / r;    // ksth (twist)

      // Material matrices.
      double Dm[3][3] = {{Dm0, Dm0 * nu, 0}, {Dm0 * nu, Dm0, 0},
                         {0, 0, Dm0 * (1.0 - nu) / 2.0}};
      double Db[3][3] = {{Db0, Db0 * nu, 0}, {Db0 * nu, Db0, 0},
                         {0, 0, Db0 * (1.0 - nu)}};
      double scale = w * Le * r;
      for (int a = 0; a < 8; a++)
        for (int b = 0; b < 8; b++) {
          double s = 0.0;
          for (int p = 0; p < 3; p++)
            for (int q = 0; q < 3; q++)
              s += Bm[p][a] * Dm[p][q] * Bm[q][b] +
                   Bb[p][a] * Db[p][q] * Bb[q][b];
          Ke[a][b] += scale * s;
        }

      // Consistent mass (translational + small rotary inertia for beta).
      double NU[8] = {N1, 0, 0, 0, N2, 0, 0, 0};
      double NV[8] = {0, N1, 0, 0, 0, N2, 0, 0};
      double NW[8] = {0, 0, N1, 0, 0, 0, N2, 0};
      double NB[8] = {0, 0, 0, N1, 0, 0, 0, N2};
      double rhoh = rho * h, rhoI = rho * h * h * h / 12.0;
      for (int a = 0; a < 8; a++)
        for (int b = 0; b < 8; b++)
          Me[a][b] += scale * (rhoh * (NU[a] * NU[b] + NV[a] * NV[b] +
                                       NW[a] * NW[b]) +
                               rhoI * NB[a] * NB[b]);
    }

    // --- transverse shear (1-pt, reduced) ---
    {
      double N1 = 0.5, N2 = 0.5, dN = 1.0 / Le;
      double r = N1 * g->r[i0] + N2 * g->r[i1];
      double h = N1 * g->h[i0] + N2 * g->h[i1];
      if (r < 1e-5) r = 1e-5;
      double Ds0 = (5.0 / 6.0) * G * h;
      double Bs[8] = {0};
      Bs[2] = -dN; Bs[6] = dN;        // gsz = W' - beta
      Bs[3] = -N1; Bs[7] = -N2;
      double scale = 1.0 * Le * r;
      for (int a = 0; a < 8; a++)
        for (int b = 0; b < 8; b++)
          Ke[a][b] += scale * Ds0 * Bs[a] * Bs[b];
    }

    for (int a = 0; a < 8; a++)
      for (int b = 0; b < 8; b++) {
        K[map[a] * nd + map[b]] += Ke[a][b];
        M[map[a] * nd + map[b]] += Me[a][b];
      }
  }

  // Tiny diagonal mass floor so M stays SPD (guards starved DOFs at apex).
  double mref = 0.0;
  for (int i = 0; i < nd; i++)
    if (M[i * nd + i] > mref) mref = M[i * nd + i];
  for (int i = 0; i < nd; i++)
    if (M[i * nd + i] < 1e-9 * mref) M[i * nd + i] += 1e-9 * mref;
}

// ===========================================================================
// 5. Solve modes
// ===========================================================================

int bell_solve_modes(const BellGeometry *g, const BellMaterial *mat, int max_m,
                     int max_modes, BellModes *out) {
  int n = g->n;
  int nd = DOF_PER_NODE * n;
  double *K = malloc(sizeof(double) * (size_t)nd * nd);
  double *M = malloc(sizeof(double) * (size_t)nd * nd);
  double *ev = malloc(sizeof(double) * nd);
  double *V = malloc(sizeof(double) * (size_t)nd * nd);
  if (!K || !M || !ev || !V) {
    free(K); free(M); free(ev); free(V);
    return -1;
  }
  out->count = 0;

  // The bell *tone* lives in the rim-flexural families (m>=2): hum, prime,
  // tierce, quint, nominal are all m=2/m=3 modes with differing nodal circles.
  // m=0 (breathing) and m=1 (whole-body sway/bend) are weakly struck and not
  // part of the strike tone, so they are excluded.
  for (int m = 2; m <= max_m; m++) {
    shell_assemble(g, mat, m, K, M, nd);
    if (gen_eig(K, M, nd, ev, V) != 0) continue;
    // Reference (stiffest) frequency to threshold rigid-body modes.
    double topw = ev[nd - 1] > 0 ? ev[nd - 1] : 1.0;
    int keptThisM = 0;
    for (int k = 0; k < nd && keptThisM < 6; k++) {
      if (ev[k] <= 0) continue;
      // Rigid-body modes are ~0 relative to the stiff membrane spectrum; soft
      // bending modes sit ~1e-10*topw, so a deep threshold separates them.
      if (ev[k] < 1e-12 * topw) continue;
      double f = sqrt(ev[k]) / TAU;
      if (f < 1.0 || f > 40000.0) continue;
      if (out->count >= BELL_MAX_MODES) break;
      BellMode *md = &out->mode[out->count];
      md->m = m;
      md->freq = f;
      md->nshape = n;
      double wmax = 0.0;
      for (int i = 0; i < n; i++) {
        double Ui = V[(DOF_PER_NODE * i + 0) * nd + k];
        double Wi = V[(DOF_PER_NODE * i + 2) * nd + k];
        md->ushape[i] = Ui;
        md->wshape[i] = Wi;
        if (fabs(Wi) > wmax) wmax = fabs(Wi);
      }
      if (wmax < 1e-30) wmax = 1.0;
      for (int i = 0; i < n; i++) {  // normalize shapes to unit peak normal
        md->wshape[i] /= wmax;
        md->ushape[i] /= wmax;
      }
      md->part = fabs(md->wshape[0]); // strike at the mouth rim (node 0)
      md->tau = 0.0;                  // filled after material loss below
      out->count++;
      keptThisM++;
    }
  }

  // Sort all retained modes by frequency.
  for (int a = 0; a < out->count; a++) {
    int mn = a;
    for (int b = a + 1; b < out->count; b++)
      if (out->mode[b].freq < out->mode[mn].freq) mn = b;
    if (mn != a) {
      BellMode t = out->mode[a];
      out->mode[a] = out->mode[mn];
      out->mode[mn] = t;
    }
  }
  // Keep only the lowest max_modes.
  if (max_modes > 0 && out->count > max_modes) out->count = max_modes;

  // Material damping: amplitude decay delta = pi f eta, tau = 1/delta.
  for (int k = 0; k < out->count; k++) {
    double delta = M_PI * out->mode[k].freq * mat->loss;
    out->mode[k].tau = delta > 1e-9 ? 1.0 / delta : 1e9;
  }

  // Strike note = loudest partial at the strike point (perceived pitch).
  int best = 0;
  double bestScore = -1.0;
  for (int k = 0; k < out->count; k++) {
    // Favor strong, low partials (perceptually carry the pitch).
    double score = out->mode[k].part / (1.0 + out->mode[k].freq / 800.0);
    if (score > bestScore) {
      bestScore = score;
      best = k;
    }
  }
  out->strike_index = out->count ? best : 0;
  out->strike_freq = out->count ? out->mode[best].freq : 0.0;

  free(K); free(M); free(ev); free(V);
  return out->count;
}

void bell_retune(BellModes *modes, double target_freq) {
  if (modes->count == 0 || modes->strike_freq <= 0.0) return;
  double s = target_freq / modes->strike_freq;
  for (int k = 0; k < modes->count; k++) {
    modes->mode[k].freq *= s;
    double delta = modes->mode[k].tau > 0 ? 1.0 / modes->mode[k].tau : 0.0;
    (void)delta;
    // Recompute tau against the new frequency would change decay; keep decay
    // tied to the *new* pitch so retuned bells of different sizes still ring
    // believably: tau scales as 1/f at fixed loss factor.
    modes->mode[k].tau /= s;
  }
  modes->strike_freq = target_freq;
}

// ===========================================================================
// 6. Strike excitation + modal render
// ===========================================================================

// Render toggles (set by the CLI; default on). Disabling both makes the output
// a pure deterministic modal sum for the compare.mjs JS-parity harness.
static int g_strike = 1; // add the clapper-contact noise transient
static int g_norm = 1;   // peak-normalize the final mix

long bell_render(const BellModes *modes, double strike_vel, double sr,
                 double dur, float *L, float *R, long nsamp) {
  long N = (long)(dur * sr);
  if (N > nsamp) N = nsamp;
  for (long i = 0; i < N; i++) L[i] = R[i] = 0.0f;
  if (modes->count == 0) return N;

  // Per-mode initial amplitude from strike participation. A_k = part * vel / w.
  double amp[BELL_MAX_MODES], pan[BELL_MAX_MODES];
  double peak = 0.0;
  for (int k = 0; k < modes->count; k++) {
    double w = TAU * modes->mode[k].freq;
    amp[k] = modes->mode[k].part * strike_vel / (w > 1e-9 ? sqrt(w) : 1.0);
    peak += fabs(amp[k]);
    // Stereo spread by circumferential order (m=2 center, higher = wider).
    double sp = 0.16 * ((modes->mode[k].m % 5) - 2);
    pan[k] = sp;
  }
  if (peak < 1e-30) peak = 1.0;

  for (int k = 0; k < modes->count; k++) {
    double f = modes->mode[k].freq;
    double tau = modes->mode[k].tau;
    double a = amp[k] / peak;
    double gl = 0.5 - 0.5 * pan[k]; // simple equal-ish pan
    double gr = 0.5 + 0.5 * pan[k];
    double phase = 0.0, inc = f / sr;
    for (long i = 0; i < N; i++) {
      double env = exp(-(double)i / (tau * sr));
      double s = a * env * sin(TAU * phase);
      L[i] += (float)(s * gl);
      R[i] += (float)(s * gr);
      phase += inc;
      if (phase >= 1.0) phase -= 1.0;
    }
  }

  // Strike transient: brief filtered-noise click (the clapper contact).
  if (g_strike) {
    uint32_t rng = 0x1234567u;
    double lp = 0.0;
    long clk = (long)(0.006 * sr); // ~6 ms
    double camp = 0.18 * strike_vel;
    for (long i = 0; i < clk && i < N; i++) {
      rng = rng * 1664525u + 1013904223u;
      double white = ((double)rng / 4294967296.0) * 2.0 - 1.0;
      lp = 0.35 * white + 0.65 * lp;
      double env = exp(-(double)i / (0.0018 * sr));
      double s = camp * env * lp;
      L[i] += (float)s;
      R[i] += (float)s;
    }
  }

  // Peak normalize to 0.9.
  double pk = 0.0;
  for (long i = 0; i < N; i++) {
    if (fabs(L[i]) > pk) pk = fabs(L[i]);
    if (fabs(R[i]) > pk) pk = fabs(R[i]);
  }
  if (g_norm && pk > 1e-9) {
    double gain = 0.9 / pk;
    for (long i = 0; i < N; i++) {
      L[i] = (float)(L[i] * gain);
      R[i] = (float)(R[i] * gain);
    }
  }
  return N;
}

// ===========================================================================
// 7. JSON export (geometry + modes + shapes) for the viz
// ===========================================================================

int bell_export_modes_json(const BellGeometry *g, const BellMaterial *mat,
                           const BellModes *modes, const char *path) {
  FILE *f = fopen(path, "w");
  if (!f) return -1;
  fprintf(f, "{\n");
  fprintf(f, "  \"material\": {\"name\":\"%s\",\"E\":%g,\"rho\":%g,\"nu\":%g,"
             "\"loss\":%g},\n",
          mat->name, mat->E, mat->rho, mat->nu, mat->loss);
  fprintf(f, "  \"geometry\": {\"name\":\"%s\",\"n\":%d,\n", g->name, g->n);
  fprintf(f, "    \"z\": [");
  for (int i = 0; i < g->n; i++) fprintf(f, "%s%.6f", i ? "," : "", g->z[i]);
  fprintf(f, "],\n    \"r\": [");
  for (int i = 0; i < g->n; i++) fprintf(f, "%s%.6f", i ? "," : "", g->r[i]);
  fprintf(f, "],\n    \"h\": [");
  for (int i = 0; i < g->n; i++) fprintf(f, "%s%.6f", i ? "," : "", g->h[i]);
  fprintf(f, "]\n  },\n");
  fprintf(f, "  \"strike_index\": %d,\n", modes->strike_index);
  fprintf(f, "  \"strike_freq\": %.4f,\n", modes->strike_freq);
  fprintf(f, "  \"modes\": [\n");
  for (int k = 0; k < modes->count; k++) {
    const BellMode *md = &modes->mode[k];
    fprintf(f, "    {\"m\":%d,\"freq\":%.8f,\"tau\":%.8f,\"part\":%.8f,\n",
            md->m, md->freq, md->tau, md->part);
    fprintf(f, "     \"w\": [");
    for (int i = 0; i < md->nshape; i++)
      fprintf(f, "%s%.5f", i ? "," : "", md->wshape[i]);
    fprintf(f, "],\n     \"u\": [");
    for (int i = 0; i < md->nshape; i++)
      fprintf(f, "%s%.5f", i ? "," : "", md->ushape[i]);
    fprintf(f, "]}%s\n", k + 1 < modes->count ? "," : "");
  }
  fprintf(f, "  ]\n}\n");
  fclose(f);
  return 0;
}

// ===========================================================================
// 8. WAV writer + CLI
// ===========================================================================

static void write_wav_f32_stereo(const char *path, const float *L,
                                  const float *R, long n, int sr) {
  FILE *f = fopen(path, "wb");
  if (!f) return;
  int ch = 2, bits = 32;
  int byteRate = sr * ch * bits / 8;
  int blockAlign = ch * bits / 8;
  long dataBytes = n * ch * (bits / 8);
  fwrite("RIFF", 1, 4, f);
  uint32_t riff = 36 + (uint32_t)dataBytes;
  fwrite(&riff, 4, 1, f);
  fwrite("WAVE", 1, 4, f);
  fwrite("fmt ", 1, 4, f);
  uint32_t fmtlen = 16;
  uint16_t fmt = 3; // IEEE float
  uint16_t chs = (uint16_t)ch;
  uint32_t srr = (uint32_t)sr;
  uint16_t ba = (uint16_t)blockAlign, bps = (uint16_t)bits;
  uint32_t br = (uint32_t)byteRate;
  fwrite(&fmtlen, 4, 1, f);
  fwrite(&fmt, 2, 1, f);
  fwrite(&chs, 2, 1, f);
  fwrite(&srr, 4, 1, f);
  fwrite(&br, 4, 1, f);
  fwrite(&ba, 2, 1, f);
  fwrite(&bps, 2, 1, f);
  fwrite("data", 1, 4, f);
  uint32_t dlen = (uint32_t)dataBytes;
  fwrite(&dlen, 4, 1, f);
  for (long i = 0; i < n; i++) {
    fwrite(&L[i], 4, 1, f);
    fwrite(&R[i], 4, 1, f);
  }
  fclose(f);
}

static double note_to_freq(const char *s) {
  // Accept "A4", "C#5", "Db3", or a bare number (Hz).
  char *end;
  double num = strtod(s, &end);
  if (end != s && *end == '\0') return num;
  int sem;
  switch (s[0]) {
    case 'C': sem = 0; break;
    case 'D': sem = 2; break;
    case 'E': sem = 4; break;
    case 'F': sem = 5; break;
    case 'G': sem = 7; break;
    case 'A': sem = 9; break;
    case 'B': sem = 11; break;
    default: return 440.0;
  }
  int i = 1;
  if (s[i] == '#') { sem++; i++; }
  else if (s[i] == 'b') { sem--; i++; }
  int oct = atoi(&s[i]);
  int midi = (oct + 1) * 12 + sem;
  return 440.0 * pow(2.0, (midi - 69) / 12.0);
}

int main(int argc, char **argv) {
  const char *note = "A4", *material = "bronze", *geom = "church";
  const char *out = NULL, *modesPath = NULL;
  double dur = 6.0, vel = 0.9;
  double sr = 48000.0;
  int maxM = 8, maxModes = 32;
  int showModes = 0;

  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "--selftest") == 0) return bell_selftest(1);
    else if (strcmp(argv[i], "--note") == 0 && i + 1 < argc) note = argv[++i];
    else if (strcmp(argv[i], "--material") == 0 && i + 1 < argc) material = argv[++i];
    else if (strcmp(argv[i], "--geometry") == 0 && i + 1 < argc) geom = argv[++i];
    else if (strcmp(argv[i], "--dur") == 0 && i + 1 < argc) dur = atof(argv[++i]);
    else if (strcmp(argv[i], "--vel") == 0 && i + 1 < argc) vel = atof(argv[++i]);
    else if (strcmp(argv[i], "--sr") == 0 && i + 1 < argc) sr = atof(argv[++i]);
    else if (strcmp(argv[i], "--maxm") == 0 && i + 1 < argc) maxM = atoi(argv[++i]);
    else if (strcmp(argv[i], "--out") == 0 && i + 1 < argc) out = argv[++i];
    else if (strcmp(argv[i], "--modes") == 0 && i + 1 < argc) modesPath = argv[++i];
    else if (strcmp(argv[i], "--print-modes") == 0) showModes = 1;
    else if (strcmp(argv[i], "--nostrike") == 0) g_strike = 0;
    else if (strcmp(argv[i], "--nonorm") == 0) g_norm = 0;
    else {
      fprintf(stderr, "bell: unknown arg %s\n", argv[i]);
      return 2;
    }
  }

  BellGeometry g;
  BellMaterial mat;
  if (bell_geometry_preset(&g, geom) != 0) {
    fprintf(stderr, "bell: unknown geometry '%s'\n", geom);
    return 2;
  }
  if (bell_material_preset(&mat, material) != 0) {
    fprintf(stderr, "bell: unknown material '%s'\n", material);
    return 2;
  }

  BellModes modes;
  int nm = bell_solve_modes(&g, &mat, maxM, maxModes, &modes);
  if (nm <= 0) {
    fprintf(stderr, "bell: mode solve failed\n");
    return 1;
  }
  double target = note_to_freq(note);
  bell_retune(&modes, target);

  fprintf(stderr,
          "bell: %s/%s, %d modes, strike note %.2f Hz (%s), pre-tune %.2f Hz\n",
          geom, material, nm, modes.strike_freq, note,
          modes.strike_freq / (target / (modes.mode[modes.strike_index].freq)));

  if (showModes) {
    fprintf(stderr, "  idx   m    freq(Hz)   ratio    tau(s)  part\n");
    double f0 = modes.mode[modes.strike_index].freq;
    for (int k = 0; k < modes.count; k++)
      fprintf(stderr, "  %3d  %2d  %9.2f  %6.3f  %7.2f  %.3f\n", k,
              modes.mode[k].m, modes.mode[k].freq, modes.mode[k].freq / f0,
              modes.mode[k].tau, modes.mode[k].part);
  }

  if (modesPath) {
    if (bell_export_modes_json(&g, &mat, &modes, modesPath) == 0)
      fprintf(stderr, "bell: wrote modes -> %s\n", modesPath);
  }

  if (out) {
    long nsamp = (long)(dur * sr) + 16;
    float *Lb = malloc(sizeof(float) * nsamp);
    float *Rb = malloc(sizeof(float) * nsamp);
    long n = bell_render(&modes, vel, sr, dur, Lb, Rb, nsamp);
    write_wav_f32_stereo(out, Lb, Rb, n, (int)sr);
    fprintf(stderr, "bell: wrote %ld samples -> %s\n", n, out);
    free(Lb);
    free(Rb);
  }

  return 0;
}
