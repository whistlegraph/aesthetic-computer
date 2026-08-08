// ac_necklace.h — the geometry of musical rhythm for native C/WASM engines.
//
// Header-only, allocation-free, libm only. Compiles unchanged to WebAssembly,
// same posture as ac_hrtf.h. The JS twin is pop/lib/necklace.mjs; the two are
// kept in lockstep by tests/pop-necklace.test.mjs, which pins both against the
// published numbers of pop/minitek/c/{hypnotek,dubtek}.c.
//
// Specified by papers/rhythm-platter/digest/. Where a definition here and the
// digest disagree, the digest is the spec and this file is the bug.
//
// A rhythm is an int[n] of 0/1, one entry per pulse. Nothing here allocates;
// callers own every buffer and AC_NECKLACE_MAX bounds the scratch.

#ifndef AC_NECKLACE_H
#define AC_NECKLACE_H

#include <math.h>

#define AC_NECKLACE_MAX 64
#define AC_NECKLACE_TAU 6.28318530717958647692

// ── 01 · representation ─────────────────────────────────────────────────────

// Onset step-indices of a 0/1 pattern. Returns k.
static inline int ac_onsets(const int *p, int n, int *out) {
    int k = 0;
    for (int i = 0; i < n; i++) if (p[i]) out[k++] = i;
    return k;
}

// Cyclic inter-onset intervals. The last one wraps to the first onset — the
// off-by-one here is the classic bug, so it is written out rather than implied.
static inline int ac_ioi(const int *p, int n, int *out) {
    int idx[AC_NECKLACE_MAX], k = ac_onsets(p, n, idx);
    for (int i = 0; i < k; i++) out[i] = (i + 1 < k) ? idx[i + 1] - idx[i] : idx[0] + n - idx[i];
    return k;
}

static inline void ac_rotate(const int *p, int n, int rot, int *out) {
    int r = ((rot % n) + n) % n;
    for (int i = 0; i < n; i++) out[i] = p[(i - r + n) % n];
}

// Reverse the cycle about a pulse. The bracelet operation.
static inline void ac_reflect(const int *p, int n, int axis, int *out) {
    for (int i = 0; i < n; i++) out[i] = p[(((axis * 2 - i) % n) + n) % n];
}

// Lexicographically least rotation — the necklace representative. Writes n
// chars plus a terminator into out, which must hold n+1 bytes.
static inline void ac_necklace_canonical(const int *p, int n, char *out) {
    int best = 0;
    for (int s = 1; s < n; s++) {
        for (int i = 0; i < n; i++) {
            int a = p[(best + i) % n], b = p[(s + i) % n];
            if (a != b) { if (b < a) best = s; break; }
        }
    }
    for (int i = 0; i < n; i++) out[i] = (char)('0' + p[(best + i) % n]);
    out[n] = '\0';
}

// Two rhythms are the same necklace iff their representatives match. Comparing
// raw patterns instead is wrong: the published catalogue prints one arbitrary
// rotation per entry, so string equality gives false negatives everywhere.
static inline int ac_same_necklace(const int *a, const int *b, int n) {
    char ca[AC_NECKLACE_MAX + 1], cb[AC_NECKLACE_MAX + 1];
    ac_necklace_canonical(a, n, ca);
    ac_necklace_canonical(b, n, cb);
    for (int i = 0; i < n; i++) if (ca[i] != cb[i]) return 0;
    return 1;
}

static inline int ac_same_bracelet(const int *a, const int *b, int n) {
    int rev[AC_NECKLACE_MAX];
    if (ac_same_necklace(a, b, n)) return 1;
    ac_reflect(b, n, 0, rev);
    return ac_same_necklace(a, rev, n);
}

// ── 02 · evenness ───────────────────────────────────────────────────────────

// Bjorklund's algorithm: grow k [1] groups and (n-k) [0] groups, fold the
// smaller pile onto the larger until one pile has at most one group, read off.
// Verified against all 48 entries of Toussaint (2005) §4.
static inline void ac_bjorklund(int k, int n, int rot, int *out) {
    for (int i = 0; i < n; i++) out[i] = 0;
    if (n <= 0 || k <= 0) return;
    if (k >= n) { for (int i = 0; i < n; i++) out[i] = 1; return; }

    // Each pile is a count of identical groups plus that group's bit content.
    int a = k, b = n - k, la = 1, lb = 1;
    int A[AC_NECKLACE_MAX], B[AC_NECKLACE_MAX], nA[AC_NECKLACE_MAX];
    A[0] = 1; B[0] = 0;
    while (b > 1) {
        int m = (a < b) ? a : b;
        int nla = la + lb;
        if (nla > AC_NECKLACE_MAX) break;
        for (int i = 0; i < la; i++) nA[i] = A[i];
        for (int i = 0; i < lb; i++) nA[la + i] = B[i];
        // The unfolded remainder of the larger pile becomes the new small pile.
        int keep_a = (a > b);
        int nb = keep_a ? (a - b) : (b - a);
        int nlb = keep_a ? la : lb;
        int tmp[AC_NECKLACE_MAX];
        for (int i = 0; i < nlb; i++) tmp[i] = keep_a ? A[i] : B[i];
        for (int i = 0; i < nla; i++) A[i] = nA[i];
        for (int i = 0; i < nlb; i++) B[i] = tmp[i];
        a = m; b = nb; la = nla; lb = nlb;
    }
    int pos = 0;
    for (int g = 0; g < a && pos < n; g++) for (int i = 0; i < la && pos < n; i++) out[pos++] = A[i];
    for (int g = 0; g < b && pos < n; g++) for (int i = 0; i < lb && pos < n; i++) out[pos++] = B[i];

    if (rot) { int t[AC_NECKLACE_MAX]; ac_rotate(out, n, rot, t); for (int i = 0; i < n; i++) out[i] = t[i]; }
}

// Exact test. The two-gap property is necessary but NOT sufficient — it pins the
// gap multiset and not its order — so this compares necklaces. See digest/02.
static inline int ac_is_maximally_even(const int *p, int n) {
    int idx[AC_NECKLACE_MAX], e[AC_NECKLACE_MAX];
    int k = ac_onsets(p, n, idx);
    if (k < 2) return 1;
    ac_bjorklund(k, n, 0, e);
    return ac_same_necklace(p, e, n);
}

// Sum of pairwise Euclidean CHORD lengths, onsets as points on the unit circle.
// Demaine et al. (2009) prove the maximally even set maximises exactly this, so
// chord-sum is the definition; arc-sum is a different, wrong functional.
static inline double ac_evenness_chord_sum(const int *p, int n) {
    int on[AC_NECKLACE_MAX], k = ac_onsets(p, n, on);
    double sum = 0;
    for (int a = 0; a < k; a++)
        for (int b = a + 1; b < k; b++)
            sum += 2.0 * fabs(sin(AC_NECKLACE_TAU * (on[a] - on[b]) / n / 2.0));
    return sum;
}

// Chord-sum normalised by the regular k-gon's. 1.0 == maximally even.
static inline double ac_evenness(const int *p, int n) {
    int on[AC_NECKLACE_MAX], k = ac_onsets(p, n, on);
    if (k < 2) return 1.0;
    double ideal = 0;
    for (int a = 0; a < k; a++)
        for (int b = a + 1; b < k; b++)
            ideal += 2.0 * fabs(sin(AC_NECKLACE_TAU * (a - b) / k / 2.0));
    return ideal > 0 ? ac_evenness_chord_sum(p, n) / ideal : 1.0;
}

// Variance of the cyclic IOIs. Lower = more even.
static inline double ac_ioi_variance(const int *p, int n) {
    int g[AC_NECKLACE_MAX], k = ac_ioi(p, n, g);
    if (k < 2) return 0;
    double mean = 0, var = 0;
    for (int i = 0; i < k; i++) mean += g[i];
    mean /= k;
    for (int i = 0; i < k; i++) { double d = g[i] - mean; var += d * d; }
    return var / k;
}

// AC-LOCAL, not Toussaint: 1 - meanAbsDev/worstDev over the IOIs, worst case
// being all onsets adjacent. Kept because hypnotek.c's published E uses it.
// Prefer ac_evenness() for anything new.
static inline double ac_evenness_ioi(const int *p, int n) {
    int g[AC_NECKLACE_MAX], k = ac_ioi(p, n, g);
    if (k < 2) return 1.0;
    double ideal = (double)n / k, dev = 0;
    for (int i = 0; i < k; i++) dev += fabs(g[i] - ideal);
    double meanAbsDev = dev / k;
    double worstDev = ((k - 1) * fabs(1.0 - ideal) + fabs((double)(n - (k - 1)) - ideal)) / k;
    return (worstDev > 1e-9) ? (1.0 - meanAbsDev / worstDev) : 1.0;
}

// AC-LOCAL, not Toussaint: summed distance from each onset to the nearest vertex
// of a best-fit ideal k-gon, minimised over a sub-step phase sweep. Kept for
// parity with hypnotek.c's published D.
static inline double ac_vertex_distance(const int *p, int n) {
    int idx[AC_NECKLACE_MAX], k = ac_onsets(p, n, idx);
    if (k < 2) return 0;
    double ideal = (double)n / k, best = 1e18;
    for (int ph = 0; ph < n; ph++) {
        double sum = 0;
        for (int i = 0; i < k; i++) {
            double bestv = 1e18;
            for (int j = 0; j < k; j++) {
                double v = j * ideal + ph * ideal / n;
                double d = fabs(idx[i] - v);
                if (d > n / 2.0) d = n - d;
                if (d < bestv) bestv = d;
            }
            sum += bestv;
        }
        if (sum < best) best = sum;
    }
    return best;
}

// Centre of mass of the onsets as unit vectors. magnitude 0 == perfectly
// balanced, a DIFFERENT property from maximal evenness (digest/02). Under the
// spatial mapping the angle is the sound field's centroid direction, which makes
// this the steering signal for a spatialised lane.
typedef struct { double magnitude, angle, normalized; int balanced; } ACBalance;

static inline ACBalance ac_balance(const int *p, int n) {
    int on[AC_NECKLACE_MAX], k = ac_onsets(p, n, on);
    double re = 0, im = 0;
    for (int i = 0; i < k; i++) { double a = AC_NECKLACE_TAU * on[i] / n; re += cos(a); im += sin(a); }
    ACBalance b;
    b.magnitude = sqrt(re * re + im * im);
    // A perfectly balanced rhythm has NO direction. Without this guard atan2
    // returns the angle of the floating-point residue, and a spatialised lane
    // steers by that angle — so the garbage would become an audible heading.
    b.balanced = (b.magnitude < 1e-9);
    b.angle = b.balanced ? 0.0 : atan2(im, re);
    b.normalized = k ? b.magnitude / k : 0;
    return b;
}

// ── 03 · oddity, depth, interval content ────────────────────────────────────

// Arom's rhythmic oddity: no two onsets divide the cycle into equal halves.
// Returns -1 for odd n, where the question is vacuous rather than true.
static inline int ac_rhythmic_oddity(const int *p, int n) {
    if (n % 2) return -1;
    int half = n / 2;
    for (int i = 0; i < n; i++) if (p[i] && p[(i + half) % n]) return 0;
    return 1;
}

// Histogram of geodesic distances over all pairs. Length floor(n/2)+1.
// Not the IOI sequence — both are needed and they are easy to confuse.
static inline void ac_interval_vector(const int *p, int n, int *out) {
    int on[AC_NECKLACE_MAX], k = ac_onsets(p, n, on);
    for (int i = 0; i <= n / 2; i++) out[i] = 0;
    for (int a = 0; a < k; a++)
        for (int b = a + 1; b < k; b++) {
            int d = on[a] - on[b];
            if (d < 0) d = -d;
            if (d > n / 2) d = n - d;
            out[d]++;
        }
}

static inline int ac_gcd(int a, int b) { while (b) { int t = a % b; a = b; b = t; } return a; }

// Off-beat pulses are the generators of C(n): the p with gcd(p, n) == 1.
static inline int ac_offbeatness(const int *p, int n) {
    int c = 0;
    for (int i = 0; i < n; i++) if (p[i] && ac_gcd(i, n) == 1) c++;
    return c;
}

// Off-beatness degenerates at prime n, where every nonzero pulse is a generator.
// Callers should check this rather than print a meaningless count.
static inline int ac_offbeatness_degenerate(int n) {
    if (n < 2) return 0;
    for (int d = 2; d * d <= n; d++) if (n % d == 0) return 0;
    return 1;
}

// ── 05 · distance ───────────────────────────────────────────────────────────

static inline int ac_dist_hamming(const int *a, const int *b, int n) {
    int d = 0;
    for (int i = 0; i < n; i++) if (a[i] != b[i]) d++;
    return d;
}

// Minimum adjacent swaps. For equal k this is the L1 distance of sorted onsets.
// Returns -1 when the onset counts differ.
static inline int ac_dist_swap(const int *a, const int *b, int n) {
    int oa[AC_NECKLACE_MAX], ob[AC_NECKLACE_MAX];
    int ka = ac_onsets(a, n, oa), kb = ac_onsets(b, n, ob);
    if (ka != kb) return -1;
    int d = 0;
    for (int i = 0; i < ka; i++) { int t = oa[i] - ob[i]; d += (t < 0) ? -t : t; }
    return d;
}

// Per-pulse height = the IOI that pulse falls inside (Gustafson / Hofmann-Engl).
static inline void ac_chronotonic(const int *p, int n, int *out) {
    int idx[AC_NECKLACE_MAX], g[AC_NECKLACE_MAX];
    int k = ac_onsets(p, n, idx);
    ac_ioi(p, n, g);
    for (int i = 0; i < n; i++) out[i] = 0;
    for (int i = 0; i < k; i++) for (int d = 0; d < g[i]; d++) out[(idx[i] + d) % n] = g[i];
}

// Area between two chronotonic curves. Toussaint (2004) rates this the best
// overall dissimilarity measure, so it is the one to reach for by default.
// Equal n only; the JS twin resamples to the lcm for mixed cycles.
static inline double ac_dist_chronotonic(const int *a, const int *b, int n) {
    int ca[AC_NECKLACE_MAX], cb[AC_NECKLACE_MAX];
    ac_chronotonic(a, n, ca);
    ac_chronotonic(b, n, cb);
    double d = 0;
    for (int i = 0; i < n; i++) d += fabs((double)ca[i] - cb[i]);
    return d / n;
}

// ── 06 · complements ────────────────────────────────────────────────────────

static inline void ac_complement(const int *p, int n, int *out) {
    for (int i = 0; i < n; i++) out[i] = p[i] ? 0 : 1;
}

// No two onsets ever coincide. The combinatorial guarantee a spatialised lane
// needs so that two rings never strike the same azimuth at the same instant.
static inline int ac_interlocks(const int *a, const int *b, int n) {
    for (int i = 0; i < n; i++) if (a[i] && b[i]) return 0;
    return 1;
}

#endif // AC_NECKLACE_H
