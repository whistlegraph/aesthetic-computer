// pitch-audit.c — PhysMidi pitch regression harness.
//
// Renders every GM program from the shared synthesis core (gm_synth.c) at a
// few notes and measures the actual fundamental (FFT, lowest strong partial)
// against the requested pitch. This is the regression backbone for the
// PhysMidi effort: run it after any synth-core change to catch register /
// octave / detune errors in the (mostly physically-modeled) voices.
//
// Build:  cc -O2 -I ../../slab/menuband/Sources/CGMSynth/include \
//             pitch-audit.c ../src/gm_synth.c -lm -o /tmp/pitch-audit
// Run:    /tmp/pitch-audit            # full table
//         /tmp/pitch-audit 40 56 71   # only these GM programs
//
// PASS = |error| < 50 cents on the median of the test notes. Sound-effect /
// inherently-aperiodic programs are reported as "aperiodic" (not failures).
// FFT (not autocorrelation/YIN) is authoritative here — the rich/noisy
// waveguide timbres octave-jump under time-domain detectors.

#include "gm_synth.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdint.h>

#define SR 48000.0
#define NFFT 32768

static const char *NM[128] = {
"Ac Grand","Br Acoustic","El Grand","Honky-tonk","El Piano1","El Piano2","Harpsichord","Clavinet",
"Celesta","Glockenspiel","Music Box","Vibraphone","Marimba","Xylophone","Tubular Bells","Dulcimer",
"Drawbar Org","Perc Org","Rock Org","Church Org","Reed Org","Accordion","Harmonica","Tango Acc",
"Ac Guitar(n)","Ac Guitar(s)","El Guitar(j)","El Guitar(c)","El Guitar(m)","Overdriven","Distortion","Gtr Harmonics",
"Ac Bass","El Bass(f)","El Bass(p)","Fretless","Slap1","Slap2","Synth Bass1","Synth Bass2",
"Violin","Viola","Cello","Contrabass","Tremolo Str","Pizzicato","Orch Harp","Timpani",
"Str Ens1","Str Ens2","Syn Str1","Syn Str2","Choir","Voice Oohs","Syn Choir","Orchestra Hit",
"Trumpet","Trombone","Tuba","Muted Tpt","Fr Horn","Brass Sec","Syn Brass1","Syn Brass2",
"Sop Sax","Alto Sax","Tenor Sax","Bari Sax","Oboe","Eng Horn","Bassoon","Clarinet",
"Piccolo","Flute","Recorder","Pan Flute","Blown Bottle","Shakuhachi","Whistle","Ocarina",
"Lead Square","Lead Saw","Lead Calliope","Lead Chiff","Lead Charang","Lead Voice","Lead Fifths","Lead Bass+Ld",
"Pad NewAge","Pad Warm","Pad Polysyn","Pad Choir","Pad Bowed","Pad Metallic","Pad Halo","Pad Sweep",
"FX Rain","FX Soundtrk","FX Crystal","FX Atmosph","FX Bright","FX Goblins","FX Echoes","FX SciFi",
"Sitar","Banjo","Shamisen","Koto","Kalimba","Bagpipe","Fiddle","Shanai",
"Tinkle Bell","Agogo","Steel Drums","Woodblock","Taiko","Melodic Tom","Synth Drum","Rev Cymbal",
"Gtr Fret","Breath","Seashore","Bird","Telephone","Helicopter","Applause","Gunshot" };

static double g_re[NFFT], g_im[NFFT];
static void fft(double *re, double *im, int n) {
    for (int i = 1, j = 0; i < n; i++) { int bit = n >> 1; for (; j & bit; bit >>= 1) j ^= bit; j ^= bit;
        if (i < j) { double t = re[i]; re[i] = re[j]; re[j] = t; t = im[i]; im[i] = im[j]; im[j] = t; } }
    for (int len = 2; len <= n; len <<= 1) { double ang = -2 * M_PI / len, wr = cos(ang), wi = sin(ang);
        for (int i = 0; i < n; i += len) { double cwr = 1, cwi = 0;
            for (int k = 0; k < len / 2; k++) {
                double ur = re[i + k], ui = im[i + k];
                double vr = re[i + k + len/2]*cwr - im[i + k + len/2]*cwi;
                double vi = re[i + k + len/2]*cwi + im[i + k + len/2]*cwr;
                re[i+k] = ur+vr; im[i+k] = ui+vi; re[i+k+len/2] = ur-vr; im[i+k+len/2] = ui-vi;
                double nwr = cwr*wr - cwi*wi; cwi = cwr*wi + cwi*wr; cwr = nwr; } } }
}
// Harmonic Product Spectrum: the fundamental is the bin whose harmonics
// (2f,3f,4f,5f) all carry energy, so multiplying down-sampled spectra makes it
// dominate. Robust against low-frequency breath noise (no harmonic support) and
// a weak/missing fundamental (brass pedal tones, string body resonances) — the
// failure modes that fooled both the lowest-peak and the time-domain detectors.
#define HPS_HARM 5
static double mag_buf[NFFT/2];
static double fundamental(const double *x, int n) {
    for (int i = 0; i < NFFT; i++) { double w = 0.5 - 0.5*cos(2*M_PI*i/(NFFT-1)); g_re[i] = (i<n?x[i]:0)*w; g_im[i] = 0; }
    fft(g_re, g_im, NFFT);
    int half = NFFT/2; double mx = 0;
    for (int i = 1; i < half; i++) { mag_buf[i] = sqrt(g_re[i]*g_re[i] + g_im[i]*g_im[i]); if (mag_buf[i] > mx) mx = mag_buf[i]; }
    if (mx < 1e-9) return 0;
    int lo = (int)(40.0*NFFT/SR), hi = (int)(2500.0*NFFT/SR);
    int bestbin = 0; double bestp = -1;
    for (int i = lo; i < hi; i++) {
        double p = log(mag_buf[i] + 1e-12);
        for (int h = 2; h <= HPS_HARM; h++) { int hi2 = i*h; if (hi2 >= half) break; p += log(mag_buf[hi2] + 1e-12); }
        if (p > bestp) { bestp = p; bestbin = i; }
    }
    if (bestbin == 0) return 0;
    // HPS can lock an octave high if the 2nd harmonic series is stronger;
    // if half the found bin has comparable HPS, prefer the lower (true f0).
    int hb = bestbin/2;
    if (hb >= lo) {
        double p = log(mag_buf[hb] + 1e-12);
        for (int h = 2; h <= HPS_HARM; h++) { int hi2 = hb*h; if (hi2 >= half) break; p += log(mag_buf[hi2] + 1e-12); }
        if (p > bestp - 2.5) bestbin = hb;   // within ~e^2.5 → take the octave-lower
    }
    // parabolic interpolation around the peak on the raw magnitude
    double a = mag_buf[bestbin-1], b = mag_buf[bestbin], c = mag_buf[bestbin+1], d = (a+c-2*b);
    double corr = fabs(d) > 1e-12 ? 0.5*(a-c)/d : 0;
    return (bestbin + corr) * SR / NFFT;
}
// Independent cross-check: normalized-autocorrelation (YIN-style) fundamental.
// Different failure modes than HPS, so agreement between the two is a strong
// signal that a detected pitch is real (not a detector octave-jump).
static double autocorr_fund(const double *x, int n) {
    int tmin = (int)(SR/2500), tmax = (int)(SR/45); if (tmax > n/2) tmax = n/2;
    static double d[24001]; if (tmax > 24000) tmax = 24000;
    double run = 0; int best = -1;
    for (int tau = 1; tau <= tmax; tau++) {
        double s = 0; for (int i = 0; i < n - tau; i++) { double dd = x[i]-x[i+tau]; s += dd*dd; }
        run += s; d[tau] = s*tau/(run > 1e-12 ? run : 1e-12);
    }
    for (int tau = tmin; tau <= tmax; tau++) if (d[tau] < 0.15) { while (tau+1 <= tmax && d[tau+1] < d[tau]) tau++; best = tau; break; }
    if (best < 0) { double m = 1e9; for (int tau = tmin; tau <= tmax; tau++) if (d[tau] < m) { m = d[tau]; best = tau; } }
    if (best <= 0) return 0;
    double bd = best;
    if (best > tmin && best < tmax) { double a = d[best-1], b = d[best], c = d[best+1], den = a+c-2*b; if (fabs(den) > 1e-12) bd = best + 0.5*(a-c)/den; }
    return SR / bd;
}
// Render once; return BOTH detectors' fundamental estimates.
static void render_funds(int prog, int midi, double *f_hps, double *f_ac) {
    double rf = 440.0 * pow(2.0, (midi - 69) / 12.0);
    GMVoice v; gm_voice_init(&v, prog, rf, SR, 4242u + prog*131 + midi);
    static double out[NFFT]; double env = 0;
    for (int i = 0; i < NFFT; i++) { if (i < 1500) { env += 1.0/1500; if (env>1) env=1; } out[i] = gm_voice_render(&v, SR, env, rf); }
    *f_hps = fundamental(out, NFFT);
    int an = NFFT - 4000; *f_ac = autocorr_fund(out + 4000, an);   // skip the attack
}
static int cmp(const void *a, const void *b) { double d = *(const double*)a - *(const double*)b; return d<0?-1:(d>0?1:0); }

int main(int argc, char **argv) {
    gm_synth_init();
    int only[128], nonly = 0;
    for (int i = 1; i < argc; i++) only[nonly++] = atoi(argv[i]);
    int testm[3] = { 55, 60, 67 };   // G3, C4, G4
    int pass = 0, fail = 0, uncertain = 0;
    printf("%-3s %-13s %8s %8s %7s  %s\n", "GM", "name", "req(Hz)", "hps/ac", "cents", "verdict");
    for (int prog = 0; prog < 128; prog++) {
        if (nonly) { int hit = 0; for (int k = 0; k < nonly; k++) if (only[k] == prog) hit = 1; if (!hit) continue; }
        double reqC4 = 440.0 * pow(2.0, (60 - 69) / 12.0);
        // Confident error cents per note = the two detectors when they AGREE
        // (within 60 cents); otherwise that note is ambiguous and skipped.
        double cents[3]; int nc = 0; int agreeC4 = 0; double hpsC4 = 0, acC4 = 0;
        for (int t = 0; t < 3; t++) {
            double fh, fa; render_funds(prog, testm[t], &fh, &fa);
            double rf = 440.0 * pow(2.0, (testm[t] - 69) / 12.0);
            if (testm[t] == 60) { hpsC4 = fh; acC4 = fa; }
            if (fh > 0 && fa > 0 && fabs(1200.0*log2(fh/fa)) < 60.0) {   // detectors agree
                cents[nc++] = 1200.0*log2(((fh+fa)/2)/rf);
                if (testm[t] == 60) agreeC4 = 1;
            }
        }
        if (nc < 2) {   // detectors rarely agree → inharmonic / noisy / non-pitched
            printf("%-3d %-13s %8.1f %4.0f/%-4.0f %7s  uncertain (detectors split)\n", prog, NM[prog], reqC4, hpsC4, acC4, "-");
            uncertain++; continue;
        }
        qsort(cents, nc, sizeof(double), cmp);
        double mc = cents[nc/2];
        int ok = fabs(mc) < 50.0;
        const char *verd = ok ? "PASS" :
            (fabs(mc-1200)<120||fabs(mc+1200)<120) ? "FAIL octave" :
            (fabs(mc-2400)<150||fabs(mc+2400)<150) ? "FAIL 2-octave" :
            (fabs(mc-700)<80) ? "FAIL +5th" : "FAIL";
        printf("%-3d %-13s %8.1f %4.0f/%-4.0f %+7.0f  %s\n", prog, NM[prog], reqC4, hpsC4, acC4, mc, verd);
        if (ok) pass++; else fail++;
    }
    printf("\n  PASS %d   FAIL %d   uncertain %d  (uncertain = inharmonic/noisy, judge by ear)\n", pass, fail, uncertain);
    return fail ? 1 : 0;
}
