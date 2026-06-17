// gm-audition.c — offline audition / static test of the AC-native GM synth.
//
// Renders every implemented General-MIDI program from the shared synthesis
// core (fedac/native/src/gm_synth.c, the same engine Menu Band's "Use AC OS
// MIDI" mode and AC OS itself play) as a short held C-major chord, back to
// back, into ONE continuous 48 kHz stereo WAV. Prints a tracklist (timestamp →
// instrument) and flags any program that comes out silent — the quick way to
// hear the whole set and catch a broken/unstable voice without booting the app.
//
// Build + run:  cc -O2 gm-audition.c ../Sources/CGMSynth/gm_synth.c -lm -o /tmp/gm-audition
//               /tmp/gm-audition  <output.wav>
//
// It is a TEST as much as a demo: exit code is non-zero if any program renders
// silent or non-finite, so it can gate a synth-core change in CI.

#include "gm_synth.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdint.h>

static const char *GM_NAMES[128] = {
    "Acoustic Grand Piano","Bright Acoustic Piano","Electric Grand Piano","Honky-tonk Piano",
    "Electric Piano 1","Electric Piano 2","Harpsichord","Clavinet",
    "Celesta","Glockenspiel","Music Box","Vibraphone",
    "Marimba","Xylophone","Tubular Bells","Dulcimer",
    "Drawbar Organ","Percussive Organ","Rock Organ","Church Organ",
    "Reed Organ","Accordion","Harmonica","Tango Accordion",
    "Acoustic Guitar (nylon)","Acoustic Guitar (steel)","Electric Guitar (jazz)","Electric Guitar (clean)",
    "Electric Guitar (muted)","Overdriven Guitar","Distortion Guitar","Guitar Harmonics",
    "Acoustic Bass","Electric Bass (finger)","Electric Bass (pick)","Fretless Bass",
    "Slap Bass 1","Slap Bass 2","Synth Bass 1","Synth Bass 2",
    "Violin","Viola","Cello","Contrabass",
    "Tremolo Strings","Pizzicato Strings","Orchestral Harp","Timpani",
    "String Ensemble 1","String Ensemble 2","Synth Strings 1","Synth Strings 2",
    "Choir Aahs","Voice Oohs","Synth Choir","Orchestra Hit",
    "Trumpet","Trombone","Tuba","Muted Trumpet",
    "French Horn","Brass Section","Synth Brass 1","Synth Brass 2",
    "Soprano Sax","Alto Sax","Tenor Sax","Baritone Sax",
    "Oboe","English Horn","Bassoon","Clarinet",
    "Piccolo","Flute","Recorder","Pan Flute",
    "Blown Bottle","Shakuhachi","Whistle","Ocarina",
    "Lead 1 (square)","Lead 2 (sawtooth)","Lead 3 (calliope)","Lead 4 (chiff)",
    "Lead 5 (charang)","Lead 6 (voice)","Lead 7 (fifths)","Lead 8 (bass + lead)",
    "Pad 1 (new age)","Pad 2 (warm)","Pad 3 (polysynth)","Pad 4 (choir)",
    "Pad 5 (bowed)","Pad 6 (metallic)","Pad 7 (halo)","Pad 8 (sweep)",
    "FX 1 (rain)","FX 2 (soundtrack)","FX 3 (crystal)","FX 4 (atmosphere)",
    "FX 5 (brightness)","FX 6 (goblins)","FX 7 (echoes)","FX 8 (sci-fi)",
    "Sitar","Banjo","Shamisen","Koto",
    "Kalimba","Bagpipe","Fiddle","Shanai",
    "Tinkle Bell","Agogo","Steel Drums","Woodblock",
    "Taiko Drum","Melodic Tom","Synth Drum","Reverse Cymbal",
    "Guitar Fret Noise","Breath Noise","Seashore","Bird Tweet",
    "Telephone Ring","Helicopter","Applause","Gunshot",
};

#define SR        48000.0
#define CHORD_N   4
static const int CHORD[CHORD_N] = { 60, 64, 67, 72 };   // C4 E4 G4 C5

// Per-program timing (seconds): attack, sustain-hold, release, trailing gap.
#define T_ATTACK  0.006
#define T_HOLD    1.30
#define T_REL     0.45
#define T_GAP     0.30

static double midi_freq(int m) { return 440.0 * pow(2.0, (m - 69) / 12.0); }

int main(int argc, char **argv) {
    const char *out_path = (argc > 1) ? argv[1] : "gm-audition.wav";
    gm_synth_init();

    const int sr = (int)SR;
    const long note_samps = (long)((T_HOLD + T_REL) * SR);
    const long gap_samps  = (long)(T_GAP * SR);
    const long prog_samps = note_samps + gap_samps;
    const long rel_start  = (long)(T_HOLD * SR);
    const double atk_inc  = 1.0 / (T_ATTACK * SR);
    const double rel_dec  = 1.0 / (T_REL * SR);

    // Count implemented programs up front to size the buffer.
    int impl[128]; int n_impl = 0;
    for (int p = 0; p < 128; p++) { impl[p] = gm_program_implemented(p) == 1; if (impl[p]) n_impl++; }

    long total = prog_samps * n_impl;
    double *mix = calloc((size_t)total, sizeof(double));   // mono master, normalized later
    if (!mix) { fprintf(stderr, "out of memory (%ld samples)\n", total); return 2; }

    int silent = 0, nonfinite = 0;
    long cursor = 0;
    printf("  time   GM   instrument                          peak\n");
    printf("  -----  ---  ----------------------------------  ------\n");

    for (int p = 0; p < 128; p++) {
        if (!impl[p]) continue;
        double sec = cursor / SR;

        // One GMVoice per chord note; a shared AR envelope gates them together.
        GMVoice v[CHORD_N];
        for (int c = 0; c < CHORD_N; c++) {
            uint32_t seed = (uint32_t)(p * 2654435761u + CHORD[c] * 40503u + 0x1234u);
            gm_voice_init(&v[c], p, midi_freq(CHORD[c]), SR, seed);
        }

        double env = 0.0; int releasing = 0;
        double peak = 0.0; long bad = 0;
        for (long i = 0; i < note_samps; i++) {
            if (i >= rel_start) releasing = 1;
            if (releasing) { env -= rel_dec; if (env < 0) env = 0; }
            else if (env < 1.0) { env += atk_inc; if (env > 1.0) env = 1.0; }

            double s = 0.0;
            for (int c = 0; c < CHORD_N; c++) {
                double f = midi_freq(CHORD[c]);
                double x = gm_voice_render(&v[c], SR, env, f);
                if (!isfinite(x)) { bad++; x = 0.0; }
                s += x;
            }
            s *= 0.28;                 // headroom for 4 summed voices
            s = tanh(s);               // gentle safety saturation
            if (cursor + i < total) mix[cursor + i] = s;
            double a = fabs(s); if (a > peak) peak = a;
        }

        if (bad) nonfinite++;
        double peak_db = peak > 1e-9 ? 20.0 * log10(peak) : -120.0;
        int is_silent = peak < 0.001;
        if (is_silent) silent++;
        printf("  %2d:%05.2f  %3d  %-34s  %5.1f dB%s%s\n",
               (int)(sec / 60), fmod(sec, 60.0), p, GM_NAMES[p], peak_db,
               is_silent ? "  <SILENT>" : "", bad ? "  <NONFINITE>" : "");

        cursor += prog_samps;          // note + trailing gap (gap stays zero)
    }

    // Normalize the whole file to -1 dBFS so quiet and loud voices are audible
    // together without any clipping.
    double gpeak = 0.0;
    for (long i = 0; i < total; i++) { double a = fabs(mix[i]); if (a > gpeak) gpeak = a; }
    double norm = gpeak > 1e-9 ? (0.891 / gpeak) : 1.0;   // 0.891 ≈ -1 dBFS

    // ---- Write 16-bit stereo WAV (mono content duplicated L/R). ----
    FILE *f = fopen(out_path, "wb");
    if (!f) { fprintf(stderr, "cannot open %s\n", out_path); free(mix); return 2; }
    long frames = total;
    int channels = 2, bits = 16;
    long data_bytes = frames * channels * (bits / 8);
    long byte_rate = (long)sr * channels * (bits / 8);
    uint16_t block_align = channels * (bits / 8);
    uint32_t u32; uint16_t u16;
    fwrite("RIFF", 1, 4, f); u32 = (uint32_t)(36 + data_bytes); fwrite(&u32, 4, 1, f);
    fwrite("WAVE", 1, 4, f);
    fwrite("fmt ", 1, 4, f); u32 = 16; fwrite(&u32, 4, 1, f);
    u16 = 1; fwrite(&u16, 2, 1, f);                  // PCM
    u16 = channels; fwrite(&u16, 2, 1, f);
    u32 = sr; fwrite(&u32, 4, 1, f);
    u32 = (uint32_t)byte_rate; fwrite(&u32, 4, 1, f);
    fwrite(&block_align, 2, 1, f);
    u16 = bits; fwrite(&u16, 2, 1, f);
    fwrite("data", 1, 4, f); u32 = (uint32_t)data_bytes; fwrite(&u32, 4, 1, f);
    for (long i = 0; i < frames; i++) {
        double s = mix[i] * norm;
        if (s > 1.0) s = 1.0; if (s < -1.0) s = -1.0;
        int16_t v = (int16_t)lround(s * 32767.0);
        fwrite(&v, 2, 1, f); fwrite(&v, 2, 1, f);    // L, R
    }
    fclose(f);
    free(mix);

    double dur = total / SR;
    printf("\n  %d programs · %.1f s · %s\n", n_impl, dur, out_path);
    if (silent)    printf("  WARNING: %d program(s) rendered SILENT\n", silent);
    if (nonfinite) printf("  WARNING: %d program(s) hit NON-FINITE samples (trapped)\n", nonfinite);
    return (silent || nonfinite) ? 1 : 0;
}
