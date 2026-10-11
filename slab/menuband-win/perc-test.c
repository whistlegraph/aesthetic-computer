// perc-test.c — render the kit through percussion.c into a WAV and report.
//   cl /O2 /std:c17 perc-test.c percussion.c
//   perc-test.exe out.wav
// Each of the twelve drums (held drums released after 200 ms), then three
// TrackDrum strikes (centre, mid-radius, rim), half a second each at 48 kHz.
#define _USE_MATH_DEFINES
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "percussion.h"

#define SR 48000
#define SLOT (SR / 2)
#define BLOCK 480

int main(int argc, char **argv) {
    const char *path = argc > 1 ? argv[1] : "perc-test.wav";
    perc_init(SR);
    perc_set_level(0.90);
    int slots = PERC_DRUM_COUNT + 3, total = slots * SLOT;
    float *buf = (float *)calloc((size_t)total * 2, sizeof(float));
    const double pads[3][2] = { { 0.5, 0.5 }, { 0.70, 0.5 }, { 0.975, 0.5 } };
    const char *padNames[3] = { "pad centre", "pad mid", "pad rim" };
    for (int s = 0; s < slots; s++) {
        int base = s * SLOT; uint64_t group = (uint64_t)(s + 1);
        if (s < PERC_DRUM_COUNT) perc_hit_drum((PercDrum)s, 100.0 / 127.0, 0.0, group);
        else perc_strike_pad(pads[s - PERC_DRUM_COUNT][0], pads[s - PERC_DRUM_COUNT][1], 100.0 / 127.0, NULL, 0, group);
        for (int off = 0; off < SLOT; off += BLOCK) {
            if (off == SR / 5) { if (s < PERC_DRUM_COUNT) perc_release(group); else perc_lift_pad(pads[s - PERC_DRUM_COUNT][0], pads[s - PERC_DRUM_COUNT][1], group); }
            perc_render(buf + (size_t)(base + off) * 2, BLOCK);
        }
        double peak = 0; int last = 0;
        for (int i = 0; i < SLOT; i++) { double a = fabs(buf[(size_t)(base + i) * 2]); if (a > peak) peak = a; if (a > 0.001) last = i; }
        printf("%-12s peak %.3f  rings %.3f s\n", s < PERC_DRUM_COUNT ? perc_label((PercDrum)s) : padNames[s - PERC_DRUM_COUNT], peak, last / (double)SR);
    }
    FILE *f = fopen(path, "wb"); if (!f) { puts("cannot write"); return 1; }
    unsigned long dataBytes = (unsigned long)total * 4, riff = 36 + dataBytes, fmtLen = 16, byteRate = SR * 4, sr = SR;
    unsigned short fmt = 1, ch = 2, block = 4, bits = 16;
    fwrite("RIFF", 1, 4, f); fwrite(&riff, 4, 1, f); fwrite("WAVEfmt ", 1, 8, f); fwrite(&fmtLen, 4, 1, f); fwrite(&fmt, 2, 1, f); fwrite(&ch, 2, 1, f);
    fwrite(&sr, 4, 1, f); fwrite(&byteRate, 4, 1, f); fwrite(&block, 2, 1, f); fwrite(&bits, 2, 1, f); fwrite("data", 1, 4, f); fwrite(&dataBytes, 4, 1, f);
    for (size_t i = 0; i < (size_t)total * 2; i++) { float v = buf[i]; if (v > 1) v = 1; if (v < -1) v = -1; short o = (short)(v * 32767); fwrite(&o, 2, 1, f); }
    fclose(f);
    printf("wrote %s (%d slots x 0.5 s)\n", path, slots);
    return 0;
}
