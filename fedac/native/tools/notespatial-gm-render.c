// Batch adapter to the SAME GM synthesis core used by AC Native.
// stdin: program hz samples attackSamples decaySamples seed; stdout: float32 PCM.
// The JS caller owns routing, mixing and output conversion. No audio device.
#include "gm_synth.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdint.h>

int main(int argc, char **argv) {
    double sr = argc == 2 ? atof(argv[1]) : 0;
    if (sr < 8000 || sr > 192000) return 2;
    int program, samples, count = 0;
    double hz, attack, decay;
    unsigned int seed;
    GMVoice *v = calloc(1, sizeof(GMVoice));
    if (!v) return 3;
    float buf[4096];
    int fields;
    while ((fields = scanf("%d %lf %d %lf %lf %u", &program, &hz, &samples, &attack, &decay, &seed)) == 6) {
        if (program < 0 || program > 127 || !isfinite(hz) || hz <= 0 || hz >= sr / 2 ||
            samples <= 0 || samples > sr * 120 || !isfinite(attack) || !isfinite(decay) || attack < 0 || decay < 0) return 4;
        if (gm_voice_init(v, program, hz, sr, seed ? seed : 1) != 0) return 5;
        double decay_start = fmax(0, samples - decay);
        for (int b = 0; b < samples; b += 4096) {
            int n = samples - b < 4096 ? samples - b : 4096;
            for (int i = 0; i < n; i++) {
                int t = b + i;
                double env = attack > 0 && t < attack ? t / attack : 1;
                if (decay > 0 && t > decay_start) env *= fmax(0, 1 - (t - decay_start) / decay);
                double sample = gm_voice_render(v, sr, env, hz);
                if (!isfinite(sample)) { fprintf(stderr, "GM %d produced nonfinite audio\n", program); return 6; }
                buf[i] = (float)sample;
            }
            if (fwrite(buf, sizeof(float), n, stdout) != (size_t)n) return 7;
        }
        count++;
    }
    free(v);
    if (fields != EOF || ferror(stdin) || fflush(stdout)) return 8;
    fprintf(stderr, "native GM: %d notes rendered\n", count);
    return 0;
}
