// acdsp CLI — wraps the lib for the pop/ pipeline.
//
//   acdsp in.wav out.wav --chain "1176:ratio=4:in=-12:out=+6 eq:presence=+2 eq:air=+1.5"
//
// Flags:
//   --chain "..."        — required, space-separated stage specs.
//   --bits 16|24|32      — output PCM int width (default 24). Ignored if --float.
//   --float              — write 32-bit IEEE float WAV (preserve headroom).
//   --quiet              — silence per-stage telemetry.

#include "acdsp.h"
#include "wav.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

static void usage(void) {
  fputs(
    "usage: acdsp in.wav out.wav --chain \"stage1 stage2 ...\"\n"
    "             [--bits 16|24|32] [--float] [--quiet]\n"
    "stages:\n"
    "  1176:ratio=4:in=-12:out=+6:attack=4:release=4:iron=0.5\n"
    "  eq:<intent>[=gain_db]    e.g. eq:presence=+2, eq:air=+1.5, eq:sub\n"
    "  eq:peak:f=3500:q=1.2:g=+2\n"
    "  eq:hp:f=30\n"
    "  eq:highshelf:f=11000:g=+1.8\n"
    "intents: sub rumble warmth mud chest boxy nasal honk harshness\n"
    "         presence bite sibilance air sparkle tilt-bright tilt-warm\n",
    stderr);
}

int main(int argc, char **argv) {
  const char *in_path = NULL, *out_path = NULL, *chain = NULL;
  int bits = 24, is_float = 0, quiet = 0;

  int pos = 0;
  for (int i = 1; i < argc; i++) {
    if      (!strcmp(argv[i], "--chain") && i + 1 < argc) chain    = argv[++i];
    else if (!strcmp(argv[i], "--bits")  && i + 1 < argc) bits     = atoi(argv[++i]);
    else if (!strcmp(argv[i], "--float"))                 is_float = 1;
    else if (!strcmp(argv[i], "--quiet"))                 quiet    = 1;
    else if (!strcmp(argv[i], "-h") || !strcmp(argv[i], "--help")) { usage(); return 0; }
    else if (argv[i][0] == '-')   { fprintf(stderr, "acdsp: unknown flag %s\n", argv[i]); usage(); return 2; }
    else if (pos == 0)            { in_path  = argv[i]; pos++; }
    else if (pos == 1)            { out_path = argv[i]; pos++; }
    else                          { fprintf(stderr, "acdsp: extra arg %s\n", argv[i]); return 2; }
  }
  if (!in_path || !out_path || !chain) { usage(); return 2; }
  if (is_float) bits = 32;
  if (bits != 16 && bits != 24 && bits != 32) {
    fprintf(stderr, "acdsp: --bits must be 16, 24, or 32\n"); return 2;
  }

  wav_t in;
  if (wav_read(in_path, &in) != 0) {
    fprintf(stderr, "acdsp: read %s: %s\n", in_path, acdsp_last_error()); return 1;
  }
  if (!quiet) {
    fprintf(stderr, "acdsp: %s  %dHz / %dch / %d frames  (%.2fs)\n",
            in_path, in.sample_rate, in.channels, in.n_frames,
            (double)in.n_frames / (double)in.sample_rate);
  }

  struct timespec t0, t1; clock_gettime(CLOCK_MONOTONIC, &t0);
  int rc = acdsp_process(in.data, in.n_frames, in.sample_rate, in.channels, chain);
  clock_gettime(CLOCK_MONOTONIC, &t1);
  if (rc != 0) {
    fprintf(stderr, "acdsp: chain error: %s\n", acdsp_last_error());
    wav_free(&in); return 1;
  }
  if (!quiet) {
    double sec = (t1.tv_sec - t0.tv_sec) + (t1.tv_nsec - t0.tv_nsec) * 1e-9;
    double audio_s = (double)in.n_frames / (double)in.sample_rate;
    fprintf(stderr, "acdsp: chain '%s'\n        ran in %.3fs (%.1fx realtime)\n",
            chain, sec, audio_s / (sec > 0 ? sec : 1e-9));
  }

  if (wav_write(out_path, &in, bits, is_float) != 0) {
    fprintf(stderr, "acdsp: write %s: %s\n", out_path, acdsp_last_error());
    wav_free(&in); return 1;
  }
  if (!quiet) fprintf(stderr, "acdsp: wrote %s (%dbit%s)\n",
                       out_path, bits, is_float ? " float" : "");
  wav_free(&in);
  return 0;
}
