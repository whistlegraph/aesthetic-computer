// sf2render.c — render an audition phrase from an SF2 preset, for the
// sample-library audition (audition-sample-libraries.py). Celesta and the
// freepats guitar ship only as SF2, so they go through TinySoundFont, the
// same player Menu Band for Windows uses.
//
//   cc -O2 -I slab/menuband-win sf2render.c -o sf2render -lm
//   ./sf2render bank.sf2 preset_index out.f32 < events
//
// events: one "onset_seconds duration_seconds midi" per line; a negative
// duration means let it ring (no note-off). Output: f32le mono 48 kHz.

#define TSF_IMPLEMENTATION
#include "tsf.h"
#include <stdio.h>
#include <stdlib.h>

#define RATE 48000
#define MAX_EVENTS 256

typedef struct { double t; int midi; int on; } Edge;

static int by_time(const void* a, const void* b) {
  double d = ((const Edge*)a)->t - ((const Edge*)b)->t;
  return d < 0 ? -1 : d > 0 ? 1 : 0;
}

int main(int argc, char** argv) {
  if (argc < 4) { fprintf(stderr, "usage: sf2render bank.sf2 preset out.f32 < events\n"); return 1; }
  tsf* f = tsf_load_filename(argv[1]);
  if (!f) { fprintf(stderr, "could not load %s\n", argv[1]); return 1; }
  int preset = atoi(argv[2]);
  tsf_set_output(f, TSF_MONO, RATE, 0);

  Edge edges[MAX_EVENTS * 2];
  int n = 0;
  double on, dur, end = 0;
  int midi;
  while (n < MAX_EVENTS * 2 - 1 && scanf("%lf %lf %d", &on, &dur, &midi) == 3) {
    edges[n++] = (Edge){ on, midi, 1 };
    if (dur >= 0) edges[n++] = (Edge){ on + dur, midi, 0 };
    double stop = on + (dur >= 0 ? dur : 4.0);
    if (stop > end) end = stop;
  }
  qsort(edges, n, sizeof(Edge), by_time);

  int total = (int)((end + 1.5) * RATE);
  float* out = calloc(total, sizeof(float));
  int cursor = 0;
  for (int i = 0; i <= n; i++) {
    int until = i < n ? (int)(edges[i].t * RATE) : total;
    if (until > total) until = total;
    if (until > cursor) { tsf_render_float(f, out + cursor, until - cursor, 0); cursor = until; }
    if (i == n) break;
    if (edges[i].on) tsf_note_on(f, preset, edges[i].midi, 0.7f);
    else tsf_note_off(f, preset, edges[i].midi);
  }

  FILE* o = fopen(argv[3], "wb");
  fwrite(out, sizeof(float), total, o);
  fclose(o);
  free(out);
  tsf_close(f);
  return 0;
}
