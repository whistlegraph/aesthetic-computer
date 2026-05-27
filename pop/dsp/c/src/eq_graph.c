// Compiled-in mirror of pop/dsp/eq-graph.json. The JSON is the source of
// truth (it's also read by pop/lib/master.mjs in node land); this table
// keeps the C lib zero-dep — no JSON parser pulled in. Keep in sync when
// editing the JSON; the tiny script bin/gen-eq-table.mjs (TODO) will
// codegen this file from the JSON when the graph grows.

#include "eq_graph.h"
#include <math.h>
#include <string.h>

static const eq_intent_t TABLE[] = {
  { "sub",          EQ_KIND_HIGHPASS,    30.0f, 0.707f,  0.0f },
  { "rumble",       EQ_KIND_HIGHPASS,    60.0f, 0.707f,  0.0f },
  { "warmth",       EQ_KIND_PEAK,       150.0f, 1.0f,    1.5f },
  { "mud",          EQ_KIND_PEAK,       250.0f, 1.0f,   -2.0f },
  { "chest",        EQ_KIND_PEAK,       220.0f, 1.0f,    2.0f },
  { "boxy",         EQ_KIND_PEAK,       500.0f, 1.4f,   -2.0f },
  { "nasal",        EQ_KIND_PEAK,       900.0f, 1.6f,   -2.0f },
  { "honk",         EQ_KIND_PEAK,      1200.0f, 1.6f,   -2.0f },
  { "harshness",    EQ_KIND_PEAK,      3000.0f, 1.4f,   -2.0f },
  { "presence",     EQ_KIND_PEAK,      4000.0f, 1.2f,    2.0f },
  { "bite",         EQ_KIND_PEAK,      5500.0f, 1.0f,    1.5f },
  { "sibilance",    EQ_KIND_PEAK,      7500.0f, 2.0f,   -3.0f },
  { "air",          EQ_KIND_HIGHSHELF,11000.0f, 0.707f,  1.8f },
  { "sparkle",      EQ_KIND_HIGHSHELF,14000.0f, 0.707f,  1.5f },
  { "tilt-bright",  EQ_KIND_HIGHSHELF, 8000.0f, 0.5f,    1.2f },
  { "tilt-warm",    EQ_KIND_LOWSHELF,   200.0f, 0.5f,    1.0f },
};
static const int TABLE_N = (int)(sizeof(TABLE) / sizeof(TABLE[0]));

const eq_intent_t *eq_graph_find(const char *intent) {
  if (!intent) return NULL;
  for (int i = 0; i < TABLE_N; i++) {
    if (strcmp(intent, TABLE[i].intent) == 0) return &TABLE[i];
  }
  return NULL;
}

int eq_graph_apply(biquad_t *bq, const char *intent, float gain_db, float sr) {
  const eq_intent_t *e = eq_graph_find(intent);
  if (!e) return -1;
  float g = gain_db;
  biquad_clear(bq);
  switch (e->kind) {
    case EQ_KIND_PEAK:      biquad_peak     (bq, e->freq, e->q, g, sr); break;
    case EQ_KIND_LOWSHELF:  biquad_lowshelf (bq, e->freq, e->q, g, sr); break;
    case EQ_KIND_HIGHSHELF: biquad_highshelf(bq, e->freq, e->q, g, sr); break;
    case EQ_KIND_HIGHPASS:  biquad_highpass (bq, e->freq, e->q,    sr); break;
    case EQ_KIND_LOWPASS:   biquad_lowpass  (bq, e->freq, e->q,    sr); break;
  }
  return 0;
}
