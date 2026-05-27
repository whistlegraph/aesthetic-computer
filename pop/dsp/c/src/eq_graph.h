#ifndef ACDSP_EQ_GRAPH_H
#define ACDSP_EQ_GRAPH_H

#include "biquad.h"

typedef enum {
  EQ_KIND_PEAK,
  EQ_KIND_LOWSHELF,
  EQ_KIND_HIGHSHELF,
  EQ_KIND_HIGHPASS,
  EQ_KIND_LOWPASS,
} eq_kind_t;

typedef struct {
  const char *intent;
  eq_kind_t   kind;
  float       freq;
  float       q;
  float       gain_default;
} eq_intent_t;

const eq_intent_t *eq_graph_find(const char *intent);

// Configure a biquad from an intent (NULL = unknown intent). If gain_db_override
// is finite, it overrides gain_default (no effect for HP/LP).
int eq_graph_apply(biquad_t *bq, const char *intent, float gain_db_override, float sr);

#endif
