// chain.c — stage dispatcher + spec parser.
//
// Spec syntax (CLI-friendly, colon-separated):
//   1176:ratio=4:in=-12:out=+6:attack=4:release=4:iron=0.5
//   eq:presence=+2                      ← knowledge-graph intent
//   eq:peak:f=3500:q=1.2:g=+2           ← raw biquad
//   eq:hp:f=30                          ← raw biquad (highpass)
//   eq:highshelf:f=11000:g=+1.8         ← raw biquad
//   eq:lowshelf:f=200:g=+1.0
//   eq:lp:f=4500
//
// 1176 knob conventions match the unit:
//   attack: 1..7  (1=800μs / 7=20μs, exponential)
//   release: 1..7 (1=1100ms / 7=50ms, exponential)
//   ratio: 4, 8, 12, 20
//   iron: 0..2 (FET+A12 nonlinearity depth)

#include "acdsp.h"
#include "biquad.h"
#include "comp_1176.h"
#include "eq_graph.h"
#include "util.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

typedef struct stage {
  void (*process)(struct stage *, float *, int, int);
  void (*destroy)(struct stage *);
  void *state;
} stage_t;

struct acdsp_chain {
  int sr, channels;
  stage_t *stages;
  int n_stages, cap_stages;
};

// ── 1176 stage ──────────────────────────────────────────────────────────
typedef struct { comp_1176_t c; } s_1176_t;
static void s_1176_process(stage_t *st, float *buf, int n, int ch) {
  comp_1176_process(&((s_1176_t *)st->state)->c, buf, n, ch);
}
static void s_1176_destroy(stage_t *st) { free(st->state); }

// ── biquad EQ stage (one biquad per channel, shared coeffs) ─────────────
typedef struct { biquad_t bq[2]; } s_eq_t;
static void s_eq_process(stage_t *st, float *buf, int n, int ch) {
  s_eq_t *e = (s_eq_t *)st->state;
  if (ch == 1) {
    for (int i = 0; i < n; i++) buf[i] = biquad_process(&e->bq[0], buf[i]);
  } else {
    for (int i = 0; i < n; i++) {
      buf[i*ch  ] = biquad_process(&e->bq[0], buf[i*ch  ]);
      buf[i*ch+1] = biquad_process(&e->bq[1], buf[i*ch+1]);
    }
  }
}
static void s_eq_destroy(stage_t *st) { free(st->state); }
static void s_eq_set_both(s_eq_t *e, const biquad_t *src) {
  e->bq[0] = *src; biquad_clear(&e->bq[0]);
  e->bq[1] = *src; biquad_clear(&e->bq[1]);
}

// ── parse helpers ────────────────────────────────────────────────────────
// Split "key=val" — returns 1 if of that form, fills key+val (in-place).
static int kv(char *tok, char **key, char **val) {
  char *eq = strchr(tok, '=');
  if (!eq) { *key = tok; *val = NULL; return 0; }
  *eq = '\0';
  *key = tok;
  *val = eq + 1;
  return 1;
}

// Add a stage to the chain, growing if needed.
static int push_stage(acdsp_chain_t *c,
                      void (*proc)(stage_t *, float *, int, int),
                      void (*dest)(stage_t *),
                      void *state) {
  if (c->n_stages >= c->cap_stages) {
    int nc = c->cap_stages ? c->cap_stages * 2 : 4;
    stage_t *ns = (stage_t *)realloc(c->stages, sizeof(stage_t) * nc);
    if (!ns) { acdsp_set_error("chain: oom"); return -1; }
    c->stages = ns;
    c->cap_stages = nc;
  }
  c->stages[c->n_stages].process = proc;
  c->stages[c->n_stages].destroy = dest;
  c->stages[c->n_stages].state   = state;
  c->n_stages++;
  return 0;
}

// ── 1176 spec parser ────────────────────────────────────────────────────
static int parse_1176(acdsp_chain_t *c, char *rest) {
  s_1176_t *s = (s_1176_t *)calloc(1, sizeof(s_1176_t));
  if (!s) { acdsp_set_error("1176: oom"); return -1; }
  comp_1176_init(&s->c, (float)c->sr);

  char *save = NULL;
  for (char *tok = strtok_r(rest, ":", &save); tok; tok = strtok_r(NULL, ":", &save)) {
    char *k, *v;
    if (!kv(tok, &k, &v) || !v) {
      acdsp_set_error("1176: expected key=value, got '%s'", tok);
      free(s); return -1;
    }
    float fv = strtof(v, NULL);
    if      (!strcmp(k, "ratio"))    s->c.ratio        = fv;
    else if (!strcmp(k, "in"))       s->c.input_db     = fv;
    else if (!strcmp(k, "out"))      s->c.output_db    = fv;
    else if (!strcmp(k, "iron"))     s->c.iron         = fv;
    else if (!strcmp(k, "attack"))   s->c.attack_us    = comp_1176_knob_to_attack_us(fv);
    else if (!strcmp(k, "release"))  s->c.release_ms   = comp_1176_knob_to_release_ms(fv);
    else if (!strcmp(k, "attack-us"))    s->c.attack_us    = fv;
    else if (!strcmp(k, "release-ms"))   s->c.release_ms   = fv;
    else if (!strcmp(k, "threshold")) s->c.threshold_db = fv;
    else if (!strcmp(k, "knee"))      s->c.knee_db      = fv;
    else {
      acdsp_set_error("1176: unknown key '%s'", k);
      free(s); return -1;
    }
  }
  comp_1176_update(&s->c);
  return push_stage(c, s_1176_process, s_1176_destroy, s);
}

// ── eq spec parser ──────────────────────────────────────────────────────
// Two forms:
//   eq:<intent>[=gain]                   — knowledge-graph lookup
//   eq:<type>:f=...:q=...:g=...          — raw biquad (peak/hp/lp/highshelf/lowshelf)
static int parse_eq(acdsp_chain_t *c, char *rest) {
  // First token after "eq:" decides the form.
  char *save = NULL;
  char *first = strtok_r(rest, ":", &save);
  if (!first) { acdsp_set_error("eq: empty spec"); return -1; }

  s_eq_t *s = (s_eq_t *)calloc(1, sizeof(s_eq_t));
  if (!s) { acdsp_set_error("eq: oom"); return -1; }
  biquad_t bq; biquad_clear(&bq);

  const char *intent = NULL;
  float intent_gain = 0.0f;
  int   has_gain = 0;

  // Detect "intent=gain" vs "type" — try intent lookup first.
  char *eq = strchr(first, '=');
  if (eq) {
    *eq = '\0';
    intent = first;
    intent_gain = strtof(eq + 1, NULL);
    has_gain = 1;
  } else {
    // Either bare intent ("eq:air") or a raw type ("eq:peak")
    if (eq_graph_find(first)) {
      intent = first;
    }
  }

  if (intent) {
    // eq_graph_apply checks isfinite() on the override — pass a sentinel
    // out-of-band by calling with the default when no override given.
    const eq_intent_t *e = eq_graph_find(intent);
    float gain = has_gain ? intent_gain : (e ? e->gain_default : 0.0f);
    if (eq_graph_apply(&bq, intent, gain, (float)c->sr) != 0) {
      acdsp_set_error("eq: unknown intent '%s'", intent);
      free(s); return -1;
    }
    s_eq_set_both(s, &bq);
    // Any trailing tokens? (No allowed for intent form.)
    char *extra = strtok_r(NULL, ":", &save);
    if (extra) {
      acdsp_set_error("eq: unexpected '%s' after intent '%s'", extra, intent);
      free(s); return -1;
    }
    return push_stage(c, s_eq_process, s_eq_destroy, s);
  }

  // Raw biquad form: first is the type ("peak", "hp", "lp", "highshelf", "lowshelf").
  enum { T_PEAK, T_HP, T_LP, T_HS, T_LS } type;
  if      (!strcmp(first, "peak"))      type = T_PEAK;
  else if (!strcmp(first, "hp"))        type = T_HP;
  else if (!strcmp(first, "highpass"))  type = T_HP;
  else if (!strcmp(first, "lp"))        type = T_LP;
  else if (!strcmp(first, "lowpass"))   type = T_LP;
  else if (!strcmp(first, "highshelf")) type = T_HS;
  else if (!strcmp(first, "lowshelf"))  type = T_LS;
  else {
    acdsp_set_error("eq: unknown type/intent '%s'", first);
    free(s); return -1;
  }

  float f = 1000.0f, q = 0.707f, g = 0.0f;
  for (char *tok = strtok_r(NULL, ":", &save); tok; tok = strtok_r(NULL, ":", &save)) {
    char *k, *v;
    if (!kv(tok, &k, &v) || !v) {
      acdsp_set_error("eq: expected key=value, got '%s'", tok);
      free(s); return -1;
    }
    if      (!strcmp(k, "f"))    f = strtof(v, NULL);
    else if (!strcmp(k, "freq")) f = strtof(v, NULL);
    else if (!strcmp(k, "q"))    q = strtof(v, NULL);
    else if (!strcmp(k, "g"))    g = strtof(v, NULL);
    else if (!strcmp(k, "gain")) g = strtof(v, NULL);
    else { acdsp_set_error("eq: unknown key '%s'", k); free(s); return -1; }
  }

  switch (type) {
    case T_PEAK: biquad_peak     (&bq, f, q, g, (float)c->sr); break;
    case T_HP:   biquad_highpass (&bq, f, q,    (float)c->sr); break;
    case T_LP:   biquad_lowpass  (&bq, f, q,    (float)c->sr); break;
    case T_HS:   biquad_highshelf(&bq, f, q, g, (float)c->sr); break;
    case T_LS:   biquad_lowshelf (&bq, f, q, g, (float)c->sr); break;
  }
  s_eq_set_both(s, &bq);
  return push_stage(c, s_eq_process, s_eq_destroy, s);
}

// ── public API ───────────────────────────────────────────────────────────
acdsp_chain_t *acdsp_chain_new(int sr, int channels) {
  if (sr <= 0 || channels < 1 || channels > 2) {
    acdsp_set_error("chain_new: sr/channels out of range"); return NULL;
  }
  acdsp_chain_t *c = (acdsp_chain_t *)calloc(1, sizeof(*c));
  if (!c) { acdsp_set_error("chain_new: oom"); return NULL; }
  c->sr = sr; c->channels = channels;
  return c;
}

void acdsp_chain_free(acdsp_chain_t *c) {
  if (!c) return;
  for (int i = 0; i < c->n_stages; i++) {
    if (c->stages[i].destroy) c->stages[i].destroy(&c->stages[i]);
  }
  free(c->stages);
  free(c);
}

int acdsp_chain_add(acdsp_chain_t *c, const char *spec) {
  if (!c || !spec) { acdsp_set_error("chain_add: null"); return -1; }
  char *copy = strdup(spec);
  if (!copy) { acdsp_set_error("chain_add: oom"); return -1; }

  // First colon separates stage name from the rest.
  char *colon = strchr(copy, ':');
  char *rest = "";
  if (colon) { *colon = '\0'; rest = colon + 1; }
  char *name = acdsp_strtrim(copy);

  int rc;
  if      (!strcmp(name, "1176")) rc = parse_1176(c, rest);
  else if (!strcmp(name, "eq"))   rc = parse_eq  (c, rest);
  else { acdsp_set_error("chain_add: unknown stage '%s'", name); rc = -1; }
  free(copy);
  return rc;
}

void acdsp_chain_process(acdsp_chain_t *c, float *buf, int n_frames) {
  if (!c) return;
  for (int i = 0; i < c->n_stages; i++) {
    c->stages[i].process(&c->stages[i], buf, n_frames, c->channels);
  }
}

int acdsp_process(float *buf, int n_frames, int sr, int channels, const char *spec) {
  acdsp_chain_t *c = acdsp_chain_new(sr, channels);
  if (!c) return -1;
  char *copy = strdup(spec);
  if (!copy) { acdsp_chain_free(c); acdsp_set_error("process: oom"); return -1; }
  char *save = NULL;
  for (char *tok = strtok_r(copy, " \t\n", &save); tok; tok = strtok_r(NULL, " \t\n", &save)) {
    if (acdsp_chain_add(c, tok) != 0) {
      free(copy); acdsp_chain_free(c); return -1;
    }
  }
  free(copy);
  acdsp_chain_process(c, buf, n_frames);
  acdsp_chain_free(c);
  return 0;
}
