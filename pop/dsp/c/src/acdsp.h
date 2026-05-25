// acdsp.h — Aesthetic Computer DSP library: public C API.
//
// One C library, three build targets:
//   • native CLI (`acdsp in.wav out.wav --chain "..."`) for the pop/ pipeline,
//   • wasm bundle for the browser runtime (pieces can call comp/EQ at paint time),
//   • static lib for AC Native (when the kernel grows audio out).
//
// Float audio everywhere. Interleaved [L,R,L,R,...] for stereo.

#ifndef ACDSP_H
#define ACDSP_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct acdsp_chain acdsp_chain_t;

acdsp_chain_t *acdsp_chain_new(int sample_rate, int channels);
void           acdsp_chain_free(acdsp_chain_t *c);

// Add a stage to the chain. `spec` is the CLI syntax:
//   "1176:ratio=4:in=-12:out=+6:attack=0.4:release=200"
//   "eq:presence=+2"     ← intent lookup via eq-graph.json
//   "eq:peak:f=3500:q=1.2:g=+2"   ← raw biquad
//   "eq:hp:f=30"
//   "eq:highshelf:f=11000:g=+1.8"
// Returns 0 on success, -1 on parse error.
int  acdsp_chain_add(acdsp_chain_t *c, const char *spec);

// Process an interleaved float buffer in place.
void acdsp_chain_process(acdsp_chain_t *c, float *buf, int n_frames);

// Convenience: build a chain from a space-separated spec list,
// process the whole buffer, then free. Returns 0 on success.
int  acdsp_process(float *buf, int n_frames, int sr, int channels,
                   const char *chain_spec);

// Last error message from the most recent call on this thread (NULL if none).
const char *acdsp_last_error(void);

#ifdef __cplusplus
}
#endif
#endif
