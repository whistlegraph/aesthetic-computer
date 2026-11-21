// nccd_engine.h — the C face of NCCd's Signal engine.
//
// One engine runs on a thread of its own. Requests go in as JSON lines, and
// replies and events come back as JSON lines through the callback, in the
// shape signal-cli's jsonRpc uses, so the app reads both the same way.
#ifndef NCCD_ENGINE_H
#define NCCD_ENGINE_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct NccdEngine NccdEngine;

// Called from the engine's thread. `json` is valid only for the call.
typedef void (*NccdEngineCallback)(void *ctx, const char *json);

// config: {"db": path, "key": 64 hex characters, "attachments": dir,
//          "link": device name (only to link a new device)}
// Returns NULL when the config is not valid.
NccdEngine *nccd_engine_start(const char *config, NccdEngineCallback callback, void *ctx);

// {"id": "1", "method": "send", "params": {...}}. Returns 0 when it was taken.
int nccd_engine_request(NccdEngine *engine, const char *json);

// Stops the engine and frees it. No callback runs after this returns.
void nccd_engine_stop(NccdEngine *engine);

#ifdef __cplusplus
}
#endif
#endif
