// swank-bridge.h — Evaluate Common Lisp via local Swank server (port 4005)
#ifndef SWANK_BRIDGE_H
#define SWANK_BRIDGE_H

// Evaluate a CL expression via the local Swank server.
// Returns 0 on success, -1 on error.
// Result is written to `result` (up to result_len bytes).
int swank_eval(const char *expr, char *result, int result_len);

// Check if Swank server is running (connect test).
int swank_available(void);

#endif
