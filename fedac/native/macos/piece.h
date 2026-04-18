// piece.h — QuickJS piece runtime for ac-native macOS host.
// Hosts a single AC piece: loads the source, registers JS bindings that draw
// into a shared ARGB8888 framebuffer, and dispatches lifecycle calls.
#ifndef AC_MACOS_PIECE_H
#define AC_MACOS_PIECE_H

#include <stdint.h>

typedef struct {
    uint32_t *pixels;
    int width;
    int height;
    int stride;  // pixels per row
} PieceFB;

typedef struct {
    // Event fields surfaced to JS via e.is() / e.x / e.y / e.key.
    // type examples: "keyboard:down:a", "keyboard:up:space", "touch", "lift", "draw"
    // key: for keyboard events, the AC key name ("a", "arrowleft"); empty otherwise.
    char type[48];
    char key[32];
    int x, y;
} PieceEvent;

typedef struct PieceCtx PieceCtx;

// Create runtime, load source at path, prepare lifecycle fn lookups.
PieceCtx *piece_load(const char *path, PieceFB *fb);

// Call lifecycle functions (no-ops if the piece doesn't define them).
void piece_boot(PieceCtx *ctx);
void piece_paint(PieceCtx *ctx);
void piece_sim(PieceCtx *ctx);
void piece_act(PieceCtx *ctx, const PieceEvent *ev);

// Teardown.
void piece_destroy(PieceCtx *ctx);

#endif
