// ac-native macOS host — stage 2
// SDL3 window + QuickJS-hosted AC piece. The piece module owns drawing; this
// file owns the SDL3 loop, texture upload, and event translation.

#include <SDL3/SDL.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>
#include <libgen.h>
#include <mach-o/dyld.h>

#include "piece.h"
#include "audio.h"

#define FB_W 640
#define FB_H 400
#define WIN_SCALE 2

// Translate an SDL_Keycode into the AC key-name used in event types
// ("keyboard:down:<name>"). Covers what hello.mjs and notepat need; extend
// as more pieces are tested.
static void sdl_key_name(SDL_Keycode k, char *out, size_t n) {
    const char *s = NULL;
    switch (k) {
        case SDLK_LEFT:      s = "arrowleft";  break;
        case SDLK_RIGHT:     s = "arrowright"; break;
        case SDLK_UP:        s = "arrowup";    break;
        case SDLK_DOWN:      s = "arrowdown";  break;
        case SDLK_SPACE:     s = "space";      break;
        case SDLK_RETURN:    s = "enter";      break;
        case SDLK_ESCAPE:    s = "escape";     break;
        case SDLK_BACKSPACE: s = "backspace";  break;
        case SDLK_TAB:       s = "tab";        break;
        default: break;
    }
    if (s) { snprintf(out, n, "%s", s); return; }
    // Printable ASCII: lowercase name, matching AC's pattern.
    if (k >= 32 && k < 127) {
        char c = (char)k;
        if (c >= 'A' && c <= 'Z') c = (char)(c - 'A' + 'a');
        snprintf(out, n, "%c", c);
        return;
    }
    // Fall back to SDL's own key name for anything unmapped.
    const char *sdl_name = SDL_GetKeyName(k);
    if (sdl_name && *sdl_name) snprintf(out, n, "%s", sdl_name);
    else snprintf(out, n, "key%u", (unsigned)k);
}

// If the executable lives inside an .app bundle (path ends with
// Contents/MacOS/<name>), set AC_LIB_PATH and default the piece to the
// bundled Resources/piece.mjs. Returns the piece path to use (points
// into `piece_buf` if a bundle match was found, otherwise NULL).
static const char *detect_bundle(char *piece_buf, size_t piece_sz,
                                 char *lib_buf, size_t lib_sz) {
    char exe[1024];
    uint32_t n = (uint32_t)sizeof(exe);
    if (_NSGetExecutablePath(exe, &n) != 0) return NULL;
    // Resolve symlinks / `./` segments so dirname lands on the real bundle.
    char resolved[1024];
    if (!realpath(exe, resolved)) snprintf(resolved, sizeof(resolved), "%s", exe);
    char *dir = dirname(resolved);
    // Expect ".../Contents/MacOS/<exec>". If so, Resources/ sits next to it.
    if (strstr(dir, "/Contents/MacOS")) {
        snprintf(piece_buf, piece_sz, "%s/../Resources/piece.mjs", dir);
        snprintf(lib_buf,   lib_sz,   "%s/../Resources/lib",       dir);
        if (access(piece_buf, R_OK) == 0) {
            setenv("AC_LIB_PATH", lib_buf, 1);
            return piece_buf;
        }
    }
    return NULL;
}

int main(int argc, char **argv) {
    // --test-tone: exercise the audio engine only; no window, no piece.
    // Plays a 440 Hz sine for ~1s and prints the peak output sample. Useful
    // for verifying audio works in isolation (CI / headless regression).
    if (argc > 1 && strcmp(argv[1], "--test-tone") == 0) {
        if (!SDL_Init(SDL_INIT_AUDIO)) {
            fprintf(stderr, "SDL_Init audio: %s\n", SDL_GetError());
            return 1;
        }
        Audio *a = audio_init();
        if (!a) { SDL_Quit(); return 1; }
        audio_synth(a, WAVE_SINE, 440.0, 1.0, 0.3, 0.01, 0.1, 0.0);
        SDL_Delay(1200);
        audio_destroy(a);
        SDL_Quit();
        return 0;
    }

    char bundle_piece[1200], bundle_lib[1200];
    const char *piece_path = NULL;
    if (argc > 1) {
        piece_path = argv[1];
    } else {
        piece_path = detect_bundle(bundle_piece, sizeof(bundle_piece),
                                   bundle_lib, sizeof(bundle_lib));
        if (!piece_path) piece_path = "../test-pieces/hello.mjs";
    }

    if (!SDL_Init(SDL_INIT_VIDEO)) {
        fprintf(stderr, "SDL_Init failed: %s\n", SDL_GetError());
        return 1;
    }

    // Windowed + resizable by default. AC_FULLSCREEN=1 opts in to fullscreen
    // for bare-metal-style presentation. Native pixel density (no HIGH_PIXEL_DENSITY):
    // pixels appear at their natural on-screen size instead of being
    // retina-multiplied into 4×4 blocks on a Mac, which reads as "extra large".
    int fullscreen = getenv("AC_FULLSCREEN") != NULL;
    Uint32 win_flags = SDL_WINDOW_RESIZABLE;
    if (fullscreen) win_flags |= SDL_WINDOW_FULLSCREEN;

    SDL_Window *win = SDL_CreateWindow("Notepat",
                                       FB_W * WIN_SCALE, FB_H * WIN_SCALE,
                                       win_flags);
    if (!win) { fprintf(stderr, "SDL_CreateWindow: %s\n", SDL_GetError()); SDL_Quit(); return 1; }

    SDL_Renderer *ren = SDL_CreateRenderer(win, NULL);
    if (!ren) { fprintf(stderr, "SDL_CreateRenderer: %s\n", SDL_GetError()); SDL_Quit(); return 1; }
    SDL_SetRenderVSync(ren, 1);
    fprintf(stderr, "[macos] renderer: %s (%s)\n", SDL_GetRendererName(ren),
            fullscreen ? "fullscreen" : "windowed");

    // Integer-scale letterbox: FB_W×FB_H is the logical canvas, the renderer
    // picks the largest integer multiple that fits and centers it with black
    // bars. Combined with SDL_SCALEMODE_NEAREST on the texture, this yields
    // pixel-perfect chunky pixels at every window size.
    SDL_SetRenderLogicalPresentation(ren, FB_W, FB_H, SDL_LOGICAL_PRESENTATION_INTEGER_SCALE);

    // Overlay feel: hide the OS cursor in fullscreen. Windowed keeps the
    // system cursor so resize/drag/close controls behave normally.
    if (fullscreen) SDL_HideCursor();

    SDL_Texture *tex = SDL_CreateTexture(ren, SDL_PIXELFORMAT_ARGB8888,
                                         SDL_TEXTUREACCESS_STREAMING, FB_W, FB_H);
    if (!tex) { fprintf(stderr, "SDL_CreateTexture: %s\n", SDL_GetError()); SDL_Quit(); return 1; }
    SDL_SetTextureScaleMode(tex, SDL_SCALEMODE_NEAREST);

    PieceFB fb = {
        .pixels = calloc((size_t)FB_W * FB_H, sizeof(uint32_t)),
        .width  = FB_W,
        .height = FB_H,
        .stride = FB_W,
    };
    if (!fb.pixels) { fprintf(stderr, "fb alloc failed\n"); return 1; }

    PieceCtx *pc = piece_load(piece_path, &fb);
    if (!pc) {
        fprintf(stderr, "[macos] failed to load piece: %s\n", piece_path);
        free(fb.pixels);
        SDL_DestroyTexture(tex); SDL_DestroyRenderer(ren); SDL_DestroyWindow(win); SDL_Quit();
        return 1;
    }
    piece_boot(pc);

    // Optional single-frame dump for headless verification. Set AC_DUMP_FRAME
    // to a path; the host renders one paint cycle, writes a raw ARGB .ppm-
    // like dump (actually BGRA-PPM with a header), and exits.
    const char *dump_path = getenv("AC_DUMP_FRAME");
    if (dump_path) {
        piece_paint(pc);
        FILE *f = fopen(dump_path, "wb");
        if (f) {
            fprintf(f, "P6\n%d %d\n255\n", fb.width, fb.height);
            for (int y = 0; y < fb.height; y++) {
                for (int x = 0; x < fb.width; x++) {
                    uint32_t p = fb.pixels[y * fb.stride + x];
                    unsigned char rgb[3] = { (unsigned char)(p >> 16), (unsigned char)(p >> 8), (unsigned char)p };
                    fwrite(rgb, 1, 3, f);
                }
            }
            fclose(f);
            fprintf(stderr, "[macos] dumped frame to %s\n", dump_path);
        }
        piece_destroy(pc);
        free(fb.pixels);
        SDL_DestroyTexture(tex); SDL_DestroyRenderer(ren); SDL_DestroyWindow(win); SDL_Quit();
        return 0;
    }

    // Optional headless auto-exit for CI/regression (AC_HEADLESS_MS=<n>).
    // Runs the normal event loop but breaks after N ms. Useful for audio tests.
    const char *headless_env = getenv("AC_HEADLESS_MS");
    int headless_ms = headless_env ? atoi(headless_env) : 0;
    // AC_INJECT_KEY=<name>: after 300 ms, synthesize a keyboard:down:<name>
    // event so notepat's sound.synth path fires in a headless run.
    const char *inject_key = getenv("AC_INJECT_KEY");
    int injected = 0;
    Uint64 start_tick = SDL_GetTicks();

    int running = 1;
    while (running) {
        if (headless_ms > 0 && (int)(SDL_GetTicks() - start_tick) >= headless_ms) {
            running = 0;
            break;
        }
        if (inject_key && !injected && (SDL_GetTicks() - start_tick) >= 300) {
            PieceEvent pe = {0};
            snprintf(pe.key,  sizeof(pe.key),  "%s", inject_key);
            snprintf(pe.type, sizeof(pe.type), "keyboard:down:%s", inject_key);
            piece_act(pc, &pe);
            fprintf(stderr, "[inject] keyboard:down:%s\n", inject_key);
            injected = 1;
        }
        SDL_Event ev;
        while (SDL_PollEvent(&ev)) {
            // Remap mouse/touch coords from window pixels into the logical
            // FB_W × FB_H canvas so pieces see native framebuffer coords
            // regardless of fullscreen scale factor or retina backing.
            SDL_ConvertEventToRenderCoordinates(ren, &ev);
            if (ev.type == SDL_EVENT_QUIT) running = 0;
            else if (ev.type == SDL_EVENT_KEY_DOWN) {
                if (ev.key.key == SDLK_ESCAPE) { running = 0; continue; }
                PieceEvent pe = {0};
                sdl_key_name(ev.key.key, pe.key, sizeof(pe.key));
                snprintf(pe.type, sizeof(pe.type), "keyboard:down:%s", pe.key);
                piece_act(pc, &pe);
            } else if (ev.type == SDL_EVENT_KEY_UP) {
                PieceEvent pe = {0};
                sdl_key_name(ev.key.key, pe.key, sizeof(pe.key));
                snprintf(pe.type, sizeof(pe.type), "keyboard:up:%s", pe.key);
                piece_act(pc, &pe);
            } else if (ev.type == SDL_EVENT_MOUSE_BUTTON_DOWN) {
                PieceEvent pe = { .x = (int)ev.button.x, .y = (int)ev.button.y };
                snprintf(pe.type, sizeof(pe.type), "touch");
                piece_act(pc, &pe);
            } else if (ev.type == SDL_EVENT_MOUSE_BUTTON_UP) {
                PieceEvent pe = { .x = (int)ev.button.x, .y = (int)ev.button.y };
                snprintf(pe.type, sizeof(pe.type), "lift");
                piece_act(pc, &pe);
            } else if (ev.type == SDL_EVENT_MOUSE_MOTION && (ev.motion.state & SDL_BUTTON_LMASK)) {
                PieceEvent pe = { .x = (int)ev.motion.x, .y = (int)ev.motion.y };
                snprintf(pe.type, sizeof(pe.type), "draw");
                piece_act(pc, &pe);
            }
        }

        piece_sim(pc);
        piece_paint(pc);

        SDL_UpdateTexture(tex, NULL, fb.pixels, fb.stride * (int)sizeof(uint32_t));
        SDL_RenderClear(ren);
        SDL_RenderTexture(ren, tex, NULL, NULL);
        SDL_RenderPresent(ren);
    }

    piece_destroy(pc);
    free(fb.pixels);
    SDL_DestroyTexture(tex);
    SDL_DestroyRenderer(ren);
    SDL_DestroyWindow(win);
    SDL_Quit();
    return 0;
}
