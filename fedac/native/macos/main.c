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
#include <Carbon/Carbon.h>

#include "piece.h"
#include "audio.h"

// Initial window size in logical points — what you'd expect for a
// non-retina display. The actual framebuffer is computed from the window's
// pixel size divided by DENSITY so it reflows on resize without letterbox.
#define INITIAL_WIN_W 1280
#define INITIAL_WIN_H 800
#define DEFAULT_DENSITY 1  // logical points per framebuffer pixel — 1 = web AC parity (FB matches window points, notepat's top bar has room); higher = chunkier

// Shared state the event watch callback needs. SDL calls the watch on the
// same thread as SDL_PollEvent, during the OS resize modal run loop, so
// synchronous access without a lock is safe.
typedef struct {
    SDL_Window   *win;
    SDL_Renderer *ren;
    SDL_Texture **tex;    // by-ref so we can recreate
    PieceFB      *fb;
    PieceCtx     *pc;
    int           density;
} RenderCtx;

// Reallocate FB + texture + logical presentation when the window size changes.
// Returns true if dimensions actually changed.
static int maybe_reframe(RenderCtx *c) {
    int nwp = 0, nhp = 0;
    SDL_GetWindowSize(c->win, &nwp, &nhp);
    int nw = nwp / c->density; if (nw < 64) nw = 64;
    int nh = nhp / c->density; if (nh < 64) nh = 64;
    if (nw == c->fb->width && nh == c->fb->height) return 0;
    uint32_t *np = calloc((size_t)nw * nh, sizeof(uint32_t));
    if (!np) return 0;
    free(c->fb->pixels);
    c->fb->pixels = np;
    c->fb->width  = nw;
    c->fb->height = nh;
    c->fb->stride = nw;
    SDL_DestroyTexture(*c->tex);
    *c->tex = SDL_CreateTexture(c->ren, SDL_PIXELFORMAT_ARGB8888,
                                SDL_TEXTUREACCESS_STREAMING, nw, nh);
    SDL_SetTextureScaleMode(*c->tex, SDL_SCALEMODE_NEAREST);
    SDL_SetRenderLogicalPresentation(c->ren, nw, nh,
                                     SDL_LOGICAL_PRESENTATION_STRETCH);
    piece_reframe(c->pc, nw, nh);
    return 1;
}

// Single frame: ask the piece to paint, upload, present.
static void render_frame(RenderCtx *c) {
    piece_paint(c->pc);
    SDL_UpdateTexture(*c->tex, NULL, c->fb->pixels,
                      c->fb->stride * (int)sizeof(uint32_t));
    SDL_RenderClear(c->ren);
    SDL_RenderTexture(c->ren, *c->tex, NULL, NULL);
    SDL_RenderPresent(c->ren);
}

// SDL_AddEventWatch callback: fires synchronously from inside the OS resize
// modal run loop on macOS. Without this, the main event loop is frozen
// during drag and the renderer keeps stretching the stale FB to the new
// window size. Handling resize + paint + present here keeps pixels honest.
static bool SDLCALL resize_watch(void *userdata, SDL_Event *ev) {
    if (ev->type == SDL_EVENT_WINDOW_RESIZED ||
        ev->type == SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED ||
        ev->type == SDL_EVENT_WINDOW_EXPOSED) {
        RenderCtx *c = (RenderCtx *)userdata;
        maybe_reframe(c);
        render_frame(c);
    }
    return true;
}

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

// ── Overlay mode helpers ────────────────────────────────────────────────────

// Cmd-drag moves the borderless overlay window. Hit test runs every mouse
// move to decide whether the click belongs to the app or the window manager.
static SDL_HitTestResult SDLCALL hit_test_cmd_drag(SDL_Window *win,
                                                   const SDL_Point *pt,
                                                   void *data) {
    (void)win; (void)pt; (void)data;
    SDL_Keymod mod = SDL_GetModState();
    if (mod & SDL_KMOD_GUI) return SDL_HITTEST_DRAGGABLE;
    return SDL_HITTEST_NORMAL;
}

// Build a small RGBA surface for the tray icon. 22×22 fits the macOS menu
// bar comfortably. Pattern mirrors the app icon — yellow "N" on teal.
static SDL_Surface *make_tray_icon_surface(void) {
    const int W = 22, H = 22;
    SDL_Surface *s = SDL_CreateSurface(W, H, SDL_PIXELFORMAT_RGBA32);
    if (!s) return NULL;
    uint32_t *px = (uint32_t *)s->pixels;
    const uint32_t BG = (255u << 24) | (110u << 16) | (90u << 8) | 45u;   // R G B A... careful
    // RGBA32 ordering depends on platform — use SDL's packer.
    SDL_PixelFormat fmt = s->format;
    const SDL_PixelFormatDetails *d = SDL_GetPixelFormatDetails(fmt);
    uint32_t bg = SDL_MapRGBA(d, NULL, 45, 90, 110, 255);
    uint32_t fg = SDL_MapRGBA(d, NULL, 255, 210, 90, 255);
    uint32_t tp = SDL_MapRGBA(d, NULL, 0, 0, 0, 0);
    (void)BG;
    for (int y = 0; y < H; y++) {
        for (int x = 0; x < W; x++) {
            int idx = y * W + x;
            // Rounded corner mask
            int in_corner = 0;
            int cr = 4;
            if ((x < cr && y < cr)) { int dx = cr-x-1, dy = cr-y-1; if (dx*dx+dy*dy > cr*cr) in_corner = 1; }
            if ((x >= W-cr && y < cr)) { int dx = x-(W-cr), dy = cr-y-1; if (dx*dx+dy*dy >= cr*cr) in_corner = 1; }
            if ((x < cr && y >= H-cr)) { int dx = cr-x-1, dy = y-(H-cr); if (dx*dx+dy*dy >= cr*cr) in_corner = 1; }
            if ((x >= W-cr && y >= H-cr)) { int dx = x-(W-cr), dy = y-(H-cr); if (dx*dx+dy*dy >= cr*cr) in_corner = 1; }
            if (in_corner) { px[idx] = tp; continue; }
            // Simple "N" — bars at cols 4 and 15, diagonal between.
            int is_n = 0;
            if ((x >= 4 && x <= 6 && y >= 4 && y <= 17) ||
                (x >= 15 && x <= 17 && y >= 4 && y <= 17)) is_n = 1;
            // diagonal: from (4,4)→(17,17) with width 2
            if (!is_n && y >= 4 && y <= 17) {
                int target = 4 + (y - 4) * 13 / 13;
                if (x >= target + 2 && x <= target + 4) is_n = 1;
            }
            px[idx] = is_n ? fg : bg;
        }
    }
    return s;
}

// Flags the tray / hotkey callbacks write; the main loop polls them.
static volatile int g_toggle_visible = 0;
static volatile int g_quit_requested = 0;

static void SDLCALL tray_show_hide(void *ud, SDL_TrayEntry *entry) {
    (void)ud; (void)entry;
    g_toggle_visible = 1;
}
static void SDLCALL tray_quit(void *ud, SDL_TrayEntry *entry) {
    (void)ud; (void)entry;
    g_quit_requested = 1;
}

static OSStatus hotkey_cb(EventHandlerCallRef href, EventRef ev, void *ud) {
    (void)href; (void)ev; (void)ud;
    g_toggle_visible = 1;
    return noErr;
}

// Register a system-wide hotkey via Carbon. Doesn't need accessibility
// permission for this path — the app just has to be a foreground bundle.
// keyCode is the Carbon virtual key; modifiers are the Carbon flags.
static EventHotKeyRef g_hotkey = NULL;
static EventHandlerRef g_hotkey_handler = NULL;
static int install_global_hotkey(void) {
    EventTypeSpec spec = { kEventClassKeyboard, kEventHotKeyPressed };
    InstallEventHandler(GetEventDispatcherTarget(), (EventHandlerUPP)hotkey_cb,
                        1, &spec, NULL, &g_hotkey_handler);
    EventHotKeyID id = { .signature = 'ntpt', .id = 1 };
    // kVK_ANSI_N = 0x2D. Modifiers: cmdKey | optionKey | controlKey.
    OSStatus s = RegisterEventHotKey(0x2D,
                                     cmdKey | optionKey | controlKey,
                                     id, GetEventDispatcherTarget(), 0, &g_hotkey);
    return s == noErr ? 0 : -1;
}
static void uninstall_global_hotkey(void) {
    if (g_hotkey) UnregisterEventHotKey(g_hotkey);
    if (g_hotkey_handler) RemoveEventHandler(g_hotkey_handler);
    g_hotkey = NULL; g_hotkey_handler = NULL;
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

    // Windowed + resizable by default. AC_FULLSCREEN=1 opts in to fullscreen.
    // HIGH_PIXEL_DENSITY is required on retina: without it the renderer works
    // at logical-point resolution and macOS bilinear-upscales to the physical
    // backing store, which defeats our nearest-neighbor texture filter and
    // reads as blurry. With it on, nearest-neighbor stays nearest-neighbor
    // all the way through to the pixel.
    int fullscreen = getenv("AC_FULLSCREEN") != NULL;
    int overlay    = getenv("AC_OVERLAY")    != NULL;
    Uint32 win_flags = SDL_WINDOW_RESIZABLE | SDL_WINDOW_HIGH_PIXEL_DENSITY;
    if (fullscreen) win_flags |= SDL_WINDOW_FULLSCREEN;
    if (overlay) {
        // Borderless + transparent so the piece can draw on the desktop;
        // always-on-top pins it above other windows (HUD feel). Cmd+drag
        // to reposition — the hit test below routes those clicks to the
        // window server rather than the piece.
        win_flags |= SDL_WINDOW_TRANSPARENT
                   | SDL_WINDOW_BORDERLESS
                   | SDL_WINDOW_ALWAYS_ON_TOP;
    }

    SDL_Window *win = SDL_CreateWindow("Notepat",
                                       INITIAL_WIN_W, INITIAL_WIN_H,
                                       win_flags);
    if (!win) { fprintf(stderr, "SDL_CreateWindow: %s\n", SDL_GetError()); SDL_Quit(); return 1; }
    if (overlay) SDL_SetWindowHitTest(win, hit_test_cmd_drag, NULL);

    SDL_Renderer *ren = SDL_CreateRenderer(win, NULL);
    if (!ren) { fprintf(stderr, "SDL_CreateRenderer: %s\n", SDL_GetError()); SDL_Quit(); return 1; }
    // VSync on by default (smooth paint). AC_LATENCY_TEST disables it so the
    // event loop polls tight and latency measurements aren't frame-aligned.
    int latency_runs = getenv("AC_LATENCY_TEST") ? atoi(getenv("AC_LATENCY_TEST")) : 0;
    if (latency_runs < 0) latency_runs = 0;
    SDL_SetRenderVSync(ren, latency_runs > 0 ? 0 : 1);
    fprintf(stderr, "[macos] renderer: %s (%s)\n", SDL_GetRendererName(ren),
            fullscreen ? "fullscreen" : "windowed");

    // Pixel density: AC_DENSITY overrides. Higher = smaller framebuffer
    // (chunkier pixels), lower = more framebuffer resolution.
    const char *density_env = getenv("AC_DENSITY");
    int density = density_env ? atoi(density_env) : DEFAULT_DENSITY;
    if (density < 1) density = 1;
    if (density > 8) density = 8;

    if (fullscreen) SDL_HideCursor();

    // Compute initial FB size from the window's *logical* (point) size, not
    // its pixel size. This matches web AC's CSS-pixel model: density is
    // "points per FB pixel", so a 1280×800 logical window at density 2 gives
    // a 640×400 FB regardless of retina scale. Retina sharpness still comes
    // from HIGH_PIXEL_DENSITY + nearest-neighbor presentation.
    int win_w = INITIAL_WIN_W, win_h = INITIAL_WIN_H;
    SDL_GetWindowSize(win, &win_w, &win_h);
    int fb_w = win_w / density; if (fb_w < 64) fb_w = 64;
    int fb_h = win_h / density; if (fb_h < 64) fb_h = 64;
    fprintf(stderr, "[macos] initial fb %dx%d (window %dx%d points, density %d)\n",
            fb_w, fb_h, win_w, win_h, density);

    // STRETCH presentation fills the window with no letterbox. Nearest-
    // neighbor texture filtering + integer density means pixels stay crisp.
    SDL_SetRenderLogicalPresentation(ren, fb_w, fb_h, SDL_LOGICAL_PRESENTATION_STRETCH);

    SDL_Texture *tex = SDL_CreateTexture(ren, SDL_PIXELFORMAT_ARGB8888,
                                         SDL_TEXTUREACCESS_STREAMING, fb_w, fb_h);
    if (!tex) { fprintf(stderr, "SDL_CreateTexture: %s\n", SDL_GetError()); SDL_Quit(); return 1; }
    SDL_SetTextureScaleMode(tex, SDL_SCALEMODE_NEAREST);
    if (overlay) {
        // Texture alpha blends over the transparent window; renderer clear
        // must use alpha=0 so areas the piece wiped stay see-through.
        SDL_SetTextureBlendMode(tex, SDL_BLENDMODE_BLEND);
        SDL_SetRenderDrawBlendMode(ren, SDL_BLENDMODE_BLEND);
        SDL_SetRenderDrawColor(ren, 0, 0, 0, 0);
    }

    PieceFB fb = {
        .pixels = calloc((size_t)fb_w * fb_h, sizeof(uint32_t)),
        .width  = fb_w,
        .height = fb_h,
        .stride = fb_w,
    };
    if (!fb.pixels) { fprintf(stderr, "fb alloc failed\n"); return 1; }

    PieceCtx *pc = piece_load(piece_path, &fb);
    if (!pc) {
        fprintf(stderr, "[macos] failed to load piece: %s\n", piece_path);
        free(fb.pixels);
        SDL_DestroyTexture(tex); SDL_DestroyRenderer(ren); SDL_DestroyWindow(win); SDL_Quit();
        return 1;
    }
    // Make sure the piece sees the real initial dimensions (may differ from
    // piece_load defaults if the window grew during creation on some WMs).
    piece_reframe(pc, fb.width, fb.height);
    if (overlay) piece_set_overlay(pc, 1);
    piece_boot(pc);

    // Menu-bar tray: Show/Hide and Quit entries. Callbacks flip flags the
    // main loop polls. Tray creation may fail on headless runs; we ignore
    // the error since it's not essential.
    SDL_Tray *tray = NULL;
    SDL_Surface *tray_icon = make_tray_icon_surface();
    if (tray_icon) {
        tray = SDL_CreateTray(tray_icon, "Notepat");
        SDL_DestroySurface(tray_icon);
        if (tray) {
            SDL_TrayMenu *menu = SDL_CreateTrayMenu(tray);
            SDL_TrayEntry *e_show = SDL_InsertTrayEntryAt(menu, -1, "Show / Hide Notepat", SDL_TRAYENTRY_BUTTON);
            SDL_InsertTrayEntryAt(menu, -1, NULL, SDL_TRAYENTRY_BUTTON);  // separator
            SDL_TrayEntry *e_quit = SDL_InsertTrayEntryAt(menu, -1, "Quit", SDL_TRAYENTRY_BUTTON);
            SDL_SetTrayEntryCallback(e_show, tray_show_hide, NULL);
            SDL_SetTrayEntryCallback(e_quit, tray_quit, NULL);
        }
    }

    // Global hotkey: Ctrl+Alt+Cmd+N toggles window visibility system-wide.
    if (install_global_hotkey() == 0) {
        fprintf(stderr, "[hotkey] Ctrl+Alt+Cmd+N registered\n");
    } else {
        fprintf(stderr, "[hotkey] register failed (another app may own the combo)\n");
    }

    RenderCtx rctx = { .win = win, .ren = ren, .tex = &tex, .fb = &fb,
                       .pc = pc, .density = density };
    SDL_AddEventWatch(resize_watch, &rctx);

    // Latency benchmark: inject `latency_runs` keypresses, measure each one's
    // trigger→first-audio-sample delta, print min/median/max. Vsync is off
    // so polling is tight; audio buffer size is what you set via AC_AUDIO_BUFFER.
    if (latency_runs > 0) {
        const char *lkey = getenv("AC_INJECT_KEY");
        if (!lkey) lkey = "c";
        double lats[256];
        int got = 0;
        // Let the audio device warm up + piece settle.
        Uint64 warm = SDL_GetTicks();
        while (SDL_GetTicks() - warm < 500) {
            SDL_Event e; while (SDL_PollEvent(&e)) {}
            piece_sim(pc); render_frame(&rctx);
            SDL_Delay(5);
        }
        Audio *au = piece_audio(pc);
        for (int i = 0; i < latency_runs && i < 256; i++) {
            // Drain events so the injection isn't behind queued ones.
            SDL_Event e; while (SDL_PollEvent(&e)) {}
            // Arm immediately before synthesizing the press. piece_act runs
            // the piece's handler synchronously, which enqueues the voice.
            PieceEvent pe = {0};
            snprintf(pe.key,  sizeof(pe.key),  "%s", lkey);
            snprintf(pe.type, sizeof(pe.type), "keyboard:down:%s", lkey);
            if (au) audio_arm_latency(au, 0.005f);
            piece_act(pc, &pe);
            // Busy-poll for the emit stamp, 50 ms cap.
            Uint64 until = SDL_GetTicksNS() + 50000000ULL;
            while (SDL_GetTicksNS() < until) {
                if (au && audio_latency_ns(au)) break;
            }
            uint64_t ns = au ? audio_latency_ns(au) : 0;
            if (ns) { lats[got++] = (double)ns / 1.0e6; }
            // Release + settle before next run so the voice finishes.
            PieceEvent peup = {0};
            snprintf(peup.key, sizeof(peup.key), "%s", lkey);
            snprintf(peup.type, sizeof(peup.type), "keyboard:up:%s", lkey);
            piece_act(pc, &peup);
            SDL_Delay(120);
        }
        if (got > 0) {
            // Insertion sort — tiny N.
            for (int i = 1; i < got; i++) {
                double v = lats[i]; int j = i;
                while (j > 0 && lats[j-1] > v) { lats[j] = lats[j-1]; j--; }
                lats[j] = v;
            }
            double sum = 0; for (int i = 0; i < got; i++) sum += lats[i];
            fprintf(stderr, "[latency] %d runs, key=\"%s\": "
                    "min=%.2f median=%.2f mean=%.2f max=%.2f ms\n",
                    got, lkey, lats[0], lats[got/2], sum / got, lats[got-1]);
            // Dump full list for analysis.
            fprintf(stderr, "[latency] samples:");
            for (int i = 0; i < got; i++) fprintf(stderr, " %.2f", lats[i]);
            fprintf(stderr, "\n");
        } else {
            fprintf(stderr, "[latency] no emissions recorded\n");
        }
        SDL_RemoveEventWatch(resize_watch, &rctx);
        piece_destroy(pc);
        free(fb.pixels);
        SDL_DestroyTexture(tex);
        SDL_DestroyRenderer(ren);
        SDL_DestroyWindow(win);
        SDL_Quit();
        return 0;
    }

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
    int hidden = 0;
    while (running) {
        if (g_quit_requested) { running = 0; break; }
        if (g_toggle_visible) {
            g_toggle_visible = 0;
            hidden = !hidden;
            if (hidden) SDL_HideWindow(win);
            else { SDL_ShowWindow(win); SDL_RaiseWindow(win); }
        }
        if (headless_ms > 0 && (int)(SDL_GetTicks() - start_tick) >= headless_ms) {
            running = 0;
            break;
        }
        if (inject_key && !injected && (SDL_GetTicks() - start_tick) >= 300) {
            PieceEvent pe = {0};
            snprintf(pe.key,  sizeof(pe.key),  "%s", inject_key);
            snprintf(pe.type, sizeof(pe.type), "keyboard:down:%s", inject_key);
            // Arm the latency stopwatch immediately before dispatching so the
            // captured trigger time is as close to "user hits key" as we can
            // synthesize. Audio callback stamps first non-silent emission.
            Audio *au = piece_audio(pc);
            // Low threshold catches the onset of the attack ramp rather than
            // waiting for full-level sustain — a more honest keypress→sound
            // measurement (matches what an ear would perceive as "the note").
            if (au) audio_arm_latency(au, 0.005f);
            piece_act(pc, &pe);
            fprintf(stderr, "[inject] keyboard:down:%s\n", inject_key);
            injected = 1;
        }
        // Report latency as soon as the callback stamps first emission.
        if (injected && inject_key) {
            Audio *au = piece_audio(pc);
            uint64_t lat_ns = au ? audio_latency_ns(au) : 0;
            if (lat_ns) {
                fprintf(stderr, "[latency] key \"%s\" -> first audio sample: %.3f ms\n",
                        inject_key, (double)lat_ns / 1.0e6);
                inject_key = NULL;  // only report once
            }
        }
        SDL_Event ev;
        while (SDL_PollEvent(&ev)) {
            // Remap mouse/touch coords from window pixels into the logical
            // FB_W × FB_H canvas so pieces see native framebuffer coords
            // regardless of fullscreen scale factor or retina backing.
            SDL_ConvertEventToRenderCoordinates(ren, &ev);
            if (ev.type == SDL_EVENT_QUIT) running = 0;
            else if (ev.type == SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED ||
                     ev.type == SDL_EVENT_WINDOW_RESIZED) {
                // The watch callback already handled this during the drag;
                // this catches the final tick + any resize outside a drag.
                maybe_reframe(&rctx);
                continue;
            }
            else if (ev.type == SDL_EVENT_KEY_DOWN) {
                if (ev.key.key == SDLK_ESCAPE) { running = 0; continue; }
                // Cmd+= / Cmd+- live-adjust pixel density (zoom in/out).
                // Cmd+0 resets to 1 (web-AC parity). Absorbed — piece never
                // sees these keypresses.
                if (ev.key.mod & SDL_KMOD_GUI) {
                    int ch = 0;
                    if (ev.key.key == SDLK_EQUALS || ev.key.key == SDLK_PLUS) ch = +1;
                    else if (ev.key.key == SDLK_MINUS)                       ch = -1;
                    else if (ev.key.key == SDLK_0)                           ch = 100;  // reset
                    if (ch) {
                        int new_d = (ch == 100) ? 1 : rctx.density + ch;
                        if (new_d < 1) new_d = 1;
                        if (new_d > 8) new_d = 8;
                        if (new_d != rctx.density) {
                            rctx.density = new_d;
                            fprintf(stderr, "[density] %d\n", rctx.density);
                            maybe_reframe(&rctx);
                            render_frame(&rctx);
                        }
                        continue;
                    }
                }
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
        render_frame(&rctx);
    }

    SDL_RemoveEventWatch(resize_watch, &rctx);
    uninstall_global_hotkey();
    if (tray) SDL_DestroyTray(tray);

    piece_destroy(pc);
    free(fb.pixels);
    SDL_DestroyTexture(tex);
    SDL_DestroyRenderer(ren);
    SDL_DestroyWindow(win);
    SDL_Quit();
    return 0;
}
