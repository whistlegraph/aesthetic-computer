// menuband.c — Menu Band for Windows 11.
//
// The Mac app's menu-bar piano, rebuilt for the Windows taskbar. Windows 11
// has no deskband API any more, so the strip is a borderless, topmost,
// non-activating layered window that parks itself just left of the system
// tray and follows the taskbar around. Clicking a key never steals focus
// from whatever you were doing — same feel as the status item on the Mac.
//
// Sound is the same GM synthesis core the Mac app and AC OS run
// (gm_synth.c, shared verbatim), rendered over WASAPI in shared mode with
// the outer attack/release contour, polyphony and voice stealing copied
// from MenuBandGMSynth.swift.
//
// Build: build.ps1 (cl.exe from the VS 2022 Build Tools). No dependencies
// beyond the Windows SDK.

#define UNICODE
#define _UNICODE
#define WIN32_LEAN_AND_MEAN
#define _USE_MATH_DEFINES
#include <windows.h>
#include <windowsx.h>
#include <shellapi.h>
#include <mmdeviceapi.h>
#include <audioclient.h>
#include <ksmedia.h>
#include <avrt.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include "gm_synth.h"

// The MIDL-generated audio headers only declare these; in C nothing defines
// them, so spell out the well-known values here.
static const CLSID MB_CLSID_MMDeviceEnumerator = {0xBCDE0395,0xE52F,0x467C,{0x8E,0x3D,0xC4,0x57,0x92,0x91,0x69,0x2E}};
static const IID   MB_IID_IMMDeviceEnumerator  = {0xA95664D2,0x9614,0x4F35,{0xA7,0x46,0xDE,0x8D,0xB6,0x36,0x17,0xE6}};
static const IID   MB_IID_IAudioClient         = {0x1CB9AD4C,0xDBFA,0x4C32,{0xB1,0x78,0xC2,0xF5,0x68,0xA7,0x03,0xB2}};
static const IID   MB_IID_IAudioRenderClient   = {0xF294ACFC,0x3146,0x4483,{0xA7,0xBF,0xAD,0xDC,0xA7,0xC2,0x60,0xE2}};
static const GUID  MB_SUBTYPE_IEEE_FLOAT       = {0x00000003,0x0000,0x0010,{0x80,0x00,0x00,0xAA,0x00,0x38,0x9B,0x71}};

// ───────────────────────────── synth ─────────────────────────────

#define MAX_VOICES 24
#define PENDING_MAX 256

typedef struct {
    uint8_t midi;
    double freq;
    double velocityGain;
    double attack, release;   // seconds
    double env;
    int releasing;
    int active;
} Voice;

typedef struct { int kind; uint8_t midi; uint8_t velocity; int program; uint32_t seed; } Cmd;
enum { CMD_ON = 1, CMD_OFF = 2, CMD_PANIC = 3 };

static GMVoice g_cores[MAX_VOICES];       // ~80 KB each: keep them static, never on a stack
static Voice g_voices[MAX_VOICES];
static Cmd g_pending[PENDING_MAX];
static int g_pendingCount = 0;
static CRITICAL_SECTION g_lock;
static uint32_t g_seedCounter = 0x9E3779B9u;
static double g_sampleRate = 48000.0;
static const double g_masterGain = 0.5;
static volatile int g_program = 0;        // GM program for new notes (Acoustic Grand)
static volatile LONG g_audioRunning = 0;

static double midi_freq(int midi) { return 440.0 * pow(2.0, (midi - 69) / 12.0); }

static void synth_note_on(int midi, int velocity) {
    if (!gm_program_implemented(g_program)) return;
    EnterCriticalSection(&g_lock);
    if (g_pendingCount < PENDING_MAX) {
        g_seedCounter = g_seedCounter * 1664525u + 1013904223u;
        Cmd c = { CMD_ON, (uint8_t)midi, (uint8_t)velocity, g_program, g_seedCounter };
        g_pending[g_pendingCount++] = c;
    }
    LeaveCriticalSection(&g_lock);
}

static void synth_note_off(int midi) {
    EnterCriticalSection(&g_lock);
    if (g_pendingCount < PENDING_MAX) {
        Cmd c = { CMD_OFF, (uint8_t)midi, 0, 0, 0 };
        g_pending[g_pendingCount++] = c;
    }
    LeaveCriticalSection(&g_lock);
}

static int allocate_voice(void) {
    for (int i = 0; i < MAX_VOICES; i++) if (!g_voices[i].active) return i;
    int best = 0; double bestScore = 1e300;
    for (int i = 0; i < MAX_VOICES; i++) {
        double score = (g_voices[i].releasing ? 0 : 1000) + g_voices[i].env;
        if (score < bestScore) { bestScore = score; best = i; }
    }
    return best;
}

// Render `frames` of interleaved stereo float32 into `out`.
static void synth_render(float *out, int frames) {
    memset(out, 0, sizeof(float) * 2 * frames);

    Cmd cmds[PENDING_MAX]; int n;
    EnterCriticalSection(&g_lock);
    n = g_pendingCount;
    if (n) memcpy(cmds, g_pending, sizeof(Cmd) * n);
    g_pendingCount = 0;
    LeaveCriticalSection(&g_lock);

    for (int k = 0; k < n; k++) {
        Cmd *c = &cmds[k];
        if (c->kind == CMD_ON) {
            int slot = allocate_voice();
            double f = midi_freq(c->midi);
            if (gm_voice_init(&g_cores[slot], c->program, f, g_sampleRate, c->seed) == 0) {
                Voice *v = &g_voices[slot];
                v->midi = c->midi; v->freq = f;
                v->velocityGain = fmax(0.05, fmin(1.0, c->velocity / 127.0));
                v->attack = 0.004; v->release = 0.18;
                v->env = 0; v->releasing = 0; v->active = 1;
            } else {
                g_voices[slot].active = 0;
            }
        } else if (c->kind == CMD_OFF) {
            for (int i = 0; i < MAX_VOICES; i++)
                if (g_voices[i].active && !g_voices[i].releasing && g_voices[i].midi == c->midi)
                    g_voices[i].releasing = 1;
        } else if (c->kind == CMD_PANIC) {
            for (int i = 0; i < MAX_VOICES; i++) g_voices[i].active = 0;
        }
    }

    const double dt = 1.0 / g_sampleRate;
    for (int idx = 0; idx < MAX_VOICES; idx++) {
        Voice *v = &g_voices[idx];
        if (!v->active) continue;
        GMVoice *core = &g_cores[idx];
        double attackInc = v->attack > 0 ? dt / v->attack : 1.0;
        double releaseDec = v->release > 0 ? dt / v->release : 1.0;
        double env = v->env; int active = 1;
        for (int i = 0; i < frames; i++) {
            if (v->releasing) {
                env -= releaseDec;
                if (env <= 0) { env = 0; active = 0; break; }
            } else if (env < 1.0) {
                env += attackInc; if (env > 1.0) env = 1.0;
            }
            double s = gm_voice_render(core, g_sampleRate, env, v->freq);
            if (!isfinite(s)) { active = 0; break; }
            float amp = (float)(s * v->velocityGain * g_masterGain * 0.707);
            out[2 * i] += amp; out[2 * i + 1] += amp;
        }
        v->env = env; v->active = active;
    }
}

// ───────────────────────────── WASAPI ────────────────────────────

static IAudioClient *g_client = NULL;
static IAudioRenderClient *g_render = NULL;
static HANDLE g_audioEvent = NULL;
static UINT32 g_bufferFrames = 0;

static DWORD WINAPI audio_thread(LPVOID arg) {
    (void)arg;
    DWORD taskIndex = 0;
    HANDLE mmcss = AvSetMmThreadCharacteristicsW(L"Pro Audio", &taskIndex);
    while (g_audioRunning) {
        if (WaitForSingleObject(g_audioEvent, 2000) != WAIT_OBJECT_0) continue;
        UINT32 padding = 0;
        if (FAILED(g_client->lpVtbl->GetCurrentPadding(g_client, &padding))) continue;
        UINT32 frames = g_bufferFrames - padding;
        if (frames == 0) continue;
        BYTE *data = NULL;
        if (FAILED(g_render->lpVtbl->GetBuffer(g_render, frames, &data))) continue;
        synth_render((float *)data, (int)frames);
        g_render->lpVtbl->ReleaseBuffer(g_render, frames, 0);
    }
    if (mmcss) AvRevertMmThreadCharacteristics(mmcss);
    return 0;
}

static int audio_start(void) {
    IMMDeviceEnumerator *enumerator = NULL;
    IMMDevice *device = NULL;
    HRESULT hr = CoCreateInstance(&MB_CLSID_MMDeviceEnumerator, NULL, CLSCTX_ALL,
                                  &MB_IID_IMMDeviceEnumerator, (void **)&enumerator);
    if (FAILED(hr)) return 0;
    hr = enumerator->lpVtbl->GetDefaultAudioEndpoint(enumerator, eRender, eConsole, &device);
    enumerator->lpVtbl->Release(enumerator);
    if (FAILED(hr)) return 0;
    hr = device->lpVtbl->Activate(device, &MB_IID_IAudioClient, CLSCTX_ALL, NULL, (void **)&g_client);
    device->lpVtbl->Release(device);
    if (FAILED(hr)) return 0;

    WAVEFORMATEX *mix = NULL;
    g_client->lpVtbl->GetMixFormat(g_client, &mix);
    g_sampleRate = mix ? mix->nSamplesPerSec : 48000.0;
    if (mix) CoTaskMemFree(mix);

    // Ask for float32 stereo at the mix rate; the engine converts if the
    // endpoint wants something else (AUTOCONVERTPCM).
    WAVEFORMATEXTENSIBLE fmt; memset(&fmt, 0, sizeof fmt);
    fmt.Format.wFormatTag = WAVE_FORMAT_EXTENSIBLE;
    fmt.Format.nChannels = 2;
    fmt.Format.nSamplesPerSec = (DWORD)g_sampleRate;
    fmt.Format.wBitsPerSample = 32;
    fmt.Format.nBlockAlign = 8;
    fmt.Format.nAvgBytesPerSec = fmt.Format.nSamplesPerSec * 8;
    fmt.Format.cbSize = sizeof(WAVEFORMATEXTENSIBLE) - sizeof(WAVEFORMATEX);
    fmt.Samples.wValidBitsPerSample = 32;
    fmt.dwChannelMask = SPEAKER_FRONT_LEFT | SPEAKER_FRONT_RIGHT;
    fmt.SubFormat = MB_SUBTYPE_IEEE_FLOAT;

    REFERENCE_TIME dur = 200000; // 20 ms
    hr = g_client->lpVtbl->Initialize(g_client, AUDCLNT_SHAREMODE_SHARED,
            AUDCLNT_STREAMFLAGS_EVENTCALLBACK | AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM | AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY,
            dur, 0, (WAVEFORMATEX *)&fmt, NULL);
    if (FAILED(hr)) return 0;
    g_client->lpVtbl->GetBufferSize(g_client, &g_bufferFrames);
    g_audioEvent = CreateEventW(NULL, FALSE, FALSE, NULL);
    g_client->lpVtbl->SetEventHandle(g_client, g_audioEvent);
    hr = g_client->lpVtbl->GetService(g_client, &MB_IID_IAudioRenderClient, (void **)&g_render);
    if (FAILED(hr)) return 0;

    // Prime with silence so the first callback has a clean start.
    BYTE *data = NULL;
    if (SUCCEEDED(g_render->lpVtbl->GetBuffer(g_render, g_bufferFrames, &data))) {
        memset(data, 0, g_bufferFrames * 8);
        g_render->lpVtbl->ReleaseBuffer(g_render, g_bufferFrames, 0);
    }
    gm_synth_init();
    InterlockedExchange(&g_audioRunning, 1);
    CreateThread(NULL, 0, audio_thread, NULL, 0, NULL);
    g_client->lpVtbl->Start(g_client);
    return 1;
}

// ───────────────────────────── strip ─────────────────────────────

#define WHITE_COUNT 14              // C4..B5, two octaves, like the Mac default
static int g_firstMidi = 60;        // C4
static int g_whiteMidi[WHITE_COUNT];
static int g_blackMidi[10];
static int g_blackCount = 0;

static int is_white(int midi) { int pc = ((midi % 12) + 12) % 12; return pc == 0 || pc == 2 || pc == 4 || pc == 5 || pc == 7 || pc == 9 || pc == 11; }

static void build_keys(void) {
    int w = 0; g_blackCount = 0;
    for (int m = g_firstMidi; w < WHITE_COUNT; m++) {
        if (is_white(m)) g_whiteMidi[w++] = m;
        else g_blackMidi[g_blackCount++] = m;
    }
}

// Pixel geometry (physical pixels; the window is per-monitor DPI aware).
static int g_stripW = 0, g_stripH = 0;  // whole window
static int g_keyW = 0, g_keyH = 0;      // white key
static int g_blackW = 0, g_blackH = 0;
static int g_padX = 0, g_padY = 0;

static void layout_for_taskbar_height(int taskbarH) {
    // Keys stand about as tall as the taskbar's app icons, centred.
    g_keyH = (taskbarH * 62) / 100; if (g_keyH < 16) g_keyH = 16;
    g_padY = (taskbarH - g_keyH) / 2;
    g_keyW = g_keyH / 2; if (g_keyW < 9) g_keyW = 9; if (g_keyW > 22) g_keyW = 22;
    g_blackW = (g_keyW * 6) / 10; if (g_blackW < 5) g_blackW = 5;
    g_blackH = (g_keyH * 6) / 10;
    g_padX = 6;
    g_stripW = g_padX * 2 + g_keyW * WHITE_COUNT;
    g_stripH = taskbarH;
}

static int white_index_of(int midi) { for (int i = 0; i < WHITE_COUNT; i++) if (g_whiteMidi[i] == midi) return i; return -1; }

static RECT white_rect(int i) {
    RECT r = { g_padX + i * g_keyW, g_padY, g_padX + (i + 1) * g_keyW - 1, g_padY + g_keyH };
    return r;
}

static RECT black_rect(int midi) {
    // Sits astride the groove after the white key below it.
    int below = white_index_of(midi - 1);
    int cx = g_padX + (below + 1) * g_keyW;
    RECT r = { cx - g_blackW / 2, g_padY, cx - g_blackW / 2 + g_blackW, g_padY + g_blackH };
    return r;
}

static int key_at(int x, int y) {
    POINT p = { x, y };
    for (int i = 0; i < g_blackCount; i++) { RECT r = black_rect(g_blackMidi[i]); if (PtInRect(&r, p)) return g_blackMidi[i]; }
    for (int i = 0; i < WHITE_COUNT; i++) { RECT r = white_rect(i); if (PtInRect(&r, p)) return g_whiteMidi[i]; }
    return -1;
}

// ─────────────────────────── painting ────────────────────────────
// Software rendering into a premultiplied BGRA buffer, pushed with
// UpdateLayeredWindow so the strip can have transparent padding and
// rounded ends over whatever the taskbar is doing behind it.

static uint32_t *g_pixels = NULL;
static HBITMAP g_dib = NULL;
static HDC g_memDC = NULL;

typedef struct { uint8_t r, g, b; } RGB8;

static const RGB8 CHROMA[12] = {
    {255, 50, 50}, {0,0,0}, {255,160,0}, {0,0,0}, {255,230,0}, {50,200,50},
    {0,0,0}, {50,120,255}, {0,0,0}, {130,50,200}, {0,0,0}, {180,80,255}
};

static RGB8 blend(RGB8 a, RGB8 b, double t) { RGB8 c = { (uint8_t)(a.r + (b.r - a.r) * t), (uint8_t)(a.g + (b.g - a.g) * t), (uint8_t)(a.b + (b.b - a.b) * t) }; return c; }

static void put(int x, int y, RGB8 c, double alpha) {
    if (x < 0 || y < 0 || x >= g_stripW || y >= g_stripH) return;
    uint32_t *p = &g_pixels[y * g_stripW + x];
    // premultiplied over
    double a = alpha;
    uint8_t db = (uint8_t)(*p & 0xFF), dg = (uint8_t)((*p >> 8) & 0xFF), dr = (uint8_t)((*p >> 16) & 0xFF), da = (uint8_t)(*p >> 24);
    uint8_t nb = (uint8_t)(c.b * a + db * (1 - a)), ng = (uint8_t)(c.g * a + dg * (1 - a)), nr = (uint8_t)(c.r * a + dr * (1 - a));
    uint8_t na = (uint8_t)(255 * a + da * (1 - a));
    *p = (na << 24) | (nr << 16) | (ng << 8) | nb;
}

static void fill(RECT r, RGB8 top, RGB8 bottom, double alpha) {
    int h = r.bottom - r.top; if (h <= 0) return;
    for (int y = r.top; y < r.bottom; y++) {
        RGB8 c = blend(top, bottom, (double)(y - r.top) / (double)h);
        for (int x = r.left; x < r.right; x++) put(x, y, c, alpha);
    }
}

static int g_hoverMidi = -1;
static int g_downMidi = -1;

static void paint(void) {
    memset(g_pixels, 0, sizeof(uint32_t) * g_stripW * g_stripH);
    const RGB8 whiteHi = { 62, 72, 82 }, whiteLo = { 44, 54, 62 };
    const RGB8 groove = { 140, 155, 165 };
    const RGB8 blackHi = { 170, 100, 255 }, blackLo = { 120, 60, 210 };
    const RGB8 black = { 0, 0, 0 }, white = { 255, 255, 255 };

    // Backplate: the groove colour shows between keys and as a hairline frame.
    RECT plate = { g_padX - 1, g_padY - 1, g_padX + g_keyW * WHITE_COUNT, g_padY + g_keyH + 1 };
    fill(plate, groove, groove, 0.55);

    for (int i = 0; i < WHITE_COUNT; i++) {
        int m = g_whiteMidi[i];
        RECT r = white_rect(i);
        RGB8 hi = whiteHi, lo = whiteLo;
        if (m == g_downMidi) { RGB8 lit = blend(CHROMA[m % 12], black, 0.22); hi = lit; lo = lit; }
        else if (m == g_hoverMidi) { hi = blend(whiteHi, white, 0.12); lo = blend(whiteLo, white, 0.12); }
        fill(r, hi, lo, 1.0);
        // Chromatic stripe along the foot of each natural, as on the Mac strip.
        RECT s = { r.left + 2, r.bottom - 3, r.right - 2, r.bottom - 1 };
        fill(s, CHROMA[m % 12], CHROMA[m % 12], m == g_downMidi ? 0.0 : 0.9);
    }
    for (int i = 0; i < g_blackCount; i++) {
        int m = g_blackMidi[i];
        RECT r = black_rect(m);
        RGB8 hi = blackHi, lo = blackLo;
        if (m == g_downMidi) { hi = white; lo = blend(white, blackHi, 0.4); }
        else if (m == g_hoverMidi) { hi = blend(blackHi, white, 0.18); lo = blend(blackLo, white, 0.18); }
        RECT shadow = { r.left, r.top, r.right, r.bottom + 1 };
        fill(shadow, black, black, 0.35);
        fill(r, hi, lo, 1.0);
    }
    // Round the strip's outer corners by knocking out two pixels.
    int L = g_padX - 1, R = g_padX + g_keyW * WHITE_COUNT - 1, T = g_padY - 1, B = g_padY + g_keyH;
    int corners[4][2] = { {L, T}, {R, T}, {L, B}, {R, B} };
    for (int c = 0; c < 4; c++) {
        g_pixels[corners[c][1] * g_stripW + corners[c][0]] = 0;
    }
}

static void push_frame(HWND hwnd, int x, int y) {
    paint();
    POINT dst = { x, y }, src = { 0, 0 };
    SIZE size = { g_stripW, g_stripH };
    BLENDFUNCTION bf = { AC_SRC_OVER, 0, 255, AC_SRC_ALPHA };
    HDC screen = GetDC(NULL);
    UpdateLayeredWindow(hwnd, screen, &dst, &size, g_memDC, &src, 0, &bf, ULW_ALPHA);
    ReleaseDC(NULL, screen);
}

static void ensure_surface(void) {
    if (g_dib) { DeleteObject(g_dib); g_dib = NULL; }
    if (!g_memDC) g_memDC = CreateCompatibleDC(NULL);
    BITMAPINFO bi; memset(&bi, 0, sizeof bi);
    bi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
    bi.bmiHeader.biWidth = g_stripW; bi.bmiHeader.biHeight = -g_stripH;
    bi.bmiHeader.biPlanes = 1; bi.bmiHeader.biBitCount = 32; bi.bmiHeader.biCompression = BI_RGB;
    g_dib = CreateDIBSection(g_memDC, &bi, DIB_RGB_COLORS, (void **)&g_pixels, NULL, 0);
    SelectObject(g_memDC, g_dib);
}

// ────────────────────── taskbar tracking ─────────────────────────

static int g_posX = 0, g_posY = 0;
static int g_lastTaskbarH = 0;

// Find the taskbar and tray, pick our slot: left of the tray with a gap,
// vertically centred. Returns 0 when there is no taskbar to sit on.
static int find_slot(int *outX, int *outY, int *outTaskbarH) {
    HWND tray = FindWindowW(L"Shell_TrayWnd", NULL);
    if (!tray || !IsWindowVisible(tray)) return 0;
    RECT tr; GetWindowRect(tray, &tr);
    int taskbarH = tr.bottom - tr.top;
    if (taskbarH <= 0 || taskbarH > 200) return 0;
    RECT nr = tr;
    HWND notify = FindWindowExW(tray, NULL, L"TrayNotifyWnd", NULL);
    if (notify) GetWindowRect(notify, &nr); else nr.left = tr.right - 300;
    *outTaskbarH = taskbarH;
    *outX = nr.left - 14 - g_stripW;
    *outY = tr.top;
    return 1;
}

static int fullscreen_app_in_front(void) {
    QUERY_USER_NOTIFICATION_STATE st;
    if (SUCCEEDED(SHQueryUserNotificationState(&st)))
        return st == QUNS_BUSY || st == QUNS_RUNNING_D3D_FULL_SCREEN || st == QUNS_PRESENTATION_MODE;
    return 0;
}

static void track(HWND hwnd, int force) {
    int x, y, h;
    if (!find_slot(&x, &y, &h) || fullscreen_app_in_front()) { ShowWindow(hwnd, SW_HIDE); return; }
    if (h != g_lastTaskbarH || force) {
        g_lastTaskbarH = h;
        layout_for_taskbar_height(h);
        ensure_surface();
        find_slot(&x, &y, &h);
    }
    g_posX = x; g_posY = y;
    SetWindowPos(hwnd, HWND_TOPMOST, x, y, g_stripW, g_stripH, SWP_NOACTIVATE | SWP_SHOWWINDOW);
    push_frame(hwnd, x, y);
}

// ─────────────────────────── window ──────────────────────────────

#define WM_TRAY (WM_APP + 1)
#define IDM_QUIT 100
#define IDM_PROGRAM_BASE 200
#define IDT_TRACK 1

static const struct { const wchar_t *name; int program; } PROGRAMS[] = {
    { L"Acoustic Grand Piano", 0 }, { L"Electric Piano", 4 }, { L"Harpsichord", 6 },
    { L"Vibraphone", 11 }, { L"Drawbar Organ", 16 }, { L"Nylon Guitar", 24 },
    { L"Electric Bass", 33 }, { L"Strings", 48 }, { L"Choir", 52 }, { L"Flute", 73 },
    { L"Square Lead", 80 }, { L"Warm Pad", 89 },
};

static NOTIFYICONDATAW g_nid;

static void show_menu(HWND hwnd) {
    POINT p; GetCursorPos(&p);
    HMENU menu = CreatePopupMenu();
    for (int i = 0; i < (int)(sizeof PROGRAMS / sizeof PROGRAMS[0]); i++)
        AppendMenuW(menu, MF_STRING | (PROGRAMS[i].program == g_program ? MF_CHECKED : 0), IDM_PROGRAM_BASE + i, PROGRAMS[i].name);
    AppendMenuW(menu, MF_SEPARATOR, 0, NULL);
    AppendMenuW(menu, MF_STRING, IDM_QUIT, L"Quit Menu Band");
    SetForegroundWindow(hwnd);
    TrackPopupMenu(menu, TPM_RIGHTBUTTON | TPM_BOTTOMALIGN, p.x, p.y, 0, hwnd, NULL);
    DestroyMenu(menu);
}

static void note_down(HWND hwnd, int midi) {
    if (midi < 0 || midi == g_downMidi) return;
    if (g_downMidi >= 0) synth_note_off(g_downMidi);
    g_downMidi = midi;
    synth_note_on(midi, 100);
    push_frame(hwnd, g_posX, g_posY);
}

static void note_up(HWND hwnd) {
    if (g_downMidi >= 0) { synth_note_off(g_downMidi); g_downMidi = -1; push_frame(hwnd, g_posX, g_posY); }
}

static LRESULT CALLBACK wndproc(HWND hwnd, UINT msg, WPARAM wp, LPARAM lp) {
    switch (msg) {
    case WM_CREATE:
        SetTimer(hwnd, IDT_TRACK, 400, NULL);
        return 0;
    case WM_TIMER:
        if (wp == IDT_TRACK) track(hwnd, 0);
        return 0;
    case WM_MOUSEACTIVATE:
        return MA_NOACTIVATE;
    case WM_MOUSEMOVE: {
        int midi = key_at(GET_X_LPARAM(lp), GET_Y_LPARAM(lp));
        TRACKMOUSEEVENT tme = { sizeof tme, TME_LEAVE, hwnd, 0 }; TrackMouseEvent(&tme);
        if (wp & MK_LBUTTON) { if (midi >= 0) note_down(hwnd, midi); }
        if (midi != g_hoverMidi) { g_hoverMidi = midi; push_frame(hwnd, g_posX, g_posY); }
        return 0;
    }
    case WM_MOUSELEAVE:
        if (g_hoverMidi != -1) { g_hoverMidi = -1; push_frame(hwnd, g_posX, g_posY); }
        return 0;
    case WM_LBUTTONDOWN:
        SetCapture(hwnd);
        note_down(hwnd, key_at(GET_X_LPARAM(lp), GET_Y_LPARAM(lp)));
        return 0;
    case WM_LBUTTONUP:
        ReleaseCapture();
        note_up(hwnd);
        return 0;
    case WM_CAPTURECHANGED:
        note_up(hwnd);
        return 0;
    case WM_RBUTTONUP:
        show_menu(hwnd);
        return 0;
    case WM_MOUSEWHEEL: {
        int delta = GET_WHEEL_DELTA_WPARAM(wp);
        int next = g_firstMidi + (delta > 0 ? 12 : -12);
        if (next >= 36 && next <= 84) { note_up(hwnd); g_firstMidi = next; build_keys(); push_frame(hwnd, g_posX, g_posY); }
        return 0;
    }
    case WM_TRAY:
        if (LOWORD(lp) == WM_RBUTTONUP || LOWORD(lp) == WM_LBUTTONUP) show_menu(hwnd);
        return 0;
    case WM_COMMAND: {
        int id = LOWORD(wp);
        if (id == IDM_QUIT) { DestroyWindow(hwnd); return 0; }
        if (id >= IDM_PROGRAM_BASE && id < IDM_PROGRAM_BASE + (int)(sizeof PROGRAMS / sizeof PROGRAMS[0]))
            g_program = PROGRAMS[id - IDM_PROGRAM_BASE].program;
        return 0;
    }
    case WM_DISPLAYCHANGE:
    case WM_DPICHANGED:
    case WM_SETTINGCHANGE:
        track(hwnd, 1);
        return 0;
    case WM_DESTROY:
        Shell_NotifyIconW(NIM_DELETE, &g_nid);
        InterlockedExchange(&g_audioRunning, 0);
        PostQuitMessage(0);
        return 0;
    }
    return DefWindowProcW(hwnd, msg, wp, lp);
}

int WINAPI wWinMain(HINSTANCE hInst, HINSTANCE prev, PWSTR cmdline, int show) {
    (void)prev; (void)cmdline; (void)show;
    SetProcessDpiAwarenessContext(DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2);
    CoInitializeEx(NULL, COINIT_MULTITHREADED);
    InitializeCriticalSection(&g_lock);
    HANDLE single = CreateMutexW(NULL, TRUE, L"computer.aesthetic.menuband.windows");
    if (GetLastError() == ERROR_ALREADY_EXISTS) return 0;

    build_keys();
    if (!audio_start()) MessageBoxW(NULL, L"Menu Band could not open the audio output.", L"Menu Band", MB_ICONWARNING);

    WNDCLASSW wc; memset(&wc, 0, sizeof wc);
    wc.lpfnWndProc = wndproc; wc.hInstance = hInst; wc.lpszClassName = L"MenuBandStrip";
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    RegisterClassW(&wc);
    HWND hwnd = CreateWindowExW(WS_EX_TOPMOST | WS_EX_TOOLWINDOW | WS_EX_NOACTIVATE | WS_EX_LAYERED,
                                L"MenuBandStrip", L"Menu Band", WS_POPUP, 0, 0, 10, 10, NULL, NULL, hInst, NULL);

    memset(&g_nid, 0, sizeof g_nid);
    g_nid.cbSize = sizeof g_nid; g_nid.hWnd = hwnd; g_nid.uID = 1;
    g_nid.uFlags = NIF_ICON | NIF_MESSAGE | NIF_TIP; g_nid.uCallbackMessage = WM_TRAY;
    g_nid.hIcon = LoadIcon(NULL, IDI_APPLICATION);
    wcscpy_s(g_nid.szTip, 128, L"Menu Band");
    Shell_NotifyIconW(NIM_ADD, &g_nid);

    track(hwnd, 1);

    MSG m;
    while (GetMessageW(&m, NULL, 0, 0)) { TranslateMessage(&m); DispatchMessageW(&m); }
    if (g_client) g_client->lpVtbl->Stop(g_client);
    CloseHandle(single);
    return 0;
}
