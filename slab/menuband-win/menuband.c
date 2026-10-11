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
// from MenuBandGMSynth.swift. Notes can also go out to any Windows MIDI
// port instead (a Windows MIDI Services loopback reaches a DAW).
//
// Typing: clicking a key (or the ♪ chip) focuses Menu Band — a blue
// full-screen flash, as on the Mac — and a low-level keyboard hook turns
// the Notepat letter layout into notes, swallowing those keys so the app
// underneath doesn't see them. Escape leaves (red flash).
//
// Build: build.ps1 (cl.exe from the VS 2022 Build Tools). No dependencies
// beyond the Windows SDK.

#define UNICODE
#define _UNICODE
#define WIN32_LEAN_AND_MEAN
#define _USE_MATH_DEFINES
#define _CRT_SECURE_NO_WARNINGS
#include <windows.h>
#include <windowsx.h>
#include <shellapi.h>
#include <mmdeviceapi.h>
#include <audioclient.h>
#include <mmsystem.h>
#include <avrt.h>
#include <dwmapi.h>
#include <winternl.h>
#include <hidusage.h>
#include <hidpi.h>
#include "percussion.h"
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "gm_synth.h"

// The MIDL-generated audio headers only declare these; in C nothing defines
// them, so spell out the well-known values here.
static const CLSID MB_CLSID_MMDeviceEnumerator = {0xBCDE0395,0xE52F,0x467C,{0x8E,0x3D,0xC4,0x57,0x92,0x91,0x69,0x2E}};
static const IID   MB_IID_IMMDeviceEnumerator  = {0xA95664D2,0x9614,0x4F35,{0xA7,0x46,0xDE,0x8D,0xB6,0x36,0x17,0xE6}};
static const IID   MB_IID_IAudioClient         = {0x1CB9AD4C,0xDBFA,0x4C32,{0xB1,0x78,0xC2,0xF5,0x68,0xA7,0x03,0xB2}};
static const IID   MB_IID_IAudioRenderClient   = {0xF294ACFC,0x3146,0x4483,{0xA7,0xBF,0xAD,0xDC,0xA7,0xC2,0x60,0xE2}};
static const GUID  MB_SUBTYPE_IEEE_FLOAT       = {0x00000003,0x0000,0x0010,{0x80,0x00,0x00,0xAA,0x00,0x38,0x9B,0x71}};
static const IID   MB_IID_IAudioClient3        = {0x7ED4EE07,0x8E67,0x4CD4,{0x8C,0x1A,0x2B,0x7A,0x59,0x87,0xAD,0x42}};

static void mb_log(const wchar_t *fmt, ...);
static void setting_set(const wchar_t *name, DWORD v);
static void unlatch(HWND hwnd, int all);
static void latch_note(int midi);
static void audio_set_exclusive(int want);
static void surface_reset(HWND hwnd);
static void set_page(HWND hwnd, int page);
static void cursor_hide(void); static void cursor_show(void);
static void cue(HWND hwnd, int a, int b);
// Shared state used across sections (declared early; defined where it lives).
enum { PAGE_FX = 0, PAGE_SKIN = 1, PAGE_MOUSE = 2, PAGE_SPLIT = 3 };
static int g_page = PAGE_SKIN;
static int page_takes_pad(void) { return g_page == PAGE_SKIN || g_page == PAGE_SPLIT; }
#define IDT_FX 7
static float g_puckAlpha = 0;
static DWORD g_lastTouch = 0;
#define MAX_CONTACTS 10
typedef struct { int id, down, struck; double x, y; uint64_t group; DWORD liftedAt; } Contact;
static Contact g_contacts[MAX_CONTACTS];
static void puck_show(void);
static void pad_paint(void); static void puck_paint(void);
#ifndef HID_USAGE_DIGITIZER_CONTACT_ID
#define HID_USAGE_DIGITIZER_CONTACT_ID 0x51
#endif
#ifndef HID_USAGE_DIGITIZER_TIP_SWITCH
#define HID_USAGE_DIGITIZER_TIP_SWITCH 0x42
#endif
#ifndef HID_USAGE_DIGITIZER_TOUCH_PAD
#define HID_USAGE_DIGITIZER_TOUCH_PAD 0x05
#endif
static int g_enterLatch;
static int g_latched[128];           // Enter-latched notes (drawn lit, sound until unlatched)
static int g_vkRoot[256]; static int g_vkExt[256][2];   // chord bookkeeping per held typing key
static uint64_t g_touchGroup = 1;                       // press/contact groups for the percussion engine
static int g_fnFree = 1;             // bare F3/F4/F5 act as mute / volume down / volume up
static const wchar_t *GM_NAMES[128];
static volatile LONG g_dbgNoteOns;

// ───────────────────────────── synth ─────────────────────────────

#define MAX_VOICES 24
#define PENDING_MAX 256

typedef struct {
    uint8_t midi;
    double freq;
    double velocityGain;
    float panL, panR;
    double attack, release;   // seconds
    double env;
    int releasing;
    int active;
} Voice;

typedef struct { int kind; uint8_t midi; uint8_t velocity; int program; uint32_t seed; } Cmd;
enum { CMD_ON = 1, CMD_OFF = 2, CMD_PANIC = 3, CMD_WON = 4, CMD_WOFF = 5, CMD_PON = 6, CMD_POFF = 7 };

static GMVoice g_cores[MAX_VOICES];       // ~80 KB each: keep them static, never on a stack
static Voice g_voices[MAX_VOICES];
static Cmd g_pending[PENDING_MAX];
static int g_pendingCount = 0;
static CRITICAL_SECTION g_lock;
static uint32_t g_seedCounter = 0x9E3779B9u;
static double g_sampleRate = 48000.0;
static const double g_masterGain = 0.5;
static volatile int g_program = 78;       // GM program for new notes: 79 Whistle (zero-based 78), as on the Mac
static volatile LONG g_audioRunning = 0;

static double midi_freq(int midi) { return 440.0 * pow(2.0, (midi - 69) / 12.0); }

// ── the FX surface: bend (octaves), echo and space (0…1) ──
// Written by the main thread, read by the render thread. Pitch is smoothed
// per sample (4 ms time constant, as MenuBandGMSynth's glide) so a fast
// slide never steps more than a fraction of a quarter-tone per frame.
static volatile float g_bendTarget = 0, g_echoTarget = 0, g_spaceTarget = 0;
static double g_glidePitch = 1.0;
static double g_pitchBuf[8192];
static float g_fxDry[2 * 8192];

static void pitch_block(int frames) {
    double target = pow(2.0, (double)g_bendTarget), coeff = 1.0 - exp(-1.0 / (g_sampleRate * 0.004)), p = g_glidePitch;
    for (int i = 0; i < frames; i++) { p += (target - p) * coeff; g_pitchBuf[i] = p; }
    g_glidePitch = p;
}

// Tape echo: 300 ms, 58 % feedback, 3.6 kHz low-pass in the loop, 100 % wet
// on a send whose gain is pow(a, 1.12) × 0.42 (MenuBandSynth.setEcho).
#define ECHO_MAX 96000
static float g_echoL[ECHO_MAX], g_echoR[ECHO_MAX]; static int g_echoW = 0; static float g_echoLpL = 0, g_echoLpR = 0;
// Hall: a small Schroeder — four damped combs + two all-passes per side —
// on a send of pow(space, 1.10) × 0.52, plus the echo's bloom pow(echo, 1.15) × 0.14.
#define RV_COMBS 4
static const int RV_COMB_LEN[RV_COMBS] = { 1557, 1617, 1491, 1422 };
static float g_rvComb[2][RV_COMBS][2048]; static int g_rvCombW[2][RV_COMBS]; static float g_rvCombLp[2][RV_COMBS];
static float g_rvAp1[2][1024], g_rvAp2[2][512]; static int g_rvAp1W[2], g_rvAp2W[2];   // lengths scale with the rate (605/245 at 48 k)
static float g_echoGain = 0, g_spaceGain = 0;

static float reverb_sample(int ch, float in) {
    float acc = 0; double k = g_sampleRate / 44100.0;
    for (int c = 0; c < RV_COMBS; c++) {
        int len = (int)(RV_COMB_LEN[c] * k) + ch * 23; if (len > 2047) len = 2047;
        int w = g_rvCombW[ch][c] % len; float y = g_rvComb[ch][c][w];
        g_rvCombLp[ch][c] += (y - g_rvCombLp[ch][c]) * 0.35f;             // damping
        g_rvComb[ch][c][w] = in + g_rvCombLp[ch][c] * 0.84f;
        g_rvCombW[ch][c] = (w + 1) % len; acc += y;
    }
    acc *= 0.25f;
    { int len = (int)(556 * k); if (len > 1023) len = 1023; int w = g_rvAp1W[ch] % len; float b = g_rvAp1[ch][w]; float y = -acc + b; g_rvAp1[ch][w] = acc + b * 0.5f; g_rvAp1W[ch] = (w + 1) % len; acc = y; }
    { int len = (int)(225 * k); if (len > 511) len = 511; int w = g_rvAp2W[ch] % len; float b = g_rvAp2[ch][w]; float y = -acc + b; g_rvAp2[ch][w] = acc + b * 0.5f; g_rvAp2W[ch] = (w + 1) % len; acc = y; }
    return acc;
}

static void fx_process(float *out, int frames) {
    float echoT = powf(fmaxf(0, fminf(1, g_echoTarget)), 1.12f) * 0.42f;
    float hallT = fmaxf(powf(fmaxf(0, fminf(1, g_spaceTarget)), 1.10f) * 0.52f, powf(fmaxf(0, fminf(1, g_echoTarget)), 1.15f) * 0.14f);
    int echoLen = (int)(g_sampleRate * 0.30); if (echoLen > ECHO_MAX) echoLen = ECHO_MAX;
    float lp = 1.0f - expf(-2.0f * 3.14159f * 3600.0f / (float)g_sampleRate);
    int quiet = echoT < 0.0005f && g_echoGain < 0.0005f && hallT < 0.0005f && g_spaceGain < 0.0005f;
    for (int i = 0; i < frames; i++) {
        g_echoGain += (echoT - g_echoGain) * 0.0008f; g_spaceGain += (hallT - g_spaceGain) * 0.0008f;
        float dl = out[2 * i], dr = out[2 * i + 1];
        // echo
        float el = g_echoL[g_echoW], er = g_echoR[g_echoW];
        g_echoLpL += (el - g_echoLpL) * lp; g_echoLpR += (er - g_echoLpR) * lp;
        g_echoL[g_echoW] = dl * g_echoGain + g_echoLpL * 0.58f; g_echoR[g_echoW] = dr * g_echoGain + g_echoLpR * 0.58f;
        g_echoW = (g_echoW + 1) % echoLen;
        // hall (fed by dry + echo)
        float rl = 0, rr = 0;
        if (!quiet) { rl = reverb_sample(0, (dl + el) * g_spaceGain); rr = reverb_sample(1, (dr + er) * g_spaceGain); }
        out[2 * i] = dl + el + rl; out[2 * i + 1] = dr + er + rr;
    }
}

// Stereo placement: the Mac centres every voice (0.707/0.707) and lets MIDI
// CC 10 pan a channel. Here each note sits where it would on a piano — low
// notes a little left, high a little right, ±35 % of full width, constant
// power — so chords open up without anything leaving the middle.
static void pan_for_midi(int midi, float *l, float *r) {
    double x = (midi - 60) / 36.0; if (x < -1) x = -1; if (x > 1) x = 1;
    double theta = (M_PI / 4) * (1 + 0.35 * x);          // 0 = hard left … π/2 = hard right
    *l = (float)cos(theta); *r = (float)sin(theta);
}

// Master: a soft knee that catches a fistful of notes without the pumping of
// a hard clip. Transparent below -6 dBFS, 1.0 in → ~0.76 out, never past 1.
static float soft_limit(float x) {
    float a = fabsf(x); if (a < 0.5f) return x;
    float y = 0.5f + 0.5f * tanhf((a - 0.5f) * 2.0f);
    return x < 0 ? -y : y;
}
static void wh_note_on(int midi, int velocity); static void wh_note_off(int midi); static void wh_panic(void); static void wh_render(float *out, int frames);
static void piano_note_on(int midi, int velocity); static void piano_note_off(int midi); static void piano_panic(void); static void piano_render(float *out, int frames); static int piano_available(void);

static void synth_push(Cmd c) {
    EnterCriticalSection(&g_lock);
    if (g_pendingCount < PENDING_MAX) g_pending[g_pendingCount++] = c;
    LeaveCriticalSection(&g_lock);
}

static void synth_note_on(int midi, int velocity) {
    if (!gm_program_implemented(g_program)) { mb_log(L"noteOn: program %d not implemented", g_program); return; }
    InterlockedIncrement(&g_dbgNoteOns);
    g_seedCounter = g_seedCounter * 1664525u + 1013904223u;
    Cmd c = { CMD_ON, (uint8_t)midi, (uint8_t)velocity, g_program, g_seedCounter };
    synth_push(c);
}

static void synth_note_off(int midi) { Cmd c = { CMD_OFF, (uint8_t)midi, 0, 0, 0 }; synth_push(c); }
static void synth_panic(void) { Cmd c = { CMD_PANIC, 0, 0, 0, 0 }; synth_push(c); }

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
                pan_for_midi(c->midi, &v->panL, &v->panR);
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
            wh_panic(); piano_panic();
        } else if (c->kind == CMD_WON) {
            wh_note_on(c->midi, c->velocity);
        } else if (c->kind == CMD_WOFF) {
            wh_note_off(c->midi);
        } else if (c->kind == CMD_PON) {
            piano_note_on(c->midi, c->velocity);
        } else if (c->kind == CMD_POFF) {
            piano_note_off(c->midi);
        }
    }

    pitch_block(frames);
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
            double s = gm_voice_render(core, g_sampleRate, env, v->freq * g_pitchBuf[i]);
            if (!isfinite(s)) { active = 0; break; }
            float amp = (float)(s * v->velocityGain * g_masterGain);
            out[2 * i] += amp * v->panL; out[2 * i + 1] += amp * v->panR;
        }
        v->env = env; v->active = active;
    }
    wh_render(out, frames);
    piano_render(out, frames);
    fx_process(out, frames);
    perc_set_pitch(g_glidePitch);
    perc_render(out, frames);
    for (int i = 0; i < 2 * frames; i++) out[i] = soft_limit(out[i]);
}

// ─────────── the AC grand piano (`1), ported from MenuBandACPianoVoice.swift ───────────
// The Salamander Grand Piano V3 anchors AC OS plays (fedac/native/samples/piano,
// CC0 Alexander Holm): 26 notes every three semitones, header-less float32
// mono at 48 kHz, played the way audio.c plays them — nearest anchor by MIDI
// distance, step 2^(semitones/12), linear interpolation, the note running out
// at the end of its own recorded decay, and a short exponential damper on
// key-up. Program 1 routes here when the bank is beside the exe.

#define PI_VOICES 24
typedef struct { int midi; float *data; int count; } Anchor;
static Anchor g_anchors[32]; static int g_anchorCount = 0;
typedef struct { int midi, anchor; double pos, step; float gain, panL, panR, env; int releasing, active; } PVoice;
static PVoice g_pv[PI_VOICES];
static const float PI_VOICE_AMP = 2.4f, PI_MASTER = 0.9f;
static const double PI_RELEASE = 0.07, PI_BANK_RATE = 48000.0;
static float g_piBus[2 * 8192];

static void piano_load_bank(void) {
    wchar_t dir[MAX_PATH]; GetModuleFileNameW(NULL, dir, MAX_PATH);
    wchar_t *slash = wcsrchr(dir, L'\\'); if (slash) *slash = 0;
    wcscat_s(dir, MAX_PATH, L"\\samples\\piano\\");
    for (int midi = 0; midi < 128 && g_anchorCount < 32; midi++) {
        wchar_t path[MAX_PATH]; swprintf(path, MAX_PATH, L"%s%d.raw", dir, midi);
        FILE *f = NULL; if (_wfopen_s(&f, path, L"rb") != 0 || !f) continue;
        fseek(f, 0, SEEK_END); long bytes = ftell(f); fseek(f, 0, SEEK_SET);
        if (bytes < 8) { fclose(f); continue; }
        float *data = (float *)malloc((size_t)bytes + sizeof(float));
        size_t n = fread(data, 1, (size_t)bytes, f); fclose(f);
        int count = (int)(n / sizeof(float)); data[count] = 0;
        g_anchors[g_anchorCount].midi = midi; g_anchors[g_anchorCount].data = data; g_anchors[g_anchorCount].count = count; g_anchorCount++;
    }
    mb_log(L"piano bank: %d anchors from %s", g_anchorCount, dir);
}
static int piano_available(void) { return g_anchorCount > 0; }

static int piano_allocate(void) {
    for (int i = 0; i < PI_VOICES; i++) if (!g_pv[i].active) return i;
    int best = 0; float bestScore = 1e30f;
    for (int i = 0; i < PI_VOICES; i++) { float sc = (g_pv[i].releasing ? 0 : 1000) + g_pv[i].env; if (sc < bestScore) { bestScore = sc; best = i; } }
    return best;
}
static void piano_note_on(int midi, int velocity) {
    if (!g_anchorCount) return;
    int a = 0, bestD = 999;
    for (int i = 0; i < g_anchorCount; i++) { int d = abs(midi - g_anchors[i].midi); if (d < bestD) { bestD = d; a = i; } }
    int slot = piano_allocate();
    PVoice *v = &g_pv[slot]; memset(v, 0, sizeof *v);
    v->midi = midi; v->anchor = a; v->pos = 0;
    v->step = pow(2.0, (midi - g_anchors[a].midi) / 12.0) * (PI_BANK_RATE / g_sampleRate);
    v->gain = PI_VOICE_AMP * (float)fmax(0.05, fmin(1.0, velocity / 127.0));
    pan_for_midi(midi, &v->panL, &v->panR);
    v->env = 1; v->active = 1;
}
static void piano_note_off(int midi) { for (int i = 0; i < PI_VOICES; i++) if (g_pv[i].active && !g_pv[i].releasing && g_pv[i].midi == midi) g_pv[i].releasing = 1; }
static void piano_panic(void) { for (int i = 0; i < PI_VOICES; i++) g_pv[i].active = 0; }

static void piano_render(float *out, int frames) {
    if (frames > 8192) frames = 8192;
    int any = 0; for (int i = 0; i < PI_VOICES; i++) if (g_pv[i].active) { any = 1; break; }
    if (!any) return;
    memset(g_piBus, 0, sizeof(float) * 2 * frames);
    const float rel = (float)exp(-1.0 / (g_sampleRate * PI_RELEASE));
    for (int idx = 0; idx < PI_VOICES; idx++) {
        PVoice *v = &g_pv[idx]; if (!v->active) continue;
        const Anchor *a = &g_anchors[v->anchor];
        double pos = v->pos; float env = v->env; int active = 1;
        for (int i = 0; i < frames; i++) {
            int ip = (int)pos;
            if (ip + 1 >= a->count) { active = 0; break; }          // the recording ran out
            float frac = (float)(pos - ip);
            float s = a->data[ip] + (a->data[ip + 1] - a->data[ip]) * frac;
            if (v->releasing) { env *= rel; if (env < 0.001f) { active = 0; break; } }
            float o = s * v->gain * env;
            g_piBus[2 * i] += o * v->panL; g_piBus[2 * i + 1] += o * v->panR;
            pos += v->step * g_pitchBuf[i];
        }
        v->pos = pos; v->env = env; v->active = active;
    }
    for (int i = 0; i < 2 * frames; i++) out[i] += tanhf(g_piBus[i] * PI_MASTER);
}

// ─────────────── the AC whistle (`79), ported from MenuBandWhistleVoice.swift ───────────────
// A human whistle, not a tin whistle: a sine core with an onset scoop, 1/f-ish
// jitter and a vibrato that grows in; one gentle second harmonic; "air" from
// white noise through a high-Q resonator on the fundamental plus a quiet hiss
// band; and a shared breath that drifts and flutters. Program 78 routes here,
// everything else to gm_synth.

#define WH_VOICES 12
typedef struct {
    int midi; double f0; float gain; float panL, panR;
    double phase, age, env; int releasing, active; uint32_t seed;
    double r1, r2, hpz, lpz;
} WVoice;
static WVoice g_wv[WH_VOICES];
static double g_whDrift = 0, g_whFlutter = 0, g_whJitter = 0, g_whVibPhase = 0;
static const double g_whVibRate = 5.2;
static uint32_t g_whBreathSeed = 0x2545F491u, g_whSeedCounter = 0x12345678u;
static double g_whBreath[8192], g_whCents[8192];
static float g_whBus[2 * 8192];
static const float WH_MASTER = 0.9f, WH_TONE = 0.30f;
static const double WH_OVERTONE = 0.05, WH_AIR = 0.09, WH_HISS = 0.014;
static const double WH_ATTACK = 0.035, WH_RELEASE = 0.09, WH_SCOOP = -45.0, WH_SCOOP_TAU = 0.045, WH_JITTER = 6.0, WH_VIB = 5.0;

static double wh_noise(uint32_t *st) { *st ^= *st << 13; *st ^= *st >> 17; *st ^= *st << 5; return (double)(*st) / 4294967295.0 * 2.0 - 1.0; }

static int wh_allocate(void) {
    for (int i = 0; i < WH_VOICES; i++) if (!g_wv[i].active) return i;
    int best = 0; double bestScore = 1e300;
    for (int i = 0; i < WH_VOICES; i++) { double sc = (g_wv[i].releasing ? 0 : 1000) + g_wv[i].env; if (sc < bestScore) { bestScore = sc; best = i; } }
    return best;
}

static void wh_note_on(int midi, int velocity) {
    int slot = wh_allocate();
    g_whSeedCounter = g_whSeedCounter * 1664525u + 1013904223u;
    WVoice v; memset(&v, 0, sizeof v);
    v.midi = midi; v.f0 = midi_freq(midi);
    double vel = fmax(0.05, fmin(1.0, velocity / 127.0));
    v.gain = (float)(WH_TONE * (0.55 + 0.45 * vel));
    v.seed = g_whSeedCounter | 1u; v.active = 1;
    pan_for_midi(midi, &v.panL, &v.panR);
    g_wv[slot] = v;
}

static void wh_note_off(int midi) { for (int i = 0; i < WH_VOICES; i++) if (g_wv[i].active && !g_wv[i].releasing && g_wv[i].midi == midi) g_wv[i].releasing = 1; }
static void wh_panic(void) { for (int i = 0; i < WH_VOICES; i++) g_wv[i].active = 0; }

// Adds the whistle bus (soft-clipped like the Mac node) into `out`.
static void wh_render(float *out, int frames) {
    if (frames > 8192) frames = 8192;
    int sounding = 0; double vibGrow = 0;
    for (int i = 0; i < WH_VOICES; i++) if (g_wv[i].active) { sounding++; if (g_wv[i].age > vibGrow) vibGrow = g_wv[i].age; }
    if (!sounding) return;
    const double sr = g_sampleRate, dt = 1.0 / sr;
    const double attackInc = dt / WH_ATTACK, releaseMul = exp(-dt / WH_RELEASE);
    const double driftCoeff = 1 - exp(-dt * 2 * M_PI * 0.7), flutterCoeff = 1 - exp(-dt * 2 * M_PI * 18), jitterCoeff = 1 - exp(-dt * 2 * M_PI * 7);
    const double hpCoeff = exp(-dt * 2 * M_PI * 1800), lpCoeff = exp(-dt * 2 * M_PI * 5000);
    for (int i = 0; i < frames; i++) {
        double n1 = wh_noise(&g_whBreathSeed); g_whDrift += (n1 * 0.14 - g_whDrift) * driftCoeff;
        double n2 = wh_noise(&g_whBreathSeed); g_whFlutter += (n2 * 0.06 - g_whFlutter) * flutterCoeff;
        double b = 0.82 + g_whDrift + g_whFlutter; g_whBreath[i] = b < 0.35 ? 0.35 : (b > 1.15 ? 1.15 : b);
        double n3 = wh_noise(&g_whBreathSeed); g_whJitter += (n3 * WH_JITTER - g_whJitter) * jitterCoeff;
        double vibDepth = WH_VIB * fmin(1.0, vibGrow / 0.45) * g_whBreath[i];
        g_whVibPhase += g_whVibRate * dt; if (g_whVibPhase >= 1) g_whVibPhase -= 1;
        g_whCents[i] = g_whJitter + sin(g_whVibPhase * 2 * M_PI) * vibDepth;
    }
    memset(g_whBus, 0, sizeof(float) * 2 * frames);
    double airScale = 1.0 / sqrt((double)sounding);
    for (int idx = 0; idx < WH_VOICES; idx++) {
        WVoice *v = &g_wv[idx]; if (!v->active) continue;
        int active = 1;
        for (int i = 0; i < frames; i++) {
            if (v->releasing) { v->env *= releaseMul; if (v->env < 0.0008) { active = 0; break; } }
            else if (v->env < 1) { v->env += attackInc; if (v->env > 1) v->env = 1; }
            v->age += dt;
            double breath = g_whBreath[i];
            double scoop = WH_SCOOP * exp(-v->age / WH_SCOOP_TAU);
            double droop = v->releasing ? -14.0 * (1 - v->env) : 0;
            double cents = scoop + g_whCents[i] + droop;
            double f = v->f0 * pow(2.0, cents / 1200.0) * g_pitchBuf[i];
            v->phase += f * dt; if (v->phase >= 1) v->phase -= 1;
            double ph = v->phase * 2 * M_PI;
            double tone = sin(ph) + sin(2 * ph) * WH_OVERTONE * breath;
            double w = 2 * M_PI * fmin(f, sr * 0.45) / sr;
            double alpha = sin(w) / (2 * 28.0), a0 = 1 + alpha;
            double x = wh_noise(&v->seed);
            double y = (alpha * x - (-2 * cos(w)) * v->r1 - (1 - alpha) * v->r2) / a0;
            v->r2 = v->r1; v->r1 = y;
            v->hpz = hpCoeff * (v->hpz + x) - x;
            double hp = x + v->hpz;
            v->lpz = v->lpz * lpCoeff + hp * (1 - lpCoeff);
            double air = (y * WH_AIR * (0.6 + 0.4 * breath) + v->lpz * WH_HISS * breath) * airScale;
            float amp = (float)(v->env * (0.78 + 0.22 * breath));
            float smp = (float)(tone + air) * amp * v->gain;
            g_whBus[2 * i] += smp * v->panL; g_whBus[2 * i + 1] += smp * v->panR;
        }
        v->active = active;
    }
    for (int i = 0; i < 2 * frames; i++) out[i] += tanhf(g_whBus[i] * WH_MASTER);
}

// ───────────────────────────── WASAPI ────────────────────────────

static IAudioClient *g_client = NULL;
static IAudioRenderClient *g_render = NULL;
static HANDLE g_audioEvent = NULL, g_audioThread = NULL;
static UINT32 g_bufferFrames = 0;
static int g_sampleFormat = 0;      // 0 float32, 1 int16, 2 int24-in-32, 3 int32
static int g_exclusive = 0;         // current stream is exclusive
static int g_lowLatency = 1;        // policy: exclusive while armed
static float g_scratch[2 * 8192];

static void write_frames(BYTE *data, int frames) {
    if (g_sampleFormat == 0) { synth_render((float *)data, frames); return; }
    synth_render(g_scratch, frames);
    if (g_sampleFormat == 1) { short *o = (short *)data; for (int i = 0; i < 2 * frames; i++) { float v = g_scratch[i]; if (v > 1) v = 1; if (v < -1) v = -1; o[i] = (short)(v * 32767.0f); } }
    else { int *o = (int *)data; float sc = g_sampleFormat == 2 ? 8388607.0f * 256.0f : 2147483647.0f; for (int i = 0; i < 2 * frames; i++) { float v = g_scratch[i]; if (v > 1) v = 1; if (v < -1) v = -1; o[i] = (int)(v * sc); } }
}

static volatile LONG g_dbgCallbacks = 0, g_dbgFrames = 0;
static volatile float g_dbgPeak = 0;

static DWORD WINAPI audio_thread(LPVOID arg) {
    (void)arg;
    DWORD taskIndex = 0;
    HANDLE mmcss = AvSetMmThreadCharacteristicsW(L"Pro Audio", &taskIndex);
    DWORD lastLog = GetTickCount(); int timeouts = 0;
    while (g_audioRunning) {
        if (WaitForSingleObject(g_audioEvent, 2000) != WAIT_OBJECT_0) { timeouts++; mb_log(L"audio: event timeout #%d", timeouts); continue; }
        UINT32 frames = g_bufferFrames;
        if (!g_exclusive) {
            UINT32 padding = 0;
            HRESULT hp = g_client->lpVtbl->GetCurrentPadding(g_client, &padding);
            if (FAILED(hp)) { mb_log(L"audio: GetCurrentPadding 0x%08lX", hp); continue; }
            frames = g_bufferFrames - padding;
            if (frames == 0) continue;
        }
        BYTE *data = NULL;
        HRESULT hr = g_render->lpVtbl->GetBuffer(g_render, frames, &data);
        if (FAILED(hr)) { if (hr != AUDCLNT_E_BUFFER_TOO_LARGE) mb_log(L"audio: GetBuffer 0x%08lX", hr); continue; }
        if (frames > 8192) frames = 8192;
        write_frames(data, (int)frames);
        float peak = g_dbgPeak;
        if (g_sampleFormat == 0) { float *f = (float *)data; for (UINT32 i = 0; i < frames * 2; i++) { float a = fabsf(f[i]); if (a > peak) peak = a; } }
        else { for (UINT32 i = 0; i < frames * 2; i++) { float a = fabsf(g_scratch[i]); if (a > peak) peak = a; } }
        g_dbgPeak = peak; InterlockedIncrement(&g_dbgCallbacks); InterlockedAdd(&g_dbgFrames, (LONG)frames);
        g_render->lpVtbl->ReleaseBuffer(g_render, frames, 0);
        if (GetTickCount() - lastLog > 2000) {
            int active = 0; for (int i = 0; i < MAX_VOICES; i++) if (g_voices[i].active) active++;
            for (int i = 0; i < WH_VOICES; i++) if (g_wv[i].active) active++;
            mb_log(L"audio: callbacks=%ld frames=%ld peak=%.4f active=%d noteOns=%ld sr=%.0f buf=%u %s", g_dbgCallbacks, g_dbgFrames, g_dbgPeak, active, g_dbgNoteOns, g_sampleRate, g_bufferFrames, g_exclusive ? L"exclusive" : L"shared");
            g_dbgPeak = 0; lastLog = GetTickCount();
        }
    }
    if (mmcss) AvRevertMmThreadCharacteristics(mmcss);
    return 0;
}

static void audio_stop(void) {
    if (!g_audioRunning && !g_client) return;
    InterlockedExchange(&g_audioRunning, 0);
    if (g_audioEvent) SetEvent(g_audioEvent);
    if (g_audioThread) { WaitForSingleObject(g_audioThread, 2000); CloseHandle(g_audioThread); g_audioThread = NULL; }
    if (g_client) g_client->lpVtbl->Stop(g_client);
    if (g_render) { g_render->lpVtbl->Release(g_render); g_render = NULL; }
    if (g_client) { g_client->lpVtbl->Release(g_client); g_client = NULL; }
    if (g_audioEvent) { CloseHandle(g_audioEvent); g_audioEvent = NULL; }
}

static IAudioClient *open_client(void) {
    IMMDeviceEnumerator *enumerator = NULL; IMMDevice *device = NULL; IAudioClient *c = NULL;
    if (FAILED(CoCreateInstance(&MB_CLSID_MMDeviceEnumerator, NULL, CLSCTX_ALL, &MB_IID_IMMDeviceEnumerator, (void **)&enumerator))) return NULL;
    if (SUCCEEDED(enumerator->lpVtbl->GetDefaultAudioEndpoint(enumerator, eRender, eConsole, &device))) { device->lpVtbl->Activate(device, &MB_IID_IAudioClient, CLSCTX_ALL, NULL, (void **)&c); device->lpVtbl->Release(device); }
    enumerator->lpVtbl->Release(enumerator);
    return c;
}

// Exclusive mode at the device's minimum period: bypasses the mixer and the
// Realtek effects, usually ~3 ms. Tries float, 24-in-32, 32, then 16-bit.
static int audio_start_exclusive(void) {
    g_client = open_client(); if (!g_client) return 0;
    REFERENCE_TIME defP = 0, minP = 0;
    g_client->lpVtbl->GetDevicePeriod(g_client, &defP, &minP);
    WAVEFORMATEX *mix = NULL; g_client->lpVtbl->GetMixFormat(g_client, &mix);
    DWORD rate = mix ? mix->nSamplesPerSec : 48000; if (mix) CoTaskMemFree(mix);
    const struct { int fmt; WORD bits, valid; GUID sub; } tries[] = {
        { 0, 32, 32, {0x00000003,0x0000,0x0010,{0x80,0x00,0x00,0xAA,0x00,0x38,0x9B,0x71}} },
        { 2, 32, 24, {0x00000001,0x0000,0x0010,{0x80,0x00,0x00,0xAA,0x00,0x38,0x9B,0x71}} },
        { 3, 32, 32, {0x00000001,0x0000,0x0010,{0x80,0x00,0x00,0xAA,0x00,0x38,0x9B,0x71}} },
        { 1, 16, 16, {0x00000001,0x0000,0x0010,{0x80,0x00,0x00,0xAA,0x00,0x38,0x9B,0x71}} },
    };
    for (int t = 0; t < 4; t++) {
        WAVEFORMATEXTENSIBLE fmt; memset(&fmt, 0, sizeof fmt);
        fmt.Format.wFormatTag = WAVE_FORMAT_EXTENSIBLE; fmt.Format.nChannels = 2; fmt.Format.nSamplesPerSec = rate;
        fmt.Format.wBitsPerSample = tries[t].bits; fmt.Format.nBlockAlign = (WORD)(2 * tries[t].bits / 8);
        fmt.Format.nAvgBytesPerSec = rate * fmt.Format.nBlockAlign; fmt.Format.cbSize = 22;
        fmt.Samples.wValidBitsPerSample = tries[t].valid; fmt.dwChannelMask = SPEAKER_FRONT_LEFT | SPEAKER_FRONT_RIGHT; fmt.SubFormat = tries[t].sub;
        if (g_client->lpVtbl->IsFormatSupported(g_client, AUDCLNT_SHAREMODE_EXCLUSIVE, (WAVEFORMATEX *)&fmt, NULL) != S_OK) continue;
        REFERENCE_TIME period = minP > 0 ? minP : defP;
        HRESULT hr = g_client->lpVtbl->Initialize(g_client, AUDCLNT_SHAREMODE_EXCLUSIVE, AUDCLNT_STREAMFLAGS_EVENTCALLBACK, period, period, (WAVEFORMATEX *)&fmt, NULL);
        if (hr == AUDCLNT_E_BUFFER_SIZE_NOT_ALIGNED) {
            UINT32 n = 0; g_client->lpVtbl->GetBufferSize(g_client, &n);
            period = (REFERENCE_TIME)(10000.0 * 1000 / rate * n + 0.5);
            g_client->lpVtbl->Release(g_client); g_client = open_client(); if (!g_client) return 0;
            hr = g_client->lpVtbl->Initialize(g_client, AUDCLNT_SHAREMODE_EXCLUSIVE, AUDCLNT_STREAMFLAGS_EVENTCALLBACK, period, period, (WAVEFORMATEX *)&fmt, NULL);
        }
        if (FAILED(hr)) { mb_log(L"audio: exclusive init (%d-bit) 0x%08lX", tries[t].bits, hr); g_client->lpVtbl->Release(g_client); g_client = open_client(); if (!g_client) return 0; continue; }
        g_sampleFormat = tries[t].fmt; g_sampleRate = rate; g_exclusive = 1;
        g_client->lpVtbl->GetBufferSize(g_client, &g_bufferFrames);
        g_audioEvent = CreateEventW(NULL, FALSE, FALSE, NULL);
        g_client->lpVtbl->SetEventHandle(g_client, g_audioEvent);
        if (FAILED(g_client->lpVtbl->GetService(g_client, &MB_IID_IAudioRenderClient, (void **)&g_render))) return 0;
        BYTE *data = NULL;
        if (SUCCEEDED(g_render->lpVtbl->GetBuffer(g_render, g_bufferFrames, &data))) { memset(data, 0, (size_t)g_bufferFrames * 2 * tries[t].bits / 8); g_render->lpVtbl->ReleaseBuffer(g_render, g_bufferFrames, 0); }
        mb_log(L"audio: EXCLUSIVE %d-bit (%d valid) @%lu, period %.2f ms (device min %.2f, default %.2f), buffer %u frames", tries[t].bits, tries[t].valid, rate, period / 10000.0, minP / 10000.0, defP / 10000.0, g_bufferFrames);
        gm_synth_init(); perc_init(g_sampleRate);
        InterlockedExchange(&g_audioRunning, 1);
        g_audioThread = CreateThread(NULL, 0, audio_thread, NULL, 0, NULL);
        g_client->lpVtbl->Start(g_client);
        return 1;
    }
    mb_log(L"audio: no exclusive format accepted");
    g_client->lpVtbl->Release(g_client); g_client = NULL;
    return 0;
}

static int audio_start(void) {
    g_exclusive = 0; g_sampleFormat = 0;
    g_client = open_client(); if (!g_client) return 0;
    HRESULT hr;

    WAVEFORMATEX *mix = NULL;
    g_client->lpVtbl->GetMixFormat(g_client, &mix);
    g_sampleRate = mix ? mix->nSamplesPerSec : 48000.0;

    // Low-latency shared mode (IAudioClient3, Windows 10+): run at the engine's
    // minimum period instead of the default 10 ms. Needs the mix format itself
    // (float32 at the mix rate on every machine seen so far); falls back below.
    IAudioClient3 *c3 = NULL;
    int mixIsFloat = mix && ((mix->wFormatTag == WAVE_FORMAT_IEEE_FLOAT) || (mix->wFormatTag == WAVE_FORMAT_EXTENSIBLE && ((WAVEFORMATEXTENSIBLE *)mix)->SubFormat.Data1 == 3)) && mix->nChannels == 2 && mix->wBitsPerSample == 32;
    if (mixIsFloat && SUCCEEDED(g_client->lpVtbl->QueryInterface(g_client, &MB_IID_IAudioClient3, (void **)&c3))) {
        UINT32 defP = 0, fundP = 0, minP = 0, maxP = 0;
        if (SUCCEEDED(c3->lpVtbl->GetSharedModeEnginePeriod(c3, mix, &defP, &fundP, &minP, &maxP))) {
            HRESULT h3 = c3->lpVtbl->InitializeSharedAudioStream(c3, AUDCLNT_STREAMFLAGS_EVENTCALLBACK, minP, mix, NULL);
            if (SUCCEEDED(h3)) {
                mb_log(L"audio: low-latency shared stream, period %u frames (%.2f ms; default %u, min %u, max %u)", minP, minP * 1000.0 / g_sampleRate, defP, minP, maxP);
                c3->lpVtbl->Release(c3); CoTaskMemFree(mix);
                goto initialized;
            }
            mb_log(L"audio: InitializeSharedAudioStream failed 0x%08lX, falling back", h3);
        }
        c3->lpVtbl->Release(c3);
        // A failed Initialize leaves the client unusable; get a fresh one.
        g_client->lpVtbl->Release(g_client); g_client = NULL;
        IMMDeviceEnumerator *e2 = NULL; IMMDevice *d2 = NULL;
        if (SUCCEEDED(CoCreateInstance(&MB_CLSID_MMDeviceEnumerator, NULL, CLSCTX_ALL, &MB_IID_IMMDeviceEnumerator, (void **)&e2))) {
            if (SUCCEEDED(e2->lpVtbl->GetDefaultAudioEndpoint(e2, eRender, eConsole, &d2))) { d2->lpVtbl->Activate(d2, &MB_IID_IAudioClient, CLSCTX_ALL, NULL, (void **)&g_client); d2->lpVtbl->Release(d2); }
            e2->lpVtbl->Release(e2);
        }
        if (!g_client) { CoTaskMemFree(mix); return 0; }
    }
    if (mix) CoTaskMemFree(mix);

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
    mb_log(L"audio: default shared stream (20 ms request)");
initialized:
    g_client->lpVtbl->GetBufferSize(g_client, &g_bufferFrames);
    { REFERENCE_TIME lat = 0; g_client->lpVtbl->GetStreamLatency(g_client, &lat); mb_log(L"audio: buffer %u frames (%.2f ms), stream latency %.2f ms", g_bufferFrames, g_bufferFrames * 1000.0 / g_sampleRate, lat / 10000.0); }
    g_audioEvent = CreateEventW(NULL, FALSE, FALSE, NULL);
    g_client->lpVtbl->SetEventHandle(g_client, g_audioEvent);
    hr = g_client->lpVtbl->GetService(g_client, &MB_IID_IAudioRenderClient, (void **)&g_render);
    if (FAILED(hr)) return 0;

    BYTE *data = NULL;
    if (SUCCEEDED(g_render->lpVtbl->GetBuffer(g_render, g_bufferFrames, &data))) {
        memset(data, 0, g_bufferFrames * 8);
        g_render->lpVtbl->ReleaseBuffer(g_render, g_bufferFrames, 0);
    }
    gm_synth_init(); perc_init(g_sampleRate);
    InterlockedExchange(&g_audioRunning, 1);
    g_audioThread = CreateThread(NULL, 0, audio_thread, NULL, 0, NULL);
    g_client->lpVtbl->Start(g_client);
    return 1;
}

// Policy: exclusive (lowest latency, other apps muted) only while armed.
static void audio_set_exclusive(int want) {
    if (!g_lowLatency) want = 0;
    if (want == g_exclusive) return;
    audio_stop();
    if (want && audio_start_exclusive()) return;
    if (want) mb_log(L"audio: exclusive unavailable, staying shared");
    if (!audio_start()) mb_log(L"audio: shared restart FAILED");
}

// ───────────────────────────── MIDI out ──────────────────────────
// -1 = built-in synth; otherwise a winmm device index.

static int g_midiDevice = -1;
static HMIDIOUT g_midiOut = NULL;

static void midi_select(int device) {
    if (g_midiOut) { midiOutReset(g_midiOut); midiOutClose(g_midiOut); g_midiOut = NULL; }
    g_midiDevice = -1;
    if (device >= 0 && midiOutOpen(&g_midiOut, (UINT)device, 0, 0, CALLBACK_NULL) == MMSYSERR_NOERROR) {
        g_midiDevice = device;
        midiOutShortMsg(g_midiOut, 0xC0 | ((DWORD)g_program << 8));
    }
    synth_panic();
}

static void midi_program(void) {
    if (g_midiOut) midiOutShortMsg(g_midiOut, 0xC0 | ((DWORD)g_program << 8));
}

// Every note on/off funnels through here. Program 78 is the AC whistle voice,
// as on the Mac; the other 127 are gm_synth. A note remembers who owns it so
// a program change mid-hold still releases it in the right place.
#define AC_WHISTLE_PROGRAM 78
#define AC_PIANO_PROGRAM 0
static uint8_t g_noteOwner[128];   // 0 gm, 1 whistle, 2 midi out
static void play_on(int midi, int vel) {
    if (midi < 0 || midi > 127) return;
    if (g_midiOut) { g_noteOwner[midi] = 2; midiOutShortMsg(g_midiOut, 0x90 | ((DWORD)midi << 8) | ((DWORD)vel << 16)); }
    else if (g_program == AC_WHISTLE_PROGRAM) { g_noteOwner[midi] = 1; Cmd c = { CMD_WON, (uint8_t)midi, (uint8_t)vel, 0, 0 }; synth_push(c); InterlockedIncrement(&g_dbgNoteOns); }
    else if (g_program == AC_PIANO_PROGRAM && piano_available()) { g_noteOwner[midi] = 3; Cmd c = { CMD_PON, (uint8_t)midi, (uint8_t)vel, 0, 0 }; synth_push(c); InterlockedIncrement(&g_dbgNoteOns); }
    else { g_noteOwner[midi] = 0; synth_note_on(midi, vel); }
}
static void play_off(int midi) {
    if (midi < 0 || midi > 127) return;
    if (g_noteOwner[midi] == 2 && g_midiOut) midiOutShortMsg(g_midiOut, 0x80 | ((DWORD)midi << 8));
    else if (g_noteOwner[midi] == 1) { Cmd c = { CMD_WOFF, (uint8_t)midi, 0, 0, 0 }; synth_push(c); }
    else if (g_noteOwner[midi] == 3) { Cmd c = { CMD_POFF, (uint8_t)midi, 0, 0, 0 }; synth_push(c); }
    else synth_note_off(midi);
}

// ───────────────────────────── strip ─────────────────────────────

#define WHITE_COUNT 14              // C4..B5, two octaves, like the Mac default
static int g_firstMidi = 60;        // C4
static int g_whiteMidi[WHITE_COUNT];
static int g_blackMidi[10];
static int g_blackCount = 0;
static int g_lit[128];              // held count per note (mouse + typing)

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
static int g_chipW = 0, g_pianoX = 0;   // ♪ chip at the left, then the keys

static double g_dpi = 1.5;   // physical px per Mac point; from the taskbar's monitor
static int g_blackTop = 0;

// Metrics lifted from KeyboardIconRenderer.swift (regular layout):
// whiteW 23, whiteH 21, blackW 13.5, blackH 12, pad 0.5, stripe 2, menubar 22.
static int pt(double v) { int r = (int)(v * g_dpi + 0.5); return r < 1 ? 1 : r; }

static void layout_for_taskbar_height(int taskbarH) {
    HWND tray = FindWindowW(L"Shell_TrayWnd", NULL);
    UINT dpi = tray ? GetDpiForWindow(tray) : 96; if (dpi < 72) dpi = 96;
    g_dpi = dpi / 96.0;
    // Taller, narrower keys than the Mac's: 2:1, two thirds of the visible band.
    g_keyH = (taskbarH * 66) / 100; if (g_keyH < 16) g_keyH = 16;
    g_keyW = g_keyH / 2; if (g_keyW < 9) g_keyW = 9;
    g_blackW = (g_keyW * 6) / 10; if (g_blackW < 5) g_blackW = 5;
    g_blackH = (g_keyH * 6) / 10;
    g_padY = (taskbarH - g_keyH) / 2;
    g_padX = pt(6.0);
    g_chipW = pt(36.0);
    g_pianoX = g_padX + g_chipW + pt(6.0);
    g_stripW = g_pianoX + g_keyW * WHITE_COUNT + g_padX;
    g_stripH = taskbarH;
    g_blackTop = g_padY;
}

static int white_index_of(int midi) { for (int i = 0; i < WHITE_COUNT; i++) if (g_whiteMidi[i] == midi) return i; return -1; }

static RECT chip_rect(void) { RECT r = { g_padX, g_padY - 1, g_padX + g_chipW, g_padY + g_keyH + 1 }; return r; }

static RECT white_rect(int i) {
    RECT r = { g_pianoX + i * g_keyW, g_padY, g_pianoX + (i + 1) * g_keyW - 1, g_padY + g_keyH };
    return r;
}

static RECT black_rect(int midi) {
    int below = white_index_of(midi - 1);
    int cx = g_pianoX + (below + 1) * g_keyW;
    RECT r = { cx - g_blackW / 2, g_padY, cx - g_blackW / 2 + g_blackW, g_padY + g_blackH };
    return r;
}

static int key_at(int x, int y) {
    POINT p = { x, y };
    for (int i = 0; i < g_blackCount; i++) { RECT r = black_rect(g_blackMidi[i]); if (PtInRect(&r, p)) return g_blackMidi[i]; }
    for (int i = 0; i < WHITE_COUNT; i++) { RECT r = white_rect(i); if (PtInRect(&r, p)) return g_whiteMidi[i]; }
    return -1;
}

static int chip_at(int x, int y) { POINT p = { x, y }; RECT r = chip_rect(); return PtInRect(&r, p); }

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
// The Mac's percussion pad colours by pitch class (percussionColorByPitchClass).
static const RGB8 DRUM_COLOR[12] = {
    {220, 90, 40}, {220,150,240}, {220,180,110}, {240,160,220}, {240,220,130}, {220,240,140},
    {200,150, 80}, {120,220,180}, {190,120, 70}, {120,200,240}, {230,210,170}, {180,180,230}
};
static const RGB8 FOCUS_BLUE = { 15, 107, 255 };
static const RGB8 STOP_RED = { 255, 31, 41 };

// Windows' accent colour, boosted the way the Mac boosts its system accent for
// the sharps in dark mode (saturation +0.55/+0.30, brightness +0.45/+0.20).
static RGB8 g_accent = { 150, 80, 255 };
static void read_accent(void) {
    HKEY k; DWORD v = 0, size = sizeof v, type;
    if (RegOpenKeyExW(HKEY_CURRENT_USER, L"Software\\Microsoft\\Windows\\DWM", 0, KEY_READ, &k) == ERROR_SUCCESS) {
        if (RegQueryValueExW(k, L"AccentColor", NULL, &type, (BYTE *)&v, &size) == ERROR_SUCCESS && type == REG_DWORD) {
            g_accent.r = v & 0xFF; g_accent.g = (v >> 8) & 0xFF; g_accent.b = (v >> 16) & 0xFF;   // ABGR
            RegCloseKey(k); return;
        }
        RegCloseKey(k);
    }
    DWORD c = 0; BOOL opaque = FALSE;
    if (SUCCEEDED(DwmGetColorizationColor(&c, &opaque))) { g_accent.r = (c >> 16) & 0xFF; g_accent.g = (c >> 8) & 0xFF; g_accent.b = c & 0xFF; }
}
static RGB8 hsv_boost(RGB8 c, double satBoost, double briBoost) {
    double r = c.r / 255.0, g = c.g / 255.0, b = c.b / 255.0;
    double mx = fmax(r, fmax(g, b)), mn = fmin(r, fmin(g, b)), d = mx - mn, h = 0;
    if (d > 1e-6) { if (mx == r) h = fmod((g - b) / d, 6); else if (mx == g) h = (b - r) / d + 2; else h = (r - g) / d + 4; h /= 6; if (h < 0) h += 1; }
    double sat = mx > 0 ? d / mx : 0, v = mx;
    sat = fmin(1, sat + (1 - sat) * satBoost); v = fmin(1, v + (1 - v) * briBoost);
    double C = v * sat, X = C * (1 - fabs(fmod(h * 6, 2) - 1)), m = v - C, rr = 0, gg = 0, bb = 0;
    int sec = (int)(h * 6) % 6;
    switch (sec) { case 0: rr = C; gg = X; break; case 1: rr = X; gg = C; break; case 2: gg = C; bb = X; break; case 3: gg = X; bb = C; break; case 4: rr = X; bb = C; break; default: rr = C; bb = X; }
    RGB8 o = { (uint8_t)((rr + m) * 255), (uint8_t)((gg + m) * 255), (uint8_t)((bb + m) * 255) }; return o;
}

static RGB8 blend(RGB8 a, RGB8 b, double t) { RGB8 c = { (uint8_t)(a.r + (b.r - a.r) * t), (uint8_t)(a.g + (b.g - a.g) * t), (uint8_t)(a.b + (b.b - a.b) * t) }; return c; }

static int g_clipOn = 0, g_clipL = 0, g_clipR = 0;
static void put(int x, int y, RGB8 c, double alpha) {
    if (x < 0 || y < 0 || x >= g_stripW || y >= g_stripH) return;
    if (g_clipOn && (x < g_clipL || x >= g_clipR)) return;
    uint32_t *p = &g_pixels[y * g_stripW + x];
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

// Text masks via GDI: draw white text on black into a scratch DIB, read the
// green channel back as coverage.
static uint8_t *text_mask(const wchar_t *text, const wchar_t *face, int px, int weight, int *outW, int *outH) {
    HDC dc = CreateCompatibleDC(NULL);
    HFONT font = CreateFontW(-px, 0, 0, 0, weight, 0, 0, 0, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, ANTIALIASED_QUALITY, DEFAULT_PITCH, face);
    HGDIOBJ oldFont = SelectObject(dc, font);
    SIZE sz; GetTextExtentPoint32W(dc, text, (int)wcslen(text), &sz);
    int w = sz.cx + 2, h = sz.cy + 2;
    BITMAPINFO bi; memset(&bi, 0, sizeof bi);
    bi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER); bi.bmiHeader.biWidth = w; bi.bmiHeader.biHeight = -h;
    bi.bmiHeader.biPlanes = 1; bi.bmiHeader.biBitCount = 32; bi.bmiHeader.biCompression = BI_RGB;
    uint32_t *bits = NULL; HBITMAP bm = CreateDIBSection(dc, &bi, DIB_RGB_COLORS, (void **)&bits, NULL, 0);
    if (!bm) { SelectObject(dc, oldFont); DeleteObject(font); DeleteDC(dc); *outW = *outH = 0; return NULL; }
    HGDIOBJ oldBm = SelectObject(dc, bm); SelectObject(dc, font);
    memset(bits, 0, (size_t)w * h * 4);
    SetBkMode(dc, TRANSPARENT); SetTextColor(dc, RGB(255, 255, 255));
    TextOutW(dc, 1, 1, text, (int)wcslen(text));
    uint8_t *mask = (uint8_t *)malloc((size_t)w * h);
    for (int i = 0; i < w * h; i++) mask[i] = (uint8_t)((bits[i] >> 8) & 0xFF);
    SelectObject(dc, oldBm); DeleteObject(bm); SelectObject(dc, oldFont); DeleteObject(font); DeleteDC(dc);
    *outW = w; *outH = h; return mask;
}

static void blit_mask(const uint8_t *mask, int mw, int mh, int x0, int y0, RGB8 c, double alpha) {
    if (!mask) return;
    for (int y = 0; y < mh; y++) for (int x = 0; x < mw; x++) { uint8_t m = mask[y * mw + x]; if (m) put(x0 + x, y0 + y, c, m / 255.0 * alpha); }
}

// Crop a mask to its ink so glyph placement works from what is visible,
// not from the font's em box (♪ sits in a small corner of its box).
static uint8_t *trim_mask(uint8_t *m, int w, int h, int *outW, int *outH) {
    if (!m) { *outW = *outH = 0; return NULL; }
    int x0 = w, y0 = h, x1 = -1, y1 = -1;
    for (int y = 0; y < h; y++) for (int x = 0; x < w; x++) if (m[y * w + x] > 8) { if (x < x0) x0 = x; if (x > x1) x1 = x; if (y < y0) y0 = y; if (y > y1) y1 = y; }
    if (x1 < 0) { *outW = *outH = 0; free(m); return NULL; }
    int tw = x1 - x0 + 1, th = y1 - y0 + 1;
    uint8_t *t = (uint8_t *)malloc((size_t)tw * th);
    for (int y = 0; y < th; y++) memcpy(t + y * tw, m + (y0 + y) * w + x0, tw);
    free(m); *outW = tw; *outH = th; return t;
}

static uint8_t *g_noteMask = NULL; static int g_noteW = 0, g_noteH = 0;
static uint8_t *g_numMask = NULL;  static int g_numW = 0, g_numH = 0;
static wchar_t g_numText[8] = L"";

static void render_chip_mask(void) {
    // Render once to learn the ink height, then again so the ink is ~88 % of the key height.
    int w0, h0; uint8_t *probe = trim_mask(text_mask(L"\x266A", L"Segoe UI Symbol", 64, FW_NORMAL, &w0, &h0), w0, h0, &w0, &h0);
    free(probe);
    int px = h0 > 0 ? (int)(64.0 * (g_keyH * 0.88) / h0) : g_keyH;
    free(g_noteMask); g_noteMask = trim_mask(text_mask(L"\x266A", L"Segoe UI Symbol", px, FW_NORMAL, &g_noteW, &g_noteH), g_noteW, g_noteH, &g_noteW, &g_noteH);
    free(g_numMask); g_numMask = NULL; g_numText[0] = 0;
}

static void ensure_number_mask(const wchar_t *text) {
    if (g_numMask && !wcscmp(text, g_numText)) return;
    free(g_numMask); g_numMask = trim_mask(text_mask(text, L"Segoe UI", pt(11.0), FW_HEAVY, &g_numW, &g_numH), g_numW, g_numH, &g_numW, &g_numH);
    wcscpy_s(g_numText, 8, text);
}

static wchar_t g_pendingDigits[4] = L"";   // instrument number being typed
static int g_slide = 0;                     // octave slide offset in px, eases to 0
static int g_hoverMidi = -1;
static int g_hoverChip = 0;
static int g_mouseMidi = -1;
static int g_typing = 0;

static void paint(void) {
    memset(g_pixels, 0, sizeof(uint32_t) * g_stripW * g_stripH);
    const RGB8 whiteHi = { 62, 72, 82 }, whiteLo = { 44, 54, 62 };
    const RGB8 groove = { 140, 155, 165 };
    // A near-grey accent (Windows' automatic pick from a dark wallpaper) gets
    // cool slate sharps instead of a warm smear; a real accent gets the Mac boost.
    double amx = fmax(g_accent.r, fmax(g_accent.g, g_accent.b)) / 255.0, amn = fmin(g_accent.r, fmin(g_accent.g, g_accent.b)) / 255.0;
    int neutral = amx < 0.02 || (amx - amn) / amx < 0.12;
    const RGB8 slateHi = { 176, 186, 196 }, slateLo = { 128, 138, 148 };
    const RGB8 blackHi = neutral ? slateHi : hsv_boost(g_accent, 0.55, 0.45), blackLo = neutral ? slateLo : hsv_boost(g_accent, 0.30, 0.20);
    const RGB8 black = { 0, 0, 0 }, white = { 255, 255, 255 };
    const RGB8 glyph = { 235, 238, 242 };
    int stripeH = pt(2.0);

    // ♪ chip: the note glyph with the program number as a heavy subscript, as
    // on the Mac; while digits are being typed, the subscript shows them lit.
    RECT cr = chip_rect();
    if (g_typing) { RECT pill = { cr.left, cr.top, cr.right, cr.bottom }; fill(pill, FOCUS_BLUE, blend(FOCUS_BLUE, black, 0.25), 0.9); }
    else if (g_hoverChip) { RECT pill = { cr.left, cr.top, cr.right, cr.bottom }; fill(pill, white, white, 0.10); }
    // ♪ as tall as the keys, the number hugging its lower right like a subscript.
    wchar_t num[8]; int pending = g_pendingDigits[0] != 0;
    if (pending) wcscpy_s(num, 8, g_pendingDigits); else swprintf(num, 8, L"%d", g_program + 1);
    ensure_number_mask(num);
    int gap = pt(1.0);
    int unitW = g_noteW + gap + g_numW;
    int gx = cr.left + (g_chipW - unitW) / 2; if (gx < cr.left) gx = cr.left;
    int gy = g_padY + (g_keyH - g_noteH) / 2;
    blit_mask(g_noteMask, g_noteW, g_noteH, gx, gy, glyph, 1.0);
    int nx = gx + g_noteW + gap, ny = g_padY + g_keyH - g_numH - pt(1.0);
    blit_mask(g_numMask, g_numW, g_numH, nx, ny, pending ? CHROMA[4] : glyph, 1.0);

    // Backplate: the groove colour shows between keys and as a hairline frame.
    RECT plate = { g_pianoX - 1, g_padY - 1, g_pianoX + g_keyW * WHITE_COUNT, g_padY + g_keyH + 1 };
    fill(plate, groove, groove, 0.55);

    // Keys: one tile normally; during an octave slide the run is drawn at the
    // slide offset with the neighbouring octave's tiles on either side, clipped
    // to the plate, so the piano reads as sliding past a window cut in the bar.
    g_clipOn = 1; g_clipL = g_pianoX; g_clipR = g_pianoX + g_keyW * WHITE_COUNT;
    int sliding = g_slide != 0;
    int tileW = g_keyW * WHITE_COUNT;
    for (int t = (sliding ? -1 : 0); t <= (sliding ? 1 : 0); t++) {
        int dx = g_slide + t * tileW, dm = t * 24;   // 14 whites = 2 octaves = 24 semitones
        for (int i = 0; i < WHITE_COUNT; i++) {
            int m = g_whiteMidi[i] + dm, mv = ((m % 12) + 12) % 12;
            RECT kr = white_rect(i); kr.left += dx; kr.right += dx;
            if (kr.right < g_clipL || kr.left >= g_clipR) continue;
            RGB8 hi = whiteHi, lo = whiteLo;
            int lit = m >= 0 && m < 128 && (g_lit[m] > 0 || g_latched[m]);
            int drums = g_page == PAGE_SKIN && g_typing;
            if (lit) { RGB8 l = drums ? blend(DRUM_COLOR[mv], white, 0.45) : blend(CHROMA[mv], black, 0.22); hi = l; lo = l; }
            else if (m == g_hoverMidi && !sliding) { hi = blend(whiteHi, white, 0.12); lo = blend(whiteLo, white, 0.12); }
            fill(kr, hi, lo, 1.0);
            if (drums && !lit) fill(kr, DRUM_COLOR[mv], DRUM_COLOR[mv], 0.50);
            RECT st = { kr.left + 1, kr.bottom - stripeH, kr.right - 1, kr.bottom };
            if (!lit && !drums) fill(st, CHROMA[mv], CHROMA[mv], 0.95);
        }
        for (int i = 0; i < g_blackCount; i++) {
            int m = g_blackMidi[i] + dm;
            RECT br = black_rect(g_blackMidi[i]); br.left += dx; br.right += dx;
            if (br.right < g_clipL || br.left >= g_clipR) continue;
            RGB8 hi = blackHi, lo = blackLo;
            if (g_page == PAGE_SKIN && g_typing) { int mv2 = ((m % 12) + 12) % 12; hi = blend(blackHi, DRUM_COLOR[mv2], 0.6); lo = blend(blackLo, DRUM_COLOR[mv2], 0.6); }
            if (m >= 0 && m < 128 && (g_lit[m] > 0 || g_latched[m])) { hi = white; lo = blend(white, blackHi, 0.4); }
            else if (m == g_hoverMidi && !sliding) { hi = blend(blackHi, white, 0.18); lo = blend(blackLo, white, 0.18); }
            fill(br, hi, lo, 1.0);
        }
    }
    g_clipOn = 0;
}

static int g_posX = 0, g_posY = 0;

static void push_frame(HWND hwnd) {
    paint();
    POINT dst = { g_posX, g_posY }, src = { 0, 0 };
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
    read_accent();
    render_chip_mask();
}

// ───────────────────── full-screen focus flash ───────────────────
// A click-through layered window over the primary monitor: blue when Menu
// Band takes the keyboard, red when it lets go. Peaks then fades in ~170 ms,
// like FocusFlashOverlay.swift.

static HWND g_flash = NULL;
static COLORREF g_flashColor = 0;
static double g_flashAlpha = 0;
#define IDT_FLASH 2

static LRESULT CALLBACK flashproc(HWND hwnd, UINT msg, WPARAM wp, LPARAM lp) {
    if (msg == WM_ERASEBKGND) {
        RECT r; GetClientRect(hwnd, &r);
        HBRUSH b = CreateSolidBrush(g_flashColor);
        FillRect((HDC)wp, &r, b); DeleteObject(b);
        return 1;
    }
    if (msg == WM_TIMER && wp == IDT_FLASH) {
        g_flashAlpha -= 0.35 * (16.0 / 170.0);
        if (g_flashAlpha <= 0.01) { KillTimer(hwnd, IDT_FLASH); ShowWindow(hwnd, SW_HIDE); return 0; }
        SetLayeredWindowAttributes(hwnd, 0, (BYTE)(g_flashAlpha * 255), LWA_ALPHA);
        return 0;
    }
    return DefWindowProcW(hwnd, msg, wp, lp);
}

static void flash(RGB8 c) {
    if (!g_flash) return;
    g_flashColor = RGB(c.r, c.g, c.b);
    g_flashAlpha = 0.35;
    // Inset by a pixel: a topmost window that exactly covers the monitor reads
    // as a full-screen app to the shell (QUNS_BUSY), and the strip would hide
    // itself for the length of its own flash.
    int w = GetSystemMetrics(SM_CXSCREEN), h = GetSystemMetrics(SM_CYSCREEN);
    SetLayeredWindowAttributes(g_flash, 0, (BYTE)(g_flashAlpha * 255), LWA_ALPHA);
    SetWindowPos(g_flash, HWND_TOPMOST, 1, 1, w - 2, h - 2, SWP_NOACTIVATE | SWP_SHOWWINDOW);
    InvalidateRect(g_flash, NULL, TRUE);
    SetTimer(g_flash, IDT_FLASH, 16, NULL);
}

// ────────────────────── typing (Notepat keymap) ──────────────────
// Semitone from middle C per virtual key; mirrors MenuBandLayout's table
// (and NOTE_TO_KEYBOARD_KEY in notepat.mjs).

static int g_semitoneByVk[256];
static int g_vkDown[256];
static HHOOK g_hook = NULL;
static HWND g_strip = NULL;
#define WM_TYPEKEY (WM_APP + 2)
#define WM_TYPEESC (WM_APP + 3)
#define WM_OCTAVE (WM_APP + 4)
#define WM_DIGIT (WM_APP + 5)
#define IDT_DIGITS 4

static void build_keymap(void) {
    for (int i = 0; i < 256; i++) g_semitoneByVk[i] = -128;
    const struct { int vk; int st; } map[] = {
        {'Z', -2}, {'X', -1}, {'C', 0}, {'V', 1}, {'D', 2}, {'S', 3}, {'E', 4}, {'F', 5}, {'W', 6},
        {'G', 7}, {'R', 8}, {'A', 9}, {'Q', 10}, {'B', 11}, {'H', 12}, {'T', 13}, {'I', 14}, {'Y', 15},
        {'J', 16}, {'K', 17}, {'U', 18}, {'L', 19}, {'O', 20}, {'M', 21}, {'P', 22}, {'N', 23},
        {VK_OEM_1, 24}, {VK_OEM_7, 25},
    };
    for (size_t i = 0; i < sizeof map / sizeof map[0]; i++) g_semitoneByVk[map[i].vk] = map[i].st;
}

#define WM_CTRLTRIPLE (WM_APP + 6)
#define WM_PEDAL (WM_APP + 7)
#define WM_SURF (WM_APP + 8)
#define WM_LATCH (WM_APP + 9)
#define WM_CHORDMOD (WM_APP + 11)
#define WM_PAGE (WM_APP + 13)
#define WM_UNLATCH (WM_APP + 10)
static DWORD g_ctrlTapTimes[3]; static int g_ctrlTaps = 0; static int g_ctrlSolo = 0;

static LRESULT CALLBACK ll_keyboard(int code, WPARAM wp, LPARAM lp) {
    if (code == HC_ACTION) {
        KBDLLHOOKSTRUCT *k = (KBDLLHOOKSTRUCT *)lp;
        int down = (wp == WM_KEYDOWN || wp == WM_SYSKEYDOWN);
        int isCtrl = (k->vkCode == VK_LCONTROL || k->vkCode == VK_RCONTROL);
        // Fn-free media keys while armed (MiniBook X: the bare keys arrive as
        // F3/F4/F5; with Fn they send VK_VOLUME_*). Only while Menu Band holds the
        // keyboard, so the rest of Windows keeps its F-keys.
        if (g_fnFree && g_typing && !(k->flags & LLKHF_INJECTED) && (k->vkCode == VK_F3 || k->vkCode == VK_F4 || k->vkCode == VK_F5)
            && !(GetAsyncKeyState(VK_CONTROL) & 0x8000) && !(GetAsyncKeyState(VK_MENU) & 0x8000) && !(GetAsyncKeyState(VK_LWIN) & 0x8000) && !(GetAsyncKeyState(VK_SHIFT) & 0x8000)) {
            if (down) {
                WORD media = k->vkCode == VK_F3 ? VK_VOLUME_DOWN : k->vkCode == VK_F4 ? VK_VOLUME_MUTE : VK_VOLUME_UP;   // the MiniBook X legend: F3 ↓, F4 mute, F5 ↑
                INPUT in[2]; memset(in, 0, sizeof in);
                in[0].type = INPUT_KEYBOARD; in[0].ki.wVk = media;
                in[1].type = INPUT_KEYBOARD; in[1].ki.wVk = media; in[1].ki.dwFlags = KEYEVENTF_KEYUP;
                SendInput(2, in, sizeof(INPUT));
            }
            return 1;
        }
        // Ctrl ×2: two solo taps (down, up, nothing else between) inside 450 ms.
        if (isCtrl) {
            if (down) g_ctrlSolo = 1;
            else if (g_ctrlSolo) {
                DWORD now = GetTickCount();
                if (g_ctrlTaps && now - g_ctrlTapTimes[0] > 450) g_ctrlTaps = 0;
                if (g_ctrlTaps < 3) g_ctrlTapTimes[g_ctrlTaps++] = now;
                if (g_ctrlTaps == 2) { g_ctrlTaps = 0; PostMessageW(g_strip, WM_CTRLTRIPLE, 0, 0); }
                g_ctrlSolo = 0;
            }
        } else { g_ctrlSolo = 0; if (down) g_ctrlTaps = 0; }
    }
    if (code == HC_ACTION && g_typing) {
        KBDLLHOOKSTRUCT *k = (KBDLLHOOKSTRUCT *)lp;
        int down = (wp == WM_KEYDOWN || wp == WM_SYSKEYDOWN);
        if (k->vkCode == VK_LSHIFT || k->vkCode == VK_RSHIFT || k->vkCode == VK_CAPITAL) { PostMessageW(g_strip, WM_PEDAL, 0, 0); return CallNextHookEx(g_hook, code, wp, lp); }
        if (k->vkCode == VK_TAB) { if (down) PostMessageW(g_strip, WM_PAGE, (GetAsyncKeyState(VK_SHIFT) & 0x8000) ? 1 : 0, 0); return 1; }
        if (k->vkCode == VK_RETURN) { PostMessageW(g_strip, WM_LATCH, down, 0); return 1; }
        if (k->vkCode == VK_BACK) { if (down && !(k->flags & LLKHF_UP)) PostMessageW(g_strip, WM_UNLATCH, (GetAsyncKeyState(VK_SHIFT) & 0x8000) ? 1 : 0, 0); return 1; }
        if (k->vkCode == VK_LEFT || k->vkCode == VK_RIGHT || k->vkCode == VK_UP || k->vkCode == VK_DOWN) {
            if (down) PostMessageW(g_strip, WM_SURF, k->vkCode == VK_RIGHT ? 1 : k->vkCode == VK_LEFT ? (WPARAM)-1 : k->vkCode == VK_UP ? 8 : (WPARAM)-8, 0);
            return 1;
        }
        if (k->vkCode < 256) {
            if (k->vkCode == VK_ESCAPE) { if (down) PostMessageW(g_strip, WM_TYPEESC, 0, 0); return 1; }
            if (k->vkCode >= '0' && k->vkCode <= '9') { if (down) PostMessageW(g_strip, WM_DIGIT, k->vkCode - '0', 0); return 1; }
            if (k->vkCode == VK_OEM_COMMA || k->vkCode == VK_OEM_PERIOD) {
                if (down) PostMessageW(g_strip, WM_OCTAVE, k->vkCode == VK_OEM_PERIOD ? 1 : (WPARAM)-1, 0);
                return 1;
            }
            if (k->vkCode == VK_LCONTROL || k->vkCode == VK_RCONTROL || k->vkCode == VK_LMENU || k->vkCode == VK_RMENU) {
                PostMessageW(g_strip, WM_CHORDMOD, 0, 0);
                return CallNextHookEx(g_hook, code, wp, lp);
            }
            // Mapped note keys: Ctrl = major, Alt = minor, Ctrl+Alt = sus4 — the
            // Mac's ⌘ / ⌥ / ⌘⌥ chords — and they shadow Cut/Copy etc. while armed.
            if (g_semitoneByVk[k->vkCode] != -128 && !(GetAsyncKeyState(VK_LWIN) & 0x8000) && !(GetAsyncKeyState(VK_RWIN) & 0x8000)) {
                PostMessageW(g_strip, WM_TYPEKEY, k->vkCode, down);
                return 1;
            }
        }
    }
    return CallNextHookEx(g_hook, code, wp, lp);
}

static int g_sustained[128];       // note released while its pedal was down; sounds until pedal-up
static int g_latchOrder[128], g_latchCount = 0;   // Enter-latch order
static int pedal_holds(int midi) {
    int caps = (GetKeyState(VK_CAPITAL) & 1) != 0;
    int lshift = (GetAsyncKeyState(VK_LSHIFT) & 0x8000) != 0, rshift = (GetAsyncKeyState(VK_RSHIFT) & 0x8000) != 0;
    if (caps || (lshift && rshift)) return 1;
    int split = g_firstMidi + 12;
    return (lshift && midi < split) || (rshift && midi >= split);
}
static uint64_t g_keyGroup[128];
static void lit_on(HWND hwnd, int midi, int vel) {
    if (midi < 0 || midi > 127) return;
    if (g_page == PAGE_SKIN && g_typing) {
        g_keyGroup[midi] = ++g_touchGroup;
        double pan = ((midi - g_firstMidi) / 24.0 - 0.5) * 0.8;
        perc_hit_drum((PercDrum)(((midi % 12) + 12) % 12), vel / 127.0, pan, g_keyGroup[midi]);
        g_lit[midi]++; push_frame(hwnd); return;
    }
    if (g_sustained[midi] || g_latched[midi]) { g_sustained[midi] = 0; g_latched[midi] = 0; play_off(midi); g_lit[midi] = 0; }   // retrigger a held note
    if (g_lit[midi]++ == 0) play_on(midi, vel);
    push_frame(hwnd);
}
static void lit_off(HWND hwnd, int midi) {
    if (midi < 0 || midi > 127 || g_lit[midi] == 0) return;
    if (g_keyGroup[midi]) { perc_release(g_keyGroup[midi]); g_keyGroup[midi] = 0; if (--g_lit[midi] < 0) g_lit[midi] = 0; push_frame(hwnd); return; }
    if (--g_lit[midi] == 0) { if (g_enterLatch) latch_note(midi); else if (g_latched[midi]) { } else if (pedal_holds(midi)) g_sustained[midi] = 1; else play_off(midi); }
    push_frame(hwnd);
}
// Enter-latch: while Return is held, a note you release stays on until
// Backspace pops it (newest first) or Shift+Backspace clears them all.
static void latch_note(int midi) {
    if (g_latched[midi]) return;
    g_latched[midi] = 1; if (g_latchCount < 128) g_latchOrder[g_latchCount++] = midi;
}
static void unlatch(HWND hwnd, int all) {
    while (g_latchCount > 0) {
        int m = g_latchOrder[--g_latchCount];
        if (g_latched[m]) { g_latched[m] = 0; if (g_lit[m] == 0 && !g_sustained[m]) play_off(m); }
        if (!all) break;
    }
    push_frame(hwnd);
}
// Chord quality for a root from the modifiers held on that root's side of the
// split (left Ctrl/Alt for the lower octave, right for the upper), so one hand
// can hold a triad while the other rings single notes. Intervals above the
// root, root excluded, as in MenuBandController.chordIntervals.
static int chord_quality(int midi) {
    int left = midi < g_firstMidi + 12;
    int ctrl = (GetAsyncKeyState(left ? VK_LCONTROL : VK_RCONTROL) & 0x8000) != 0;
    int alt = (GetAsyncKeyState(left ? VK_LMENU : VK_RMENU) & 0x8000) != 0;
    if (ctrl && alt) return 6;   // sus4
    if (alt) return 2;           // minor
    if (ctrl) return 1;          // major
    return 0;
}
static int chord_intervals(int quality, int *iv) {
    switch (quality) {
    case 1: iv[0] = 4; iv[1] = 7; return 2;
    case 2: iv[0] = 3; iv[1] = 7; return 2;
    case 3: iv[0] = 2; iv[1] = 7; return 2;
    case 4: iv[0] = 4; iv[1] = 8; return 2;
    case 5: iv[0] = 3; iv[1] = 6; return 2;
    case 6: iv[0] = 5; iv[1] = 7; return 2;
    default: return 0;
    }
}
// Re-voice a held key's extensions to the current modifiers, touching only
// the tones that change (the shared fifth stays put).
static void revoice(HWND hwnd, int vk) {
    int root = g_vkRoot[vk], iv[2], n = chord_intervals(chord_quality(root), iv), want[2] = { -1, -1 };
    for (int i = 0; i < n; i++) want[i] = root + iv[i];
    for (int i = 0; i < 2; i++) { int m = g_vkExt[vk][i]; if (m >= 0 && m != want[0] && m != want[1]) { lit_off(hwnd, m); g_vkExt[vk][i] = -1; } }
    for (int i = 0; i < 2; i++) { int m = want[i]; if (m < 0 || m == g_vkExt[vk][0] || m == g_vkExt[vk][1]) continue; int slot = g_vkExt[vk][0] < 0 ? 0 : 1; g_vkExt[vk][slot] = m; lit_on(hwnd, m, 92); }
}
static void pedal_release(HWND hwnd) {
    int changed = 0;
    for (int m = 0; m < 128; m++) if (g_sustained[m] && !pedal_holds(m)) { g_sustained[m] = 0; play_off(m); changed = 1; }
    if (changed) push_frame(hwnd);
}

static void release_typed(HWND hwnd) {
    for (int vk = 0; vk < 256; vk++) if (g_vkDown[vk]) {
        g_vkDown[vk] = 0;
        for (int i = 0; i < 2; i++) if (g_vkExt[vk][i] >= 0) { lit_off(hwnd, g_vkExt[vk][i]); g_vkExt[vk][i] = -1; }
        lit_off(hwnd, g_vkRoot[vk]);
    }
}

// Little ear-cue on focus change, like the Mac: up a fifth on arm, down on Esc.
#define IDT_CUE 5
static int g_cueNotes[2], g_cueIndex = 0, g_cueSounding = -1;
#define CUE_PROGRAM 9   // glockenspiel: reads as interface, not as the instrument you're playing
static void cue_note(int midi, int on) {
    if (g_midiOut) { midiOutShortMsg(g_midiOut, (on ? 0x99 : 0x89) | ((DWORD)midi << 8) | (on ? 0x5A0000 : 0)); return; }   // channel 10 percussion slot on a DAW
    if (on) { g_seedCounter = g_seedCounter * 1664525u + 1013904223u; Cmd c = { CMD_ON, (uint8_t)midi, 96, CUE_PROGRAM, g_seedCounter }; synth_push(c); }
    else { Cmd c = { CMD_OFF, (uint8_t)midi, 0, 0, 0 }; synth_push(c); }
}
static void step_cue(HWND hwnd) {
    if (g_cueSounding >= 0) { cue_note(g_cueSounding, 0); g_cueSounding = -1; }
    if (g_cueIndex >= 2) { KillTimer(hwnd, IDT_CUE); return; }
    g_cueSounding = g_cueNotes[g_cueIndex++]; cue_note(g_cueSounding, 1);
}
static void cue(HWND hwnd, int a, int b) { g_cueNotes[0] = a; g_cueNotes[1] = b; g_cueIndex = 0; SetTimer(hwnd, IDT_CUE, 150, NULL); step_cue(hwnd); }

// Pointer hidden while armed, as on the Mac: swap every system cursor for a
// blank one; SPI_SETCURSORS reloads the user's set on release, quit or crash.
static int g_cursorHidden = 0;
static void cursor_hide(void) {
    if (g_cursorHidden) return;
    static const int ids[] = { 32512, 32513, 32514, 32515, 32516, 32642, 32643, 32644, 32645, 32646, 32648, 32649, 32650, 32651 };
    BYTE andMask[32 * 4], xorMask[32 * 4]; memset(andMask, 0xFF, sizeof andMask); memset(xorMask, 0x00, sizeof xorMask);
    for (size_t i = 0; i < sizeof ids / sizeof ids[0]; i++) {
        HCURSOR blank = CreateCursor(GetModuleHandleW(NULL), 0, 0, 32, 32, andMask, xorMask);
        if (blank) SetSystemCursor(blank, (DWORD)ids[i]);   // SetSystemCursor consumes the handle
    }
    g_cursorHidden = 1;
}
static void cursor_show(void) {
    if (!g_cursorHidden) return;
    SystemParametersInfoW(SPI_SETCURSORS, 0, NULL, 0);
    g_cursorHidden = 0;
}

static void typing_set(HWND hwnd, int on) {
    if (on == g_typing) return;
    if (on && g_page != PAGE_MOUSE) cursor_hide(); else cursor_show();
    if (on && page_takes_pad()) { g_puckAlpha = 1; g_lastTouch = GetTickCount(); SetTimer(hwnd, IDT_FX, 16, NULL); }
    if (!on) { for (int i = 0; i < MAX_CONTACTS; i++) if (g_contacts[i].down && g_contacts[i].struck) { perc_release(g_contacts[i].group); g_contacts[i].struck = 0; } }
    g_typing = on;
    if (!on) { release_typed(hwnd); g_enterLatch = 0; unlatch(hwnd, 1); surface_reset(hwnd); }
    g_pendingDigits[0] = 0;
    audio_set_exclusive(on);
    flash(on ? FOCUS_BLUE : STOP_RED);
    cue(hwnd, on ? 84 : 79, on ? 91 : 72);
    push_frame(hwnd);
}

// ──────────────────────── trackpad pages (Tab / ⇧Tab) ────────────────────────
// As on the Mac: Tab cycles slide → drum pad → pointer → slide; ⇧Tab cycles
// split → drum pad → pointer → split. The drum pad (TrackDrum) is the page
// Menu Band lands on when armed.
static const wchar_t *PAGE_NAMES[4] = { L"slide", L"drum pad", L"pointer", L"split" };
static int page_after_tab(int p) { return p == PAGE_FX ? PAGE_SKIN : p == PAGE_SKIN ? PAGE_MOUSE : PAGE_FX; }
static int page_after_shift_tab(int p) { return p == PAGE_SPLIT ? PAGE_SKIN : p == PAGE_SKIN ? PAGE_MOUSE : PAGE_SPLIT; }

// ──────────────── the FX surface (pointer while armed) + puck overlay ────────────────
// Relative, like the Mac's trackpad page: pointer deltas move bend (vertical,
// 80 px per octave, ±2 octaves) and the fx axis (horizontal: right = echo,
// left = space). Values hold while anything sounds or the pointer keeps
// moving; 0.4 s after the last move with nothing sounding they ease home over
// 0.48 s. Moves over the strip itself don't count (that's for clicking keys).
#define WM_SURFACE (WM_APP + 12)
static float g_bendUnits = 0, g_fxX = 0;           // live values (octaves; -1…1)
static DWORD g_lastSurfaceMove = 0;
static int g_fxRamping = 0; static float g_rampFromBend = 0, g_rampFromX = 0; static DWORD g_rampStart = 0;
static HWND g_puck = NULL; static uint32_t *g_puckPixels = NULL; static HBITMAP g_puckDib = NULL; static HDC g_puckDC = NULL; static int g_puckSize = 0;
static HHOOK g_mouseHook = NULL;
static POINT g_lastPt = { -1, -1 };

static LRESULT CALLBACK ll_mouse(int code, WPARAM wp, LPARAM lp) {
    if (code == HC_ACTION && g_typing && page_takes_pad()) {
        MSLLHOOKSTRUCT *m = (MSLLHOOKSTRUCT *)lp;
        RECT strip = { g_posX, g_posY, g_posX + g_stripW, g_posY + g_stripH };
        if (!PtInRect(&strip, m->pt) && wp != WM_MOUSEWHEEL) return 1;
    }
    if (code == HC_ACTION && g_typing && wp == WM_MOUSEMOVE) {
        MSLLHOOKSTRUCT *m = (MSLLHOOKSTRUCT *)lp;
        if (g_lastPt.x >= 0) {
            int dx = m->pt.x - g_lastPt.x, dy = m->pt.y - g_lastPt.y;
            if ((dx || dy) && dx > -32000 && dx < 32000 && dy > -32000 && dy < 32000)
                PostMessageW(g_strip, WM_SURFACE, (WPARAM)(short)dx, (LPARAM)(short)dy);
        }
        g_lastPt = m->pt;
    } else if (code == HC_ACTION && wp == WM_MOUSEMOVE) { g_lastPt = ((MSLLHOOKSTRUCT *)lp)->pt; }
    return CallNextHookEx(g_mouseHook, code, wp, lp);
}

// Precision-touchpad contacts through Raw Input: absolute finger positions,
// independent of the pointer. Each down contact strikes the membrane once at
// landing; resting contacts are anchors; lifting articulates and releases.
static PHIDP_PREPARSED_DATA g_padPreparsed = NULL; static HANDLE g_padDevice = NULL;
static HIDP_VALUE_CAPS g_padValueCaps[64]; static USHORT g_padValueCount = 0;
static int g_padReportsSeen = 0;

static void touch_register(HWND hwnd) {
    RAWINPUTDEVICE rid = { HID_USAGE_PAGE_DIGITIZER, HID_USAGE_DIGITIZER_TOUCH_PAD, RIDEV_INPUTSINK, hwnd };
    if (!RegisterRawInputDevices(&rid, 1, sizeof rid)) mb_log(L"touchpad: RegisterRawInputDevices failed %lu", GetLastError());
}

static int pad_prepare(HANDLE dev) {
    if (dev == g_padDevice && g_padPreparsed) return 1;
    UINT size = 0; GetRawInputDeviceInfoW(dev, RIDI_PREPARSEDDATA, NULL, &size);
    if (!size) return 0;
    free(g_padPreparsed); g_padPreparsed = (PHIDP_PREPARSED_DATA)malloc(size);
    if (GetRawInputDeviceInfoW(dev, RIDI_PREPARSEDDATA, g_padPreparsed, &size) == (UINT)-1) { free(g_padPreparsed); g_padPreparsed = NULL; return 0; }
    g_padDevice = dev;
    g_padValueCount = 64;
    if (HidP_GetValueCaps(HidP_Input, g_padValueCaps, &g_padValueCount, g_padPreparsed) != HIDP_STATUS_SUCCESS) g_padValueCount = 0;
    mb_log(L"touchpad: device %p, %u value caps", dev, g_padValueCount);
    return 1;
}

static Contact *contact_slot(int id) {
    for (int i = 0; i < MAX_CONTACTS; i++) if (g_contacts[i].down && g_contacts[i].id == id) return &g_contacts[i];
    for (int i = 0; i < MAX_CONTACTS; i++) if (!g_contacts[i].down && g_contacts[i].id == id) { g_contacts[i].struck = 0; return &g_contacts[i]; }
    for (int i = 0; i < MAX_CONTACTS; i++) if (!g_contacts[i].down) { g_contacts[i].id = id; g_contacts[i].struck = 0; g_contacts[i].liftedAt = 0; return &g_contacts[i]; }
    return NULL;
}

static void pad_contact(HWND hwnd, int id, int tip, double x, double y);

static void touch_input(HWND hwnd, HRAWINPUT h) {
    UINT size = 0; GetRawInputData(h, RID_INPUT, NULL, &size, sizeof(RAWINPUTHEADER));
    if (!size || size > 65536) return;
    static BYTE buf[65536]; if (GetRawInputData(h, RID_INPUT, buf, &size, sizeof(RAWINPUTHEADER)) != size) return;
    RAWINPUT *ri = (RAWINPUT *)buf;
    if (ri->header.dwType != RIM_TYPEHID) return;
    if (!pad_prepare(ri->header.hDevice)) return;
    for (DWORD r = 0; r < ri->data.hid.dwCount; r++) {
        PCHAR report = (PCHAR)(ri->data.hid.bRawData + r * ri->data.hid.dwSizeHid); ULONG len = ri->data.hid.dwSizeHid;
        g_padReportsSeen++;
        int seen[32]; int nseen = 0;
        struct { int id; double x, y; } tipped[MAX_CONTACTS]; int ntip = 0;
        for (USHORT c = 0; c < g_padValueCount; c++) {
            HIDP_VALUE_CAPS *vc = &g_padValueCaps[c];
            if (vc->UsagePage != HID_USAGE_PAGE_GENERIC || (vc->IsRange ? vc->Range.UsageMin : vc->NotRange.Usage) != HID_USAGE_GENERIC_X) continue;
            USHORT link = vc->LinkCollection; int dup = 0; for (int k = 0; k < nseen; k++) if (seen[k] == link) dup = 1; if (dup || nseen >= 32) continue; seen[nseen++] = link;
            ULONG xv = 0, yv = 0, idv = 0;
            if (HidP_GetUsageValue(HidP_Input, HID_USAGE_PAGE_GENERIC, link, HID_USAGE_GENERIC_X, &xv, g_padPreparsed, report, len) != HIDP_STATUS_SUCCESS) continue;
            if (HidP_GetUsageValue(HidP_Input, HID_USAGE_PAGE_GENERIC, link, HID_USAGE_GENERIC_Y, &yv, g_padPreparsed, report, len) != HIDP_STATUS_SUCCESS) continue;
            HidP_GetUsageValue(HidP_Input, HID_USAGE_PAGE_DIGITIZER, link, HID_USAGE_DIGITIZER_CONTACT_ID, &idv, g_padPreparsed, report, len);
            USAGE usages[8]; ULONG nu = 8; int tip = 0;
            if (HidP_GetUsages(HidP_Input, HID_USAGE_PAGE_DIGITIZER, link, usages, &nu, g_padPreparsed, report, len) == HIDP_STATUS_SUCCESS)
                for (ULONG u = 0; u < nu; u++) if (usages[u] == HID_USAGE_DIGITIZER_TIP_SWITCH) tip = 1;
            double xmin = vc->LogicalMin, xmax = vc->LogicalMax, ymin = 0, ymax = 1;
            for (USHORT k = 0; k < g_padValueCount; k++) { HIDP_VALUE_CAPS *yc = &g_padValueCaps[k]; if (yc->LinkCollection == link && yc->UsagePage == HID_USAGE_PAGE_GENERIC && (yc->IsRange ? yc->Range.UsageMin : yc->NotRange.Usage) == HID_USAGE_GENERIC_Y) { ymin = yc->LogicalMin; ymax = yc->LogicalMax; } }
            double x = xmax > xmin ? (xv - xmin) / (xmax - xmin) : 0, y = ymax > ymin ? (yv - ymin) / (ymax - ymin) : 0;
            if (tip && ntip < MAX_CONTACTS) { tipped[ntip].id = (int)idv; tipped[ntip].x = x; tipped[ntip].y = y; ntip++; }
        }
        // Per report, the fingers with the tip down are the truth: new ids land,
        // known ids move, ids no longer present lift. Slots the pad pads out
        // with stale data never count as lifts.
        for (int i = 0; i < MAX_CONTACTS; i++) if (g_contacts[i].down) {
            int still = 0; for (int t = 0; t < ntip; t++) if (tipped[t].id == g_contacts[i].id) still = 1;
            if (!still) pad_contact(hwnd, g_contacts[i].id, 0, g_contacts[i].x, g_contacts[i].y);
        }
        for (int t = 0; t < ntip; t++) pad_contact(hwnd, tipped[t].id, 1, tipped[t].x, tipped[t].y);
    }
}

static void apply_fx(void) {
    g_bendTarget = g_bendUnits;
    g_echoTarget = g_fxX > 0 ? g_fxX : 0;
    g_spaceTarget = g_fxX < 0 ? -g_fxX : 0;
}

static int anything_sounding(void) {
    for (int i = 0; i < 128; i++) if (g_lit[i] || g_sustained[i] || g_latched[i]) return 1;
    return 0;
}

static void puck_put(int x, int y, RGB8 c, double a) {
    if (x < 0 || y < 0 || x >= g_puckSize || y >= g_puckSize) return;
    uint32_t *p = &g_puckPixels[y * g_puckSize + x];
    uint8_t db = *p & 0xFF, dg = (*p >> 8) & 0xFF, dr = (*p >> 16) & 0xFF, da = *p >> 24;
    *p = ((uint8_t)(255 * a + da * (1 - a)) << 24) | ((uint8_t)(c.r * a + dr * (1 - a)) << 16) | ((uint8_t)(c.g * a + dg * (1 - a)) << 8) | (uint8_t)(c.b * a + db * (1 - a));
}

static void puck_paint(void) {
    int S = g_puckSize; memset(g_puckPixels, 0, sizeof(uint32_t) * S * S);
    const RGB8 panel = { 18, 22, 30 }, grid = { 140, 155, 165 }, white = { 255, 255, 255 }, echoC = { 255, 160, 0 }, spaceC = { 90, 220, 230 };
    int r = pt(6.0);
    for (int y = 0; y < S; y++) for (int x = 0; x < S; x++) {
        double cx = x < r ? r - 0.5 - x : (x >= S - r ? x - (S - r - 0.5) : 0), cy = y < r ? r - 0.5 - y : (y >= S - r ? y - (S - r - 0.5) : 0);
        if (cx * cx + cy * cy > (double)r * r) continue;
        puck_put(x, y, panel, 0.78);
    }
    for (int k = 1; k < 4; k++) { int g = S * k / 4; for (int i = r; i < S - r; i++) { puck_put(g, i, grid, 0.18); puck_put(i, g, grid, 0.18); } }
    for (int i = r; i < S - r; i++) { puck_put(S / 2, i, grid, 0.45); puck_put(i, S / 2, grid, 0.45); }
    // the fx axis tint: right half warms with echo, left half cools with space
    if (g_fxX > 0.01f) { RECT a = { S / 2, r, S - r, S - r }; for (int y = a.top; y < a.bottom; y++) for (int x = a.left; x < a.right; x++) puck_put(x, y, echoC, 0.10 * g_fxX); }
    if (g_fxX < -0.01f) { RECT a = { r, r, S / 2, S - r }; for (int y = a.top; y < a.bottom; y++) for (int x = a.left; x < a.right; x++) puck_put(x, y, spaceC, 0.10 * -g_fxX); }
    // puck
    double px = S / 2.0 + (g_fxX * (S / 2.0 - r - 2)), py = S / 2.0 - (g_bendUnits / 2.0) * (S / 2.0 - r - 2);
    int pr = pt(6.0);
    RGB8 pc = g_fxX > 0.01f ? echoC : (g_fxX < -0.01f ? spaceC : white);
    for (int y = (int)py - pr - 1; y <= (int)py + pr + 1; y++) for (int x = (int)px - pr - 1; x <= (int)px + pr + 1; x++) {
        double d = sqrt((x + 0.5 - px) * (x + 0.5 - px) + (y + 0.5 - py) * (y + 0.5 - py));
        if (d < pr) puck_put(x, y, pc, 0.95); else if (d < pr + 1.5) puck_put(x, y, pc, 0.95 * (pr + 1.5 - d) / 1.5);
    }
}

static void puck_show(void) {
    if (!g_puck) return;
    if (!g_puckDC || !g_puckPixels) {
        g_puckSize = pt(120.0);
        g_puckDC = CreateCompatibleDC(NULL);
        BITMAPINFO bi; memset(&bi, 0, sizeof bi);
        bi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER); bi.bmiHeader.biWidth = g_puckSize; bi.bmiHeader.biHeight = -g_puckSize; bi.bmiHeader.biPlanes = 1; bi.bmiHeader.biBitCount = 32; bi.bmiHeader.biCompression = BI_RGB;
        g_puckDib = CreateDIBSection(g_puckDC, &bi, DIB_RGB_COLORS, (void **)&g_puckPixels, NULL, 0); SelectObject(g_puckDC, g_puckDib);
    }
    if (page_takes_pad()) pad_paint(); else puck_paint();
    POINT dst = { g_posX + g_pianoX, g_posY - g_puckSize - pt(8.0) }, src = { 0, 0 }; SIZE size = { g_puckSize, g_puckSize };
    BLENDFUNCTION bf = { AC_SRC_OVER, 0, (BYTE)(g_puckAlpha * 255), AC_SRC_ALPHA };
    HDC screen = GetDC(NULL); UpdateLayeredWindow(g_puck, screen, &dst, &size, g_puckDC, &src, 0, &bf, ULW_ALPHA); ReleaseDC(NULL, screen);
    SetWindowPos(g_puck, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE | (g_puckAlpha > 0.01f ? SWP_SHOWWINDOW : SWP_HIDEWINDOW));
}

static PercDrum g_lastPulse = PERC_KICK; static DWORD g_lastPulseAt = 0;
static void pad_contact(HWND hwnd, int id, int tip, double x, double y) {
    if (!g_typing || !page_takes_pad()) return;
    Contact *c = contact_slot(id); if (!c) return;
    int onPad = 1;
    if (g_page == PAGE_SPLIT) { if (x < 0.5) onPad = 0; }
    if (tip && !c->down) {
        DWORD now = GetTickCount();
        if (now - c->liftedAt < 40 && c->liftedAt) { c->down = 1; c->x = x; c->y = y; return; }   // debounce a flicker
        c->down = 1; c->x = x; c->y = y; c->group = ++g_touchGroup;
        if (onPad) {
            double anchors[2 * MAX_CONTACTS]; int n = 0;
            for (int i = 0; i < MAX_CONTACTS; i++) if (g_contacts[i].down && &g_contacts[i] != c && g_contacts[i].struck) { anchors[2 * n] = g_contacts[i].x; anchors[2 * n + 1] = g_contacts[i].y; n++; }
            g_lastPulse = perc_strike_pad(x, y, 0.85, anchors, n, c->group); g_lastPulseAt = GetTickCount();
            c->struck = 1;
            mb_log(L"pad: strike id %d at %.2f,%.2f -> %S (%d anchors)", id, x, y, perc_label(g_lastPulse), n);
        }
        g_lastTouch = GetTickCount(); g_puckAlpha = 1; SetTimer(hwnd, IDT_FX, 16, NULL); puck_show();
    } else if (tip && c->down) {
        if (g_page == PAGE_SPLIT && !onPad) {
            float dx = (float)((x - c->x) * 2.0), dy = (float)(y - c->y);
            g_bendUnits -= dy * 4.0f; if (g_bendUnits > 2) g_bendUnits = 2; if (g_bendUnits < -2) g_bendUnits = -2;
            g_fxX += dx; if (g_fxX > 1) g_fxX = 1; if (g_fxX < -1) g_fxX = -1;
            apply_fx(); g_lastSurfaceMove = GetTickCount();
        }
        c->x = x; c->y = y; g_lastTouch = GetTickCount();
    } else if (!tip && c->down) {
        if (c->struck) { perc_lift_pad(c->x, c->y, c->group); perc_release(c->group); }
        c->down = 0; c->struck = 0; c->liftedAt = GetTickCount(); g_lastTouch = c->liftedAt;
    }
    puck_show();
}

static void pad_paint(void) {
    int S = g_puckSize; memset(g_puckPixels, 0, sizeof(uint32_t) * S * S);
    const RGB8 panel = { 18, 22, 30 }, line = { 140, 155, 165 }, white = { 255, 255, 255 }, splitBlue = { 15, 107, 255 };
    const RGB8 kickC = { 220, 90, 40 }, tomC = { 190, 120, 70 }, snareC = { 220, 180, 110 }, hatC = { 120, 220, 180 }, clickC = { 200, 200, 210 };
    double W = S * 0.96, H = W * (0.50 / 0.82); int x0 = (S - (int)W) / 2, y0 = (S - (int)H) / 2;
    for (int y = 0; y < (int)H; y++) for (int x = 0; x < (int)W; x++) {
        double sx = (x / W) * 2 - 1, sy = (y / H) * 2 - 1;
        double hw = 0.82, hh = 0.50, corner = 0.055, px = fabs(sx) * hw, py = fabs(sy) * hh, qx = px - (hw - corner), qy = py - (hh - corner);
        double outside = hypot(fmax(qx, 0), fmax(qy, 0)), inside = fmin(fmax(qx, qy), 0), sd = outside + inside - corner; if (sd > 0) continue;
        double radius = 1.0 - fmin(1.0, fmax(0, -sd) / hh);
        RGB8 c = panel;
        if (radius < 0.23) c = blend(panel, kickC, 0.35 * (1 - radius / 0.23) + 0.08);
        else if (radius < 0.44) c = blend(panel, tomC, 0.16);
        else if (radius < 0.66) c = blend(panel, snareC, 0.16);
        else if (radius < 0.92) c = blend(panel, hatC, 0.14);
        else c = blend(panel, clickC, 0.22);
        if (g_page == PAGE_SPLIT && sx < 0) c = blend(panel, splitBlue, 0.14);
        puck_put(x0 + x, y0 + y, c, 0.82);
        double seams[] = { 0.23, 0.44, 0.66, 0.92 };
        for (int k = 0; k < 4; k++) if (fabs(radius - seams[k]) < 0.012) puck_put(x0 + x, y0 + y, line, 0.35);
    }
    if (g_page == PAGE_SPLIT) for (int y = y0; y < y0 + (int)H; y++) puck_put(x0 + (int)(W / 2), y, white, 0.5);
    for (int i = 0; i < MAX_CONTACTS; i++) if (g_contacts[i].down) {
        double px = x0 + g_contacts[i].x * W, py = y0 + g_contacts[i].y * H; int pr = pt(7.0);
        for (int y = (int)py - pr - 1; y <= (int)py + pr + 1; y++) for (int x = (int)px - pr - 1; x <= (int)px + pr + 1; x++) {
            double d = sqrt((x + 0.5 - px) * (x + 0.5 - px) + (y + 0.5 - py) * (y + 0.5 - py));
            if (d < pr) puck_put(x, y, white, 0.9); else if (d < pr + 1.5) puck_put(x, y, white, 0.9 * (pr + 1.5 - d) / 1.5);
        }
    }
}

static void surface_move(HWND hwnd, int dx, int dy) {
    if (g_page != PAGE_FX) return;
    POINT p; GetCursorPos(&p);
    RECT strip = { g_posX, g_posY, g_posX + g_stripW, g_posY + g_stripH };
    if (PtInRect(&strip, p) || g_mouseMidi >= 0) return;            // the strip is for clicking keys
    g_fxRamping = 0;
    g_bendUnits -= dy / (80.0f * (float)g_dpi); if (g_bendUnits > 2) g_bendUnits = 2; if (g_bendUnits < -2) g_bendUnits = -2;
    g_fxX += dx / (160.0f * (float)g_dpi); if (g_fxX > 1) g_fxX = 1; if (g_fxX < -1) g_fxX = -1;
    g_lastSurfaceMove = GetTickCount();
    apply_fx();
    g_puckAlpha = 1; puck_show();
    SetTimer(hwnd, IDT_FX, 16, NULL);
}

static void surface_tick(HWND hwnd) {
    DWORD now = GetTickCount(), idle = now - g_lastSurfaceMove;
    int settled = fabsf(g_bendUnits) < 0.002f && fabsf(g_fxX) < 0.002f;
    if (!settled && !g_fxRamping && idle > 400 && !anything_sounding()) { g_fxRamping = 1; g_rampStart = now; g_rampFromBend = g_bendUnits; g_rampFromX = g_fxX; }
    if (g_fxRamping) {
        float t = (now - g_rampStart) / 480.0f; if (t > 1) t = 1;
        float e = 1 - (1 - t) * (1 - t);
        g_bendUnits = g_rampFromBend * (1 - e); g_fxX = g_rampFromX * (1 - e);
        apply_fx();
        if (t >= 1) { g_fxRamping = 0; g_bendUnits = 0; g_fxX = 0; apply_fx(); }
    }
    if (page_takes_pad()) { DWORD tIdle = GetTickCount() - g_lastTouch; if (tIdle > 2000 || !g_typing) g_puckAlpha -= 16.0f / 400.0f; }
    else if (idle > 1500 && settled) g_puckAlpha -= 16.0f / 400.0f; else if (idle > 1500 && !g_typing) g_puckAlpha -= 16.0f / 400.0f;
    if (g_puckAlpha < 0) g_puckAlpha = 0;
    puck_show();
    if (g_puckAlpha <= 0 && settled && !g_fxRamping) KillTimer(hwnd, IDT_FX);
}

static void set_page(HWND hwnd, int page) {
    if (page == g_page) return;
    g_page = page; setting_set(L"page", (DWORD)page);
    mb_log(L"page -> %s", PAGE_NAMES[page]);
    for (int i = 0; i < MAX_CONTACTS; i++) if (g_contacts[i].down && g_contacts[i].struck) { perc_release(g_contacts[i].group); g_contacts[i].struck = 0; }
    if (g_typing) { if (page == PAGE_MOUSE) cursor_show(); else cursor_hide(); }
    cue(hwnd, 67, 60);
    g_puckAlpha = 1; g_lastTouch = GetTickCount(); SetTimer(hwnd, IDT_FX, 16, NULL); puck_show();
    push_frame(hwnd);
}

static void surface_reset(HWND hwnd) {
    // Focus loss ramps everything to neutral, as on the Mac.
    if (fabsf(g_bendUnits) > 0.002f || fabsf(g_fxX) > 0.002f) { g_fxRamping = 1; g_rampStart = GetTickCount(); g_rampFromBend = g_bendUnits; g_rampFromX = g_fxX; }
    g_lastSurfaceMove = 0;
    SetTimer(hwnd, IDT_FX, 16, NULL);
}

// ──────────────────── taskbar tracking ───────────────────────────

static int g_lastTaskbarH = 0;

static int find_slot(int *outX, int *outY, int *outTaskbarH) {
    HWND tray = FindWindowW(L"Shell_TrayWnd", NULL);
    if (!tray || !IsWindowVisible(tray)) return 0;
    RECT tr; GetWindowRect(tray, &tr);
    // Shell_TrayWnd is taller than what you see (108 px for a 72 px bar at 150 %);
    // the visible band is what the work area leaves out at the bottom.
    RECT wa; SystemParametersInfoW(SPI_GETWORKAREA, 0, &wa, 0);
    int screenH = GetSystemMetrics(SM_CYSCREEN);
    if (wa.bottom < screenH && screenH - wa.bottom <= 200) { tr.top = wa.bottom; tr.bottom = screenH; }
    int taskbarH = tr.bottom - tr.top;
    if (taskbarH <= 0 || taskbarH > 200) return 0;
    // Flush left: Windows 11 centres its icons, so the left end of the
    // taskbar is the empty field, like the menu bar's right end on the Mac.
    *outTaskbarH = taskbarH;
    *outX = tr.left + 12;
    *outY = tr.top;
    return 1;
}

static int fullscreen_app_in_front(void) {
    QUERY_USER_NOTIFICATION_STATE st;
    if (SUCCEEDED(SHQueryUserNotificationState(&st)))
        return st == QUNS_BUSY || st == QUNS_RUNNING_D3D_FULL_SCREEN || st == QUNS_PRESENTATION_MODE;
    return 0;
}

static int g_hiddenWhy = 0;
static void track(HWND hwnd, int force) {
    int x, y, h;
    int flashing = g_flash && IsWindowVisible(g_flash);
    int noSlot = !find_slot(&x, &y, &h), fs = (noSlot || flashing) ? 0 : fullscreen_app_in_front();
    if (noSlot || fs) {
        int why = noSlot ? 1 : 2;
        if (why != g_hiddenWhy) { g_hiddenWhy = why; mb_log(L"track: hide (%s)", noSlot ? L"no taskbar slot" : L"fullscreen app in front"); }
        ShowWindow(hwnd, SW_HIDE); return;
    }
    if (g_hiddenWhy) { g_hiddenWhy = 0; mb_log(L"track: show again"); }
    if (h != g_lastTaskbarH || force) {
        g_lastTaskbarH = h;
        layout_for_taskbar_height(h);
        ensure_surface();
        find_slot(&x, &y, &h);
    }
    g_posX = x; g_posY = y;
    SetWindowPos(hwnd, HWND_TOPMOST, x, y, g_stripW, g_stripH, SWP_NOACTIVATE | SWP_SHOWWINDOW);
    push_frame(hwnd);
}

// ─────────────────────────── window ──────────────────────────────

#define WM_TRAY (WM_APP + 1)
#define IDM_QUIT 100
#define IDM_TYPING 101
#define IDM_AUTOSTART 102
#define IDM_FNFREE 103
#define IDM_LOWLAT 104
#define IDM_PAGE_BASE 110
#define IDM_PROGRAM_BASE 200
#define IDM_MIDI_BASE 400
#define IDT_TRACK 1

// All 128 General MIDI programs, grouped in the 16 GM families for the menu.
static const wchar_t *GM_NAMES[128] = {
    L"Acoustic Grand Piano", L"Bright Acoustic Piano", L"Electric Grand Piano", L"Honky-tonk Piano", L"Electric Piano 1", L"Electric Piano 2", L"Harpsichord", L"Clavinet",
    L"Celesta", L"Glockenspiel", L"Music Box", L"Vibraphone", L"Marimba", L"Xylophone", L"Tubular Bells", L"Dulcimer",
    L"Drawbar Organ", L"Percussive Organ", L"Rock Organ", L"Church Organ", L"Reed Organ", L"Accordion", L"Harmonica", L"Tango Accordion",
    L"Nylon Guitar", L"Steel Guitar", L"Jazz Guitar", L"Clean Guitar", L"Muted Guitar", L"Overdriven Guitar", L"Distortion Guitar", L"Guitar Harmonics",
    L"Acoustic Bass", L"Fingered Bass", L"Picked Bass", L"Fretless Bass", L"Slap Bass 1", L"Slap Bass 2", L"Synth Bass 1", L"Synth Bass 2",
    L"Violin", L"Viola", L"Cello", L"Contrabass", L"Tremolo Strings", L"Pizzicato Strings", L"Orchestral Harp", L"Timpani",
    L"String Ensemble 1", L"String Ensemble 2", L"Synth Strings 1", L"Synth Strings 2", L"Choir Aahs", L"Voice Oohs", L"Synth Voice", L"Orchestra Hit",
    L"Trumpet", L"Trombone", L"Tuba", L"Muted Trumpet", L"French Horn", L"Brass Section", L"Synth Brass 1", L"Synth Brass 2",
    L"Soprano Sax", L"Alto Sax", L"Tenor Sax", L"Baritone Sax", L"Oboe", L"English Horn", L"Bassoon", L"Clarinet",
    L"Piccolo", L"Flute", L"Recorder", L"Pan Flute", L"Blown Bottle", L"Shakuhachi", L"Whistle", L"Ocarina",
    L"Square Lead", L"Sawtooth Lead", L"Calliope Lead", L"Chiff Lead", L"Charang Lead", L"Voice Lead", L"Fifths Lead", L"Bass + Lead",
    L"New Age Pad", L"Warm Pad", L"Polysynth Pad", L"Choir Pad", L"Bowed Pad", L"Metallic Pad", L"Halo Pad", L"Sweep Pad",
    L"Rain", L"Soundtrack", L"Crystal", L"Atmosphere", L"Brightness", L"Goblins", L"Echoes", L"Sci-Fi",
    L"Sitar", L"Banjo", L"Shamisen", L"Koto", L"Kalimba", L"Bagpipe", L"Fiddle", L"Shanai",
    L"Tinkle Bell", L"Agogo", L"Steel Drums", L"Woodblock", L"Taiko Drum", L"Melodic Tom", L"Synth Drum", L"Reverse Cymbal",
    L"Guitar Fret Noise", L"Breath Noise", L"Seashore", L"Bird Tweet", L"Telephone Ring", L"Helicopter", L"Applause", L"Gunshot",
};
static const wchar_t *GM_FAMILIES[16] = { L"Piano", L"Chromatic Percussion", L"Organ", L"Guitar", L"Bass", L"Strings", L"Ensemble", L"Brass", L"Reed", L"Pipe", L"Synth Lead", L"Synth Pad", L"Synth Effects", L"Ethnic", L"Percussive", L"Sound Effects" };


static NOTIFYICONDATAW g_nid;

// A ♪ tray icon built from the same glyph mask the chip uses.
static HICON make_note_icon(void) {
    int sz = GetSystemMetrics(SM_CXSMICON) * 2; if (sz < 32) sz = 32;
    int mw, mh; uint8_t *mask = text_mask(L"\x266A", L"Segoe UI Symbol", sz - 4, FW_NORMAL, &mw, &mh);
    BITMAPINFO bi; memset(&bi, 0, sizeof bi);
    bi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER); bi.bmiHeader.biWidth = sz; bi.bmiHeader.biHeight = -sz;
    bi.bmiHeader.biPlanes = 1; bi.bmiHeader.biBitCount = 32; bi.bmiHeader.biCompression = BI_RGB;
    uint32_t *bits = NULL; HBITMAP color = CreateDIBSection(NULL, &bi, DIB_RGB_COLORS, (void **)&bits, NULL, 0);
    if (!color) { free(mask); return LoadIcon(NULL, IDI_APPLICATION); }
    memset(bits, 0, (size_t)sz * sz * 4);
    int ox = (sz - mw) / 2, oy = (sz - mh) / 2;
    for (int y = 0; y < mh && mask; y++) for (int x = 0; x < mw; x++) {
        int X = ox + x, Y = oy + y; if (X < 0 || Y < 0 || X >= sz || Y >= sz) continue;
        uint8_t a = mask[y * mw + x]; bits[Y * sz + X] = ((uint32_t)a << 24) | ((uint32_t)a << 16) | ((uint32_t)a << 8) | a;   // white, premultiplied
    }
    free(mask);
    HBITMAP maskBm = CreateBitmap(sz, sz, 1, 1, NULL);
    ICONINFO ii = { TRUE, 0, 0, maskBm, color };
    HICON icon = CreateIconIndirect(&ii);
    DeleteObject(maskBm); DeleteObject(color);
    return icon ? icon : LoadIcon(NULL, IDI_APPLICATION);
}

// Player settings survive restarts: HKCU\Software\Aesthetic Computer\Menu Band.
static const wchar_t *SETTINGS_KEY = L"Software\\Aesthetic Computer\\Menu Band";
static DWORD setting_get(const wchar_t *name, DWORD fallback) {
    HKEY k; DWORD v = fallback, size = sizeof v, type;
    if (RegOpenKeyExW(HKEY_CURRENT_USER, SETTINGS_KEY, 0, KEY_READ, &k) != ERROR_SUCCESS) return fallback;
    if (RegQueryValueExW(k, name, NULL, &type, (BYTE *)&v, &size) != ERROR_SUCCESS || type != REG_DWORD) v = fallback;
    RegCloseKey(k); return v;
}
static void setting_set(const wchar_t *name, DWORD v) {
    HKEY k; if (RegCreateKeyExW(HKEY_CURRENT_USER, SETTINGS_KEY, 0, NULL, 0, KEY_WRITE, NULL, &k, NULL) != ERROR_SUCCESS) return;
    RegSetValueExW(k, name, 0, REG_DWORD, (const BYTE *)&v, sizeof v); RegCloseKey(k);
}

static int autostart_enabled(void) {
    HKEY k; if (RegOpenKeyExW(HKEY_CURRENT_USER, L"Software\\Microsoft\\Windows\\CurrentVersion\\Run", 0, KEY_READ, &k) != ERROR_SUCCESS) return 0;
    DWORD type, size = 0; int present = RegQueryValueExW(k, L"MenuBand", NULL, &type, NULL, &size) == ERROR_SUCCESS;
    RegCloseKey(k); return present;
}

static void autostart_set(int on) {
    HKEY k; if (RegCreateKeyExW(HKEY_CURRENT_USER, L"Software\\Microsoft\\Windows\\CurrentVersion\\Run", 0, NULL, 0, KEY_WRITE, NULL, &k, NULL) != ERROR_SUCCESS) return;
    if (on) {
        wchar_t path[MAX_PATH + 2]; path[0] = L'"'; GetModuleFileNameW(NULL, path + 1, MAX_PATH); wcscat_s(path, MAX_PATH + 2, L"\"");
        RegSetValueExW(k, L"MenuBand", 0, REG_SZ, (const BYTE *)path, (DWORD)((wcslen(path) + 1) * sizeof(wchar_t)));
    } else RegDeleteValueW(k, L"MenuBand");
    RegCloseKey(k);
}

static void show_menu(HWND hwnd) {
    POINT p; GetCursorPos(&p);
    HMENU menu = CreatePopupMenu();
    AppendMenuW(menu, MF_STRING | (g_typing ? MF_CHECKED : 0), IDM_TYPING, L"Type to play  (Esc leaves)");
    AppendMenuW(menu, MF_SEPARATOR, 0, NULL);
    for (int f = 0; f < 16; f++) {
        HMENU fam = CreatePopupMenu();
        for (int i = f * 8; i < f * 8 + 8; i++) {
            wchar_t label[64]; swprintf(label, 64, L"%d  %s", i + 1, GM_NAMES[i]);
            AppendMenuW(fam, MF_STRING | (i == g_program ? MF_CHECKED : 0), IDM_PROGRAM_BASE + i, label);
        }
        wchar_t famLabel[64]; swprintf(famLabel, 64, L"%s%s", GM_FAMILIES[f], (g_program / 8 == f) ? L"  \x2713" : L"");
        AppendMenuW(menu, MF_POPUP, (UINT_PTR)fam, famLabel);
    }
    AppendMenuW(menu, MF_SEPARATOR, 0, NULL);
    HMENU midi = CreatePopupMenu();
    AppendMenuW(midi, MF_STRING | (g_midiDevice < 0 ? MF_CHECKED : 0), IDM_MIDI_BASE, L"Built-in synth");
    UINT n = midiOutGetNumDevs();
    for (UINT i = 0; i < n && i < 32; i++) {
        MIDIOUTCAPSW caps; if (midiOutGetDevCapsW(i, &caps, sizeof caps) != MMSYSERR_NOERROR) continue;
        AppendMenuW(midi, MF_STRING | ((int)i == g_midiDevice ? MF_CHECKED : 0), IDM_MIDI_BASE + 1 + i, caps.szPname);
    }
    AppendMenuW(menu, MF_POPUP, (UINT_PTR)midi, L"MIDI out");
    HMENU pages = CreatePopupMenu();
    for (int i = 0; i < 4; i++) { wchar_t pl[48]; swprintf(pl, 48, L"%s%s", PAGE_NAMES[i], i == PAGE_SKIN ? L"  (TrackDrum)" : L""); AppendMenuW(pages, MF_STRING | (i == g_page ? MF_CHECKED : 0), IDM_PAGE_BASE + i, pl); }
    AppendMenuW(menu, MF_POPUP, (UINT_PTR)pages, L"Trackpad page  (Tab / Shift+Tab)");
    AppendMenuW(menu, MF_STRING | (g_lowLatency ? MF_CHECKED : 0), IDM_LOWLAT, L"Lowest latency while armed  (takes the speakers; other apps pause)");
    AppendMenuW(menu, MF_STRING | (g_fnFree ? MF_CHECKED : 0), IDM_FNFREE, L"Volume keys without Fn while armed  (F3 down, F4 mute, F5 up)");
    AppendMenuW(menu, MF_STRING | (autostart_enabled() ? MF_CHECKED : 0), IDM_AUTOSTART, L"Start at sign-in");
    AppendMenuW(menu, MF_SEPARATOR, 0, NULL);
    AppendMenuW(menu, MF_STRING, IDM_QUIT, L"Quit Menu Band");
    // TPM_RETURNCMD: a non-activating tool window is not a reliable WM_COMMAND
    // target, so take the choice back directly and dispatch it ourselves.
    int wasHidden = g_cursorHidden; if (wasHidden) cursor_show();
    SetForegroundWindow(hwnd);
    int chosen = TrackPopupMenu(menu, TPM_RIGHTBUTTON | TPM_BOTTOMALIGN | TPM_RETURNCMD | TPM_NONOTIFY, p.x, p.y, 0, hwnd, NULL);
    PostMessageW(hwnd, WM_NULL, 0, 0);
    DestroyMenu(menu);
    mb_log(L"menu: chose %d", chosen);
    if (chosen) SendMessageW(hwnd, WM_COMMAND, (WPARAM)chosen, 0);
    if (wasHidden && g_typing) cursor_hide();
}

static int g_mouseExt[2] = { -1, -1 };
static void mouse_up(HWND hwnd) {
    for (int i = 0; i < 2; i++) if (g_mouseExt[i] >= 0) { lit_off(hwnd, g_mouseExt[i]); g_mouseExt[i] = -1; }
    if (g_mouseMidi >= 0) { lit_off(hwnd, g_mouseMidi); g_mouseMidi = -1; }
}
static void mouse_down(HWND hwnd, int midi) {
    if (midi < 0 || midi == g_mouseMidi) return;
    mouse_up(hwnd);
    g_mouseMidi = midi;
    lit_on(hwnd, midi, 100);
    int iv[2], n = chord_intervals(chord_quality(midi), iv);
    for (int i = 0; i < n; i++) { g_mouseExt[i] = midi + iv[i]; lit_on(hwnd, g_mouseExt[i], 92); }
}

// Remote autoplay for install debugging (the Mac has a distributed-notification
// hook; here a file poke works across sessions): write
//   %TEMP%\menuband-play.txt   e.g. "60 64 67 72 @78 350"
// midi notes in order, optional @program, optional ms per note (default 400).
#define IDT_PLAY 3
static int g_playNotes[64], g_playCount = 0, g_playIndex = 0, g_playMs = 400, g_playSounding = -1;
static void step_play(HWND hwnd) {
    if (g_playSounding >= 0) { lit_off(hwnd, g_playSounding); g_playSounding = -1; }
    if (g_playIndex >= g_playCount) { KillTimer(hwnd, IDT_PLAY); mb_log(L"play: done"); return; }
    g_playSounding = g_playNotes[g_playIndex++];
    lit_on(hwnd, g_playSounding, 100);
}
static void poll_play_file(HWND hwnd) {
    wchar_t path[MAX_PATH]; GetTempPathW(MAX_PATH, path); wcscat_s(path, MAX_PATH, L"menuband-play.txt");
    FILE *f = NULL; if (_wfopen_s(&f, path, L"r") != 0 || !f) return;
    char line[512] = {0}; fgets(line, sizeof line, f); fclose(f); DeleteFileW(path);
    g_playCount = 0; g_playIndex = 0; g_playMs = 400;
    for (char *tok = strtok(line, " \t\r\n"); tok; tok = strtok(NULL, " \t\r\n")) {
        if (tok[0] == '@') { g_program = atoi(tok + 1) & 127; midi_program(); setting_set(L"program", (DWORD)g_program); continue; }
        int v = atoi(tok);
        if (v >= 200) g_playMs = v; else if (v >= 0 && v < 128 && g_playCount < 64) g_playNotes[g_playCount++] = v;
    }
    mb_log(L"play: %d notes, program %d, %d ms each", g_playCount, g_program, g_playMs);
    if (g_playCount) { SetTimer(hwnd, IDT_PLAY, g_playMs, NULL); step_play(hwnd); }
}

#define IDT_SLIDE 6
static double g_slideF = 0;
static void step_slide(HWND hwnd) {
    g_slideF *= 0.76;                       // ease-out, ~220 ms to settle at 16 ms ticks
    if (fabs(g_slideF) < 0.8) { g_slideF = 0; g_slide = 0; KillTimer(hwnd, IDT_SLIDE); }
    else g_slide = (int)(g_slideF + (g_slideF > 0 ? 0.5 : -0.5));
    push_frame(hwnd);
}
static void shift_octave(HWND hwnd, int dir) {
    int next = g_firstMidi + 12 * dir;
    if (next < 36 || next > 84) return;
    mouse_up(hwnd);
    release_typed(hwnd);
    g_firstMidi = next; build_keys();
    // The new octave starts where it just was (one octave of keys to the side)
    // and glides home, like the Mac's octaveSlideDistance animation.
    g_slideF += dir * 7.0 * g_keyW; g_slide = (int)g_slideF;
    SetTimer(hwnd, IDT_SLIDE, 16, NULL);
    push_frame(hwnd);
    setting_set(L"firstMidi", (DWORD)g_firstMidi);
}

static LRESULT CALLBACK wndproc(HWND hwnd, UINT msg, WPARAM wp, LPARAM lp) {
    switch (msg) {
    case WM_CREATE:
        SetTimer(hwnd, IDT_TRACK, 400, NULL);
        return 0;
    case WM_TIMER:
        if (wp == IDT_TRACK) { track(hwnd, 0); poll_play_file(hwnd); }
        if (wp == IDT_PLAY) step_play(hwnd);
        if (wp == IDT_DIGITS) { KillTimer(hwnd, IDT_DIGITS); g_pendingDigits[0] = 0; push_frame(hwnd); }
        if (wp == IDT_CUE) step_cue(hwnd);
        if (wp == IDT_SLIDE) step_slide(hwnd);
        if (wp == IDT_FX) surface_tick(hwnd);
        return 0;
    case WM_MOUSEACTIVATE:
        return MA_NOACTIVATE;
    case WM_MOUSEMOVE: {
        int x = GET_X_LPARAM(lp), y = GET_Y_LPARAM(lp);
        int midi = key_at(x, y), chip = chip_at(x, y);
        TRACKMOUSEEVENT tme = { sizeof tme, TME_LEAVE, hwnd, 0 }; TrackMouseEvent(&tme);
        if ((wp & MK_LBUTTON) && midi >= 0) mouse_down(hwnd, midi);
        if (midi != g_hoverMidi || chip != g_hoverChip) { g_hoverMidi = midi; g_hoverChip = chip; push_frame(hwnd); }
        return 0;
    }
    case WM_MOUSELEAVE:
        if (g_hoverMidi != -1 || g_hoverChip) { g_hoverMidi = -1; g_hoverChip = 0; push_frame(hwnd); }
        return 0;
    case WM_LBUTTONDOWN: {
        int x = GET_X_LPARAM(lp), y = GET_Y_LPARAM(lp);
        if (chip_at(x, y)) { show_menu(hwnd); return 0; }
        SetCapture(hwnd);
        int midi = key_at(x, y);
        mouse_down(hwnd, midi);
        if (midi >= 0) mb_log(L"click: x=%d -> midi %d (program %d)", x, midi, g_program);
        if (midi >= 0 && !g_typing) typing_set(hwnd, 1);   // a click focuses, as on the Mac
        return 0;
    }
    case WM_LBUTTONUP:
        ReleaseCapture();
        mouse_up(hwnd);
        return 0;
    case WM_CAPTURECHANGED:
        mouse_up(hwnd);
        return 0;
    case WM_RBUTTONDOWN:
        mb_log(L"right button down");
        return 0;
    case WM_RBUTTONUP:
        mb_log(L"right button up -> menu");
        show_menu(hwnd);
        return 0;
    case WM_CONTEXTMENU:
        mb_log(L"WM_CONTEXTMENU -> menu");
        show_menu(hwnd);
        return 0;
    case WM_MOUSEWHEEL:
        shift_octave(hwnd, GET_WHEEL_DELTA_WPARAM(wp) > 0 ? 1 : -1);
        return 0;
    case WM_OCTAVE:
        shift_octave(hwnd, (int)(intptr_t)wp);
        return 0;
    case WM_DIGIT: {
        // Live entry like the Mac: each digit extends the number and applies it
        // at once ("7" → 7 Harpsichord, then "79" → 79 Whistle); a pause of
        // 900 ms, a third digit, or a number that can't grow ends the entry.
        size_t n = wcslen(g_pendingDigits);
        if (n >= 3) n = 0;
        g_pendingDigits[n] = (wchar_t)(L'0' + (int)wp); g_pendingDigits[n + 1] = 0;
        int value = _wtoi(g_pendingDigits);
        if (value >= 1 && value <= 128) { g_program = value - 1; midi_program(); setting_set(L"program", (DWORD)g_program); mb_log(L"typed program %d (%s)", value, GM_NAMES[g_program]); }
        if (value > 128 || value == 0) { g_pendingDigits[0] = 0; }
        else if (value * 10 > 128 || n + 1 >= 3) { SetTimer(hwnd, IDT_DIGITS, 350, NULL); }
        else SetTimer(hwnd, IDT_DIGITS, 900, NULL);
        push_frame(hwnd);
        return 0;
    }
    case WM_TYPEKEY: {
        int vk = (int)wp, down = (int)lp;
        if (vk < 0 || vk > 255) return 0;
        int midi = 60 + g_semitoneByVk[vk] + (g_firstMidi - 60);
        if (down) {
            if (!g_vkDown[vk]) {
                g_vkDown[vk] = 1; g_vkRoot[vk] = midi; g_vkExt[vk][0] = g_vkExt[vk][1] = -1;
                lit_on(hwnd, midi, 100);
                revoice(hwnd, vk);
                mb_log(L"type: vk %d -> midi %d q%d (program %d)", vk, midi, chord_quality(midi), g_program);
            }
        } else if (g_vkDown[vk]) {
            g_vkDown[vk] = 0;
            for (int i = 0; i < 2; i++) if (g_vkExt[vk][i] >= 0) { lit_off(hwnd, g_vkExt[vk][i]); g_vkExt[vk][i] = -1; }
            lit_off(hwnd, midi);
        }
        return 0;
    case WM_CHORDMOD:
        for (int v = 0; v < 256; v++) if (g_vkDown[v]) revoice(hwnd, v);
        return 0;
    case WM_SURFACE:
        surface_move(hwnd, (int)(short)wp, (int)(short)lp);
        return 0;
    case WM_PAGE:
        set_page(hwnd, wp ? page_after_shift_tab(g_page) : page_after_tab(g_page));
        return 0;
    case WM_INPUT:
        touch_input(hwnd, (HRAWINPUT)lp);
        return DefWindowProcW(hwnd, msg, wp, lp);
    }
    case WM_TYPEESC:
        typing_set(hwnd, 0);
        return 0;
    case WM_PEDAL:
        pedal_release(hwnd);
        return 0;
    case WM_LATCH:
        g_enterLatch = (int)wp;
        return 0;
    case WM_UNLATCH:
        unlatch(hwnd, (int)wp);
        return 0;
    case WM_SURF: {
        // Arrow keys surf the instruments: ←/→ one program, ↑/↓ one family.
        int next = g_program + (int)(intptr_t)wp; if (next < 0) next += 128; if (next > 127) next -= 128;
        g_program = next; midi_program(); setting_set(L"program", (DWORD)g_program);
        mb_log(L"surf -> %d (%s)", g_program + 1, GM_NAMES[g_program]);
        push_frame(hwnd);
        return 0;
    }
    case WM_CTRLTRIPLE:
        mb_log(L"ctrl x2 -> %s", g_typing ? L"release" : L"focus");
        typing_set(hwnd, !g_typing);
        return 0;
    case WM_TRAY:
        if (LOWORD(lp) == WM_RBUTTONUP || LOWORD(lp) == WM_LBUTTONUP) show_menu(hwnd);
        return 0;
    case WM_COMMAND: {
        int id = LOWORD(wp);
        if (id == IDM_QUIT) { mb_log(L"menu: quit"); DestroyWindow(hwnd); return 0; }
        if (id == IDM_TYPING) { typing_set(hwnd, !g_typing); return 0; }
        if (id == IDM_AUTOSTART) { autostart_set(!autostart_enabled()); return 0; }
        if (id == IDM_FNFREE) { g_fnFree = !g_fnFree; setting_set(L"fnFree", (DWORD)g_fnFree); return 0; }
        if (id >= IDM_PAGE_BASE && id < IDM_PAGE_BASE + 4) { set_page(hwnd, id - IDM_PAGE_BASE); return 0; }
        if (id == IDM_LOWLAT) { g_lowLatency = !g_lowLatency; setting_set(L"lowLatency", (DWORD)g_lowLatency); audio_set_exclusive(g_typing); return 0; }
        if (id >= IDM_PROGRAM_BASE && id < IDM_PROGRAM_BASE + 128) { g_program = id - IDM_PROGRAM_BASE; midi_program(); setting_set(L"program", (DWORD)g_program); mb_log(L"program -> %d (%s)", g_program, GM_NAMES[g_program]); push_frame(hwnd); }
        if (id >= IDM_MIDI_BASE && id < IDM_MIDI_BASE + 40) midi_select(id - IDM_MIDI_BASE - 1);
        return 0;
    }
    case WM_DWMCOLORIZATIONCOLORCHANGED:
        read_accent(); push_frame(hwnd);
        return 0;
    case WM_DISPLAYCHANGE:
    case WM_DPICHANGED:
    case WM_SETTINGCHANGE:
        track(hwnd, 1);
        return 0;
    case WM_DESTROY:
        mb_log(L"WM_DESTROY on strip");
        typing_set(hwnd, 0);
        cursor_show();
        Shell_NotifyIconW(NIM_DELETE, &g_nid);
        if (g_hook) UnhookWindowsHookEx(g_hook);
        if (g_mouseHook) UnhookWindowsHookEx(g_mouseHook);
        InterlockedExchange(&g_audioRunning, 0);
        if (g_midiOut) { midiOutReset(g_midiOut); midiOutClose(g_midiOut); }
        PostQuitMessage(0);
        return 0;
    }
    return DefWindowProcW(hwnd, msg, wp, lp);
}

static void mb_log(const wchar_t *fmt, ...) {
    wchar_t path[MAX_PATH]; GetTempPathW(MAX_PATH, path); wcscat_s(path, MAX_PATH, L"menuband.log");
    FILE *f = NULL; _wfopen_s(&f, path, L"a"); if (!f) return;
    SYSTEMTIME st; GetLocalTime(&st);
    fwprintf(f, L"%02d:%02d:%02d.%03d ", st.wHour, st.wMinute, st.wSecond, st.wMilliseconds);
    va_list ap; va_start(ap, fmt); vfwprintf(f, fmt, ap); va_end(ap);
    fwprintf(f, L"\n"); fclose(f);
}

// Last-resort crash note: Windows Error Reporting is off on the lean install,
// so write the exception code and address to %TEMP%\menuband-crash.txt.
static LONG WINAPI crash_filter(EXCEPTION_POINTERS *ep) {
    SystemParametersInfoW(SPI_SETCURSORS, 0, NULL, 0);   // never leave the pointer hidden
    wchar_t path[MAX_PATH]; GetTempPathW(MAX_PATH, path); wcscat_s(path, MAX_PATH, L"menuband-crash.txt");
    FILE *f = NULL; _wfopen_s(&f, path, L"a");
    if (f) {
        SYSTEMTIME st; GetLocalTime(&st);
        fwprintf(f, L"%04d-%02d-%02d %02d:%02d:%02d code=0x%08lX addr=%p module=%p\n", st.wYear, st.wMonth, st.wDay, st.wHour, st.wMinute, st.wSecond,
                 ep->ExceptionRecord->ExceptionCode, ep->ExceptionRecord->ExceptionAddress, (void *)GetModuleHandleW(NULL));
        fclose(f);
    }
    return EXCEPTION_EXECUTE_HANDLER;
}

int WINAPI wWinMain(HINSTANCE hInst, HINSTANCE prev, PWSTR cmdline, int show) {
    (void)prev; (void)cmdline; (void)show;
    SetUnhandledExceptionFilter(crash_filter);
    SetProcessDpiAwarenessContext(DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2);
    CoInitializeEx(NULL, COINIT_MULTITHREADED);
    InitializeCriticalSection(&g_lock);
    HANDLE single = CreateMutexW(NULL, TRUE, L"computer.aesthetic.menuband.windows");
    if (GetLastError() == ERROR_ALREADY_EXISTS) return 0;

    g_program = (int)setting_get(L"program", AC_WHISTLE_PROGRAM) & 127;
    g_fnFree = setting_get(L"fnFree", 1) ? 1 : 0;
    g_lowLatency = setting_get(L"lowLatency", 1) ? 1 : 0;
    g_page = (int)setting_get(L"page", PAGE_SKIN); if (g_page < 0 || g_page > 3) g_page = PAGE_SKIN;
    g_firstMidi = (int)setting_get(L"firstMidi", 60); if (g_firstMidi < 36 || g_firstMidi > 84 || g_firstMidi % 12) g_firstMidi = 60;
    build_keys();
    build_keymap();
    piano_load_bank();
    if (!audio_start()) { mb_log(L"audio_start FAILED"); MessageBoxW(NULL, L"Menu Band could not open the audio output.", L"Menu Band", MB_ICONWARNING); }

    WNDCLASSW wc; memset(&wc, 0, sizeof wc);
    wc.lpfnWndProc = wndproc; wc.hInstance = hInst; wc.lpszClassName = L"MenuBandStrip";
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    RegisterClassW(&wc);
    WNDCLASSW fc; memset(&fc, 0, sizeof fc);
    fc.lpfnWndProc = flashproc; fc.hInstance = hInst; fc.lpszClassName = L"MenuBandFlash";
    RegisterClassW(&fc);

    HWND hwnd = CreateWindowExW(WS_EX_TOPMOST | WS_EX_TOOLWINDOW | WS_EX_NOACTIVATE | WS_EX_LAYERED,
                                L"MenuBandStrip", L"Menu Band", WS_POPUP, 0, 0, 10, 10, NULL, NULL, hInst, NULL);
    g_strip = hwnd;
    g_flash = CreateWindowExW(WS_EX_TOPMOST | WS_EX_TOOLWINDOW | WS_EX_NOACTIVATE | WS_EX_LAYERED | WS_EX_TRANSPARENT,
                              L"MenuBandFlash", L"", WS_POPUP, 0, 0, 10, 10, NULL, NULL, hInst, NULL);

    memset(&g_nid, 0, sizeof g_nid);
    g_nid.cbSize = sizeof g_nid; g_nid.hWnd = hwnd; g_nid.uID = 1;
    g_nid.uFlags = NIF_ICON | NIF_MESSAGE | NIF_TIP; g_nid.uCallbackMessage = WM_TRAY;
    g_nid.hIcon = make_note_icon();
    wcscpy_s(g_nid.szTip, 128, L"Menu Band");
    Shell_NotifyIconW(NIM_ADD, &g_nid);

    WNDCLASSW pc; memset(&pc, 0, sizeof pc);
    pc.lpfnWndProc = DefWindowProcW; pc.hInstance = hInst; pc.lpszClassName = L"MenuBandPuck";
    RegisterClassW(&pc);
    g_puck = CreateWindowExW(WS_EX_TOPMOST | WS_EX_TOOLWINDOW | WS_EX_NOACTIVATE | WS_EX_LAYERED | WS_EX_TRANSPARENT,
                             L"MenuBandPuck", L"", WS_POPUP, 0, 0, 10, 10, NULL, NULL, hInst, NULL);
    g_hook = SetWindowsHookExW(WH_KEYBOARD_LL, ll_keyboard, hInst, 0);
    g_mouseHook = SetWindowsHookExW(WH_MOUSE_LL, ll_mouse, hInst, 0);
    touch_register(hwnd);
    mb_log(L"start: strip=%p flash=%p hook=%p audio=%d (lastErr=%lu)", (void *)hwnd, (void *)g_flash, (void *)g_hook, (int)g_audioRunning, GetLastError());
    track(hwnd, 1);
    mb_log(L"start: placed at %d,%d size %dx%d taskbarH=%d", g_posX, g_posY, g_stripW, g_stripH, g_lastTaskbarH);

    MSG m;
    while (GetMessageW(&m, NULL, 0, 0)) { TranslateMessage(&m); DispatchMessageW(&m); }
    audio_stop();
    CloseHandle(single);
    return 0;
}
