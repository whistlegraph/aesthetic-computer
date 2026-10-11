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

static void mb_log(const wchar_t *fmt, ...);
static volatile LONG g_dbgNoteOns;

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
enum { CMD_ON = 1, CMD_OFF = 2, CMD_PANIC = 3, CMD_WON = 4, CMD_WOFF = 5 };

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
static void wh_note_on(int midi, int velocity); static void wh_note_off(int midi); static void wh_panic(void); static void wh_render(float *out, int frames);

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
            wh_panic();
        } else if (c->kind == CMD_WON) {
            wh_note_on(c->midi, c->velocity);
        } else if (c->kind == CMD_WOFF) {
            wh_note_off(c->midi);
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
    wh_render(out, frames);
}

// ─────────────── the AC whistle (`79), ported from MenuBandWhistleVoice.swift ───────────────
// A human whistle, not a tin whistle: a sine core with an onset scoop, 1/f-ish
// jitter and a vibrato that grows in; one gentle second harmonic; "air" from
// white noise through a high-Q resonator on the fundamental plus a quiet hiss
// band; and a shared breath that drifts and flutters. Program 78 routes here,
// everything else to gm_synth.

#define WH_VOICES 12
typedef struct {
    int midi; double f0; float gain;
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
            double f = v->f0 * pow(2.0, cents / 1200.0);
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
            g_whBus[2 * i] += smp * 0.7071f; g_whBus[2 * i + 1] += smp * 0.7071f;
        }
        v->active = active;
    }
    for (int i = 0; i < 2 * frames; i++) out[i] += tanhf(g_whBus[i] * WH_MASTER);
}

// ───────────────────────────── WASAPI ────────────────────────────

static IAudioClient *g_client = NULL;
static IAudioRenderClient *g_render = NULL;
static HANDLE g_audioEvent = NULL;
static UINT32 g_bufferFrames = 0;

static volatile LONG g_dbgCallbacks = 0, g_dbgFrames = 0;
static volatile float g_dbgPeak = 0;

static DWORD WINAPI audio_thread(LPVOID arg) {
    (void)arg;
    DWORD taskIndex = 0;
    HANDLE mmcss = AvSetMmThreadCharacteristicsW(L"Pro Audio", &taskIndex);
    DWORD lastLog = GetTickCount(); int timeouts = 0;
    while (g_audioRunning) {
        if (WaitForSingleObject(g_audioEvent, 2000) != WAIT_OBJECT_0) { timeouts++; mb_log(L"audio: event timeout #%d", timeouts); continue; }
        UINT32 padding = 0;
        HRESULT hr = g_client->lpVtbl->GetCurrentPadding(g_client, &padding);
        if (FAILED(hr)) { mb_log(L"audio: GetCurrentPadding 0x%08lX", hr); continue; }
        UINT32 frames = g_bufferFrames - padding;
        if (frames == 0) continue;
        BYTE *data = NULL;
        hr = g_render->lpVtbl->GetBuffer(g_render, frames, &data);
        if (FAILED(hr)) { mb_log(L"audio: GetBuffer 0x%08lX", hr); continue; }
        synth_render((float *)data, (int)frames);
        float *f = (float *)data; float peak = g_dbgPeak;
        for (UINT32 i = 0; i < frames * 2; i++) { float a = fabsf(f[i]); if (a > peak) peak = a; }
        g_dbgPeak = peak; InterlockedIncrement(&g_dbgCallbacks); InterlockedAdd(&g_dbgFrames, (LONG)frames);
        g_render->lpVtbl->ReleaseBuffer(g_render, frames, 0);
        if (GetTickCount() - lastLog > 2000) {
            int active = 0; for (int i = 0; i < MAX_VOICES; i++) if (g_voices[i].active) active++;
            for (int i = 0; i < WH_VOICES; i++) if (g_wv[i].active) active++;
            mb_log(L"audio: callbacks=%ld frames=%ld peak=%.4f active=%d noteOns=%ld sr=%.0f buf=%u", g_dbgCallbacks, g_dbgFrames, g_dbgPeak, active, g_dbgNoteOns, g_sampleRate, g_bufferFrames);
            g_dbgPeak = 0; lastLog = GetTickCount();
        }
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
static uint8_t g_noteOwner[128];   // 0 gm, 1 whistle, 2 midi out
static void play_on(int midi, int vel) {
    if (midi < 0 || midi > 127) return;
    if (g_midiOut) { g_noteOwner[midi] = 2; midiOutShortMsg(g_midiOut, 0x90 | ((DWORD)midi << 8) | ((DWORD)vel << 16)); }
    else if (g_program == AC_WHISTLE_PROGRAM) { g_noteOwner[midi] = 1; Cmd c = { CMD_WON, (uint8_t)midi, (uint8_t)vel, 0, 0 }; synth_push(c); InterlockedIncrement(&g_dbgNoteOns); }
    else { g_noteOwner[midi] = 0; synth_note_on(midi, vel); }
}
static void play_off(int midi) {
    if (midi < 0 || midi > 127) return;
    if (g_noteOwner[midi] == 2 && g_midiOut) midiOutShortMsg(g_midiOut, 0x80 | ((DWORD)midi << 8));
    else if (g_noteOwner[midi] == 1) { Cmd c = { CMD_WOFF, (uint8_t)midi, 0, 0, 0 }; synth_push(c); }
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

static void layout_for_taskbar_height(int taskbarH) {
    // Keys stand a little taller than the taskbar's app icons, centred.
    g_keyH = (taskbarH * 60) / 100; if (g_keyH < 16) g_keyH = 16;
    g_padY = (taskbarH - g_keyH) / 2;
    g_keyW = g_keyH / 2; if (g_keyW < 9) g_keyW = 9; if (g_keyW > 30) g_keyW = 30;
    g_blackW = (g_keyW * 6) / 10; if (g_blackW < 5) g_blackW = 5;
    g_blackH = (g_keyH * 6) / 10;
    g_padX = 6;
    g_chipW = (g_keyH * 9) / 10;
    g_pianoX = g_padX + g_chipW + 8;
    g_stripW = g_pianoX + g_keyW * WHITE_COUNT + g_padX;
    g_stripH = taskbarH;
}

static int white_index_of(int midi) { for (int i = 0; i < WHITE_COUNT; i++) if (g_whiteMidi[i] == midi) return i; return -1; }

static RECT chip_rect(void) { RECT r = { g_padX, g_padY, g_padX + g_chipW, g_padY + g_keyH }; return r; }

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
static uint8_t *g_chipMask = NULL;      // ♪ glyph coverage, g_chipW × g_keyH

typedef struct { uint8_t r, g, b; } RGB8;

static const RGB8 CHROMA[12] = {
    {255, 50, 50}, {0,0,0}, {255,160,0}, {0,0,0}, {255,230,0}, {50,200,50},
    {0,0,0}, {50,120,255}, {0,0,0}, {130,50,200}, {0,0,0}, {180,80,255}
};
static const RGB8 FOCUS_BLUE = { 15, 107, 255 };
static const RGB8 STOP_RED = { 255, 31, 41 };

static RGB8 blend(RGB8 a, RGB8 b, double t) { RGB8 c = { (uint8_t)(a.r + (b.r - a.r) * t), (uint8_t)(a.g + (b.g - a.g) * t), (uint8_t)(a.b + (b.b - a.b) * t) }; return c; }

static void put(int x, int y, RGB8 c, double alpha) {
    if (x < 0 || y < 0 || x >= g_stripW || y >= g_stripH) return;
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

// Rasterise the ♪ once per layout: GDI text into a scratch DIB, luminance → mask.
static void render_chip_mask(void) {
    free(g_chipMask); g_chipMask = (uint8_t *)calloc((size_t)g_chipW * g_keyH, 1);
    HDC dc = CreateCompatibleDC(NULL);
    BITMAPINFO bi; memset(&bi, 0, sizeof bi);
    bi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
    bi.bmiHeader.biWidth = g_chipW; bi.bmiHeader.biHeight = -g_keyH;
    bi.bmiHeader.biPlanes = 1; bi.bmiHeader.biBitCount = 32; bi.bmiHeader.biCompression = BI_RGB;
    uint32_t *bits = NULL;
    HBITMAP bm = CreateDIBSection(dc, &bi, DIB_RGB_COLORS, (void **)&bits, NULL, 0);
    if (!bm) { DeleteDC(dc); return; }
    HGDIOBJ oldBm = SelectObject(dc, bm);
    memset(bits, 0, (size_t)g_chipW * g_keyH * 4);
    HFONT font = CreateFontW(-(g_keyH * 70) / 100, 0, 0, 0, FW_NORMAL, 0, 0, 0, DEFAULT_CHARSET,
                             OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, ANTIALIASED_QUALITY, DEFAULT_PITCH, L"Segoe UI Symbol");
    HGDIOBJ oldFont = SelectObject(dc, font);
    SetBkMode(dc, TRANSPARENT); SetTextColor(dc, RGB(255, 255, 255));
    RECT r = { 0, 0, g_chipW, g_keyH };
    DrawTextW(dc, L"\x266A", 1, &r, DT_CENTER | DT_VCENTER | DT_SINGLELINE | DT_NOCLIP);
    for (int i = 0; i < g_chipW * g_keyH; i++) g_chipMask[i] = (uint8_t)((bits[i] >> 8) & 0xFF);
    SelectObject(dc, oldFont); DeleteObject(font);
    SelectObject(dc, oldBm); DeleteObject(bm); DeleteDC(dc);
}

static int g_hoverMidi = -1;
static int g_hoverChip = 0;
static int g_mouseMidi = -1;
static int g_typing = 0;

static void paint(void) {
    memset(g_pixels, 0, sizeof(uint32_t) * g_stripW * g_stripH);
    const RGB8 whiteHi = { 62, 72, 82 }, whiteLo = { 44, 54, 62 };
    const RGB8 groove = { 140, 155, 165 };
    const RGB8 blackHi = { 170, 100, 255 }, blackLo = { 120, 60, 210 };
    const RGB8 black = { 0, 0, 0 }, white = { 255, 255, 255 };

    // ♪ chip: slate normally, focus blue while typing is live.
    RECT cr = chip_rect();
    RGB8 chipTop = g_typing ? FOCUS_BLUE : whiteHi, chipBot = g_typing ? blend(FOCUS_BLUE, black, 0.25) : whiteLo;
    if (g_hoverChip && !g_typing) { chipTop = blend(chipTop, white, 0.12); chipBot = blend(chipBot, white, 0.12); }
    RECT cframe = { cr.left - 1, cr.top - 1, cr.right + 1, cr.bottom + 1 };
    fill(cframe, groove, groove, 0.55);
    fill(cr, chipTop, chipBot, 1.0);
    if (g_chipMask)
        for (int y = 0; y < g_keyH; y++) for (int x = 0; x < g_chipW; x++) {
            uint8_t m = g_chipMask[y * g_chipW + x];
            if (m) put(cr.left + x, cr.top + y, white, m / 255.0 * 0.95);
        }

    // Backplate: the groove colour shows between keys and as a hairline frame;
    // it turns focus blue while typing so the whole strip reads as armed.
    RECT plate = { g_pianoX - 1, g_padY - 1, g_pianoX + g_keyW * WHITE_COUNT, g_padY + g_keyH + 1 };
    fill(plate, g_typing ? FOCUS_BLUE : groove, g_typing ? FOCUS_BLUE : groove, g_typing ? 0.9 : 0.55);

    for (int i = 0; i < WHITE_COUNT; i++) {
        int m = g_whiteMidi[i];
        RECT r = white_rect(i);
        RGB8 hi = whiteHi, lo = whiteLo;
        int lit = g_lit[m] > 0;
        if (lit) { RGB8 l = blend(CHROMA[m % 12], black, 0.22); hi = l; lo = l; }
        else if (m == g_hoverMidi) { hi = blend(whiteHi, white, 0.12); lo = blend(whiteLo, white, 0.12); }
        fill(r, hi, lo, 1.0);
        RECT s = { r.left + 2, r.bottom - 3, r.right - 2, r.bottom - 1 };
        fill(s, CHROMA[m % 12], CHROMA[m % 12], lit ? 0.0 : 0.9);
    }
    for (int i = 0; i < g_blackCount; i++) {
        int m = g_blackMidi[i];
        RECT r = black_rect(m);
        RGB8 hi = blackHi, lo = blackLo;
        if (g_lit[m] > 0) { hi = white; lo = blend(white, blackHi, 0.4); }
        else if (m == g_hoverMidi) { hi = blend(blackHi, white, 0.18); lo = blend(blackLo, white, 0.18); }
        RECT shadow = { r.left, r.top, r.right, r.bottom + 1 };
        fill(shadow, black, black, 0.35);
        fill(r, hi, lo, 1.0);
    }
    int L = g_pianoX - 1, R = g_pianoX + g_keyW * WHITE_COUNT - 1, T = g_padY - 1, B = g_padY + g_keyH;
    int corners[8][2] = { {L, T}, {R, T}, {L, B}, {R, B}, {cframe.left, cframe.top}, {cframe.right - 1, cframe.top}, {cframe.left, cframe.bottom - 1}, {cframe.right - 1, cframe.bottom - 1} };
    for (int c = 0; c < 8; c++) g_pixels[corners[c][1] * g_stripW + corners[c][0]] = 0;
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

static LRESULT CALLBACK ll_keyboard(int code, WPARAM wp, LPARAM lp) {
    if (code == HC_ACTION && g_typing) {
        KBDLLHOOKSTRUCT *k = (KBDLLHOOKSTRUCT *)lp;
        int down = (wp == WM_KEYDOWN || wp == WM_SYSKEYDOWN);
        if (!(k->flags & LLKHF_INJECTED) && k->vkCode < 256) {
            if (k->vkCode == VK_ESCAPE) { if (down) PostMessageW(g_strip, WM_TYPEESC, 0, 0); return 1; }
            if (k->vkCode == VK_OEM_COMMA || k->vkCode == VK_OEM_PERIOD) {
                if (down) PostMessageW(g_strip, WM_OCTAVE, k->vkCode == VK_OEM_PERIOD ? 1 : (WPARAM)-1, 0);
                return 1;
            }
            if (g_semitoneByVk[k->vkCode] != -128 && !(GetAsyncKeyState(VK_CONTROL) & 0x8000) && !(GetAsyncKeyState(VK_LWIN) & 0x8000)) {
                PostMessageW(g_strip, WM_TYPEKEY, k->vkCode, down);
                return 1;
            }
        }
    }
    return CallNextHookEx(g_hook, code, wp, lp);
}

static void lit_on(HWND hwnd, int midi, int vel) { if (midi < 0 || midi > 127) return; if (g_lit[midi]++ == 0) play_on(midi, vel); push_frame(hwnd); }
static void lit_off(HWND hwnd, int midi) { if (midi < 0 || midi > 127 || g_lit[midi] == 0) return; if (--g_lit[midi] == 0) play_off(midi); push_frame(hwnd); }

static void release_typed(HWND hwnd) {
    for (int vk = 0; vk < 256; vk++) if (g_vkDown[vk]) { g_vkDown[vk] = 0; lit_off(hwnd, 60 + g_semitoneByVk[vk] + (g_firstMidi - 60)); }
}

static void typing_set(HWND hwnd, int on) {
    if (on == g_typing) return;
    g_typing = on;
    if (!on) release_typed(hwnd);
    flash(on ? FOCUS_BLUE : STOP_RED);
    push_frame(hwnd);
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
    AppendMenuW(menu, MF_STRING | (autostart_enabled() ? MF_CHECKED : 0), IDM_AUTOSTART, L"Start at sign-in");
    AppendMenuW(menu, MF_SEPARATOR, 0, NULL);
    AppendMenuW(menu, MF_STRING, IDM_QUIT, L"Quit Menu Band");
    // TPM_RETURNCMD: a non-activating tool window is not a reliable WM_COMMAND
    // target, so take the choice back directly and dispatch it ourselves.
    SetForegroundWindow(hwnd);
    int chosen = TrackPopupMenu(menu, TPM_RIGHTBUTTON | TPM_BOTTOMALIGN | TPM_RETURNCMD | TPM_NONOTIFY, p.x, p.y, 0, hwnd, NULL);
    PostMessageW(hwnd, WM_NULL, 0, 0);
    DestroyMenu(menu);
    mb_log(L"menu: chose %d", chosen);
    if (chosen) SendMessageW(hwnd, WM_COMMAND, (WPARAM)chosen, 0);
}

static void mouse_down(HWND hwnd, int midi) {
    if (midi < 0 || midi == g_mouseMidi) return;
    if (g_mouseMidi >= 0) lit_off(hwnd, g_mouseMidi);
    g_mouseMidi = midi;
    lit_on(hwnd, midi, 100);
}

static void mouse_up(HWND hwnd) {
    if (g_mouseMidi >= 0) { lit_off(hwnd, g_mouseMidi); g_mouseMidi = -1; }
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
        if (tok[0] == '@') { g_program = atoi(tok + 1) & 127; midi_program(); continue; }
        int v = atoi(tok);
        if (v >= 200) g_playMs = v; else if (v >= 0 && v < 128 && g_playCount < 64) g_playNotes[g_playCount++] = v;
    }
    mb_log(L"play: %d notes, program %d, %d ms each", g_playCount, g_program, g_playMs);
    if (g_playCount) { SetTimer(hwnd, IDT_PLAY, g_playMs, NULL); step_play(hwnd); }
}

static void shift_octave(HWND hwnd, int dir) {
    int next = g_firstMidi + 12 * dir;
    if (next < 36 || next > 84) return;
    mouse_up(hwnd);
    release_typed(hwnd);
    g_firstMidi = next; build_keys(); push_frame(hwnd);
}

static LRESULT CALLBACK wndproc(HWND hwnd, UINT msg, WPARAM wp, LPARAM lp) {
    switch (msg) {
    case WM_CREATE:
        SetTimer(hwnd, IDT_TRACK, 400, NULL);
        return 0;
    case WM_TIMER:
        if (wp == IDT_TRACK) { track(hwnd, 0); poll_play_file(hwnd); }
        if (wp == IDT_PLAY) step_play(hwnd);
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
        if (chip_at(x, y)) { typing_set(hwnd, !g_typing); return 0; }
        SetCapture(hwnd);
        int midi = key_at(x, y);
        mouse_down(hwnd, midi);
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
    case WM_RBUTTONUP:
        show_menu(hwnd);
        return 0;
    case WM_MOUSEWHEEL:
        shift_octave(hwnd, GET_WHEEL_DELTA_WPARAM(wp) > 0 ? 1 : -1);
        return 0;
    case WM_OCTAVE:
        shift_octave(hwnd, (int)(intptr_t)wp);
        return 0;
    case WM_TYPEKEY: {
        int vk = (int)wp, down = (int)lp;
        if (vk < 0 || vk > 255) return 0;
        int midi = 60 + g_semitoneByVk[vk] + (g_firstMidi - 60);
        if (down) { if (!g_vkDown[vk]) { g_vkDown[vk] = 1; lit_on(hwnd, midi, 100); } }
        else if (g_vkDown[vk]) { g_vkDown[vk] = 0; lit_off(hwnd, midi); }
        return 0;
    }
    case WM_TYPEESC:
        typing_set(hwnd, 0);
        return 0;
    case WM_TRAY:
        if (LOWORD(lp) == WM_RBUTTONUP || LOWORD(lp) == WM_LBUTTONUP) show_menu(hwnd);
        return 0;
    case WM_COMMAND: {
        int id = LOWORD(wp);
        if (id == IDM_QUIT) { mb_log(L"menu: quit"); DestroyWindow(hwnd); return 0; }
        if (id == IDM_TYPING) { typing_set(hwnd, !g_typing); return 0; }
        if (id == IDM_AUTOSTART) { autostart_set(!autostart_enabled()); return 0; }
        if (id >= IDM_PROGRAM_BASE && id < IDM_PROGRAM_BASE + 128) { g_program = id - IDM_PROGRAM_BASE; midi_program(); mb_log(L"program -> %d (%s)", g_program, GM_NAMES[g_program]); }
        if (id >= IDM_MIDI_BASE && id < IDM_MIDI_BASE + 40) midi_select(id - IDM_MIDI_BASE - 1);
        return 0;
    }
    case WM_DISPLAYCHANGE:
    case WM_DPICHANGED:
    case WM_SETTINGCHANGE:
        track(hwnd, 1);
        return 0;
    case WM_DESTROY:
        mb_log(L"WM_DESTROY on strip");
        typing_set(hwnd, 0);
        Shell_NotifyIconW(NIM_DELETE, &g_nid);
        if (g_hook) UnhookWindowsHookEx(g_hook);
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

    build_keys();
    build_keymap();
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
    g_nid.hIcon = LoadIcon(NULL, IDI_APPLICATION);
    wcscpy_s(g_nid.szTip, 128, L"Menu Band");
    Shell_NotifyIconW(NIM_ADD, &g_nid);

    g_hook = SetWindowsHookExW(WH_KEYBOARD_LL, ll_keyboard, hInst, 0);
    mb_log(L"start: strip=%p flash=%p hook=%p audio=%d (lastErr=%lu)", (void *)hwnd, (void *)g_flash, (void *)g_hook, (int)g_audioRunning, GetLastError());
    track(hwnd, 1);
    mb_log(L"start: placed at %d,%d size %dx%d taskbarH=%d", g_posX, g_posY, g_stripW, g_stripH, g_lastTaskbarH);

    MSG m;
    while (GetMessageW(&m, NULL, 0, 0)) { TranslateMessage(&m); DispatchMessageW(&m); }
    if (g_client) g_client->lpVtbl->Stop(g_client);
    CloseHandle(single);
    return 0;
}
