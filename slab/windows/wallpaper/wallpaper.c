// wallpaper.c — the slab live wallpaper for Windows 11.
//
// The Windows counterpart of slab/blueberry-wallpaper (the Mac fleet's
// desktop): the slab dark gradient with the pals marks drifting up the
// screen, swaying and spinning, in the five tints. Windows has no wallpaper
// API that animates, so this is a window parked BEHIND the desktop icons via
// the Progman/WorkerW trick: ask Progman (0x052C) to spawn a WorkerW between
// the wallpaper and the icon layer, then become that WorkerW's child.
//
// Software rendering only: a 32-bit DIB, marks drawn by inverse-mapping each
// destination pixel through the rotation into the sprite's alpha mask, and
// only the rectangles that changed are restored and blitted each frame. The
// pal sprite is pals_sprite.h, generated from fedac/plymouth/pals.png.
//
// Pauses while the session is locked, the display is off, or a full-screen
// app is in front. Single instance. Build: build.ps1 (VS 2022 Build Tools).

#define UNICODE
#define _UNICODE
#define WIN32_LEAN_AND_MEAN
#define _USE_MATH_DEFINES
#include <windows.h>
#include <shellapi.h>
#include <wtsapi32.h>
#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include "pals_sprite.h"

// ─────────────────────────── marks ───────────────────────────────
// The eight MarkSpec rows from BlueberryWallpaper.swift, then five smaller
// extras so it reads as "more scatters" than the Mac's eight.
typedef struct { double phase, width, riseSeconds, spinSeconds, sway; int reverse, variant; } MarkSpec;
static const MarkSpec SPECS[] = {
    { 0.03,  94, 31, 2.4, 70, 0, 0 },
    { 0.16,  58, 25, 2.8, 48, 1, 2 },
    { 0.29, 122, 38, 3.2, 95, 0, 4 },
    { 0.41,  72, 28, 2.6, 56, 1, 1 },
    { 0.54, 108, 35, 3.0, 82, 0, 3 },
    { 0.66,  48, 23, 2.2, 42, 1, 0 },
    { 0.77,  82, 30, 2.9, 64, 0, 2 },
    { 0.88,  64, 27, 2.5, 50, 1, 4 },
    // extras
    { 0.09,  40, 26, 2.7, 38, 1, 3 },
    { 0.23,  34, 22, 2.3, 30, 0, 1 },
    { 0.47,  44, 29, 3.1, 44, 1, 4 },
    { 0.61,  38, 24, 2.6, 36, 0, 2 },
    { 0.95,  52, 33, 2.9, 46, 1, 0 },
};
#define MARK_COUNT ((int)(sizeof SPECS / sizeof SPECS[0]))

typedef struct { uint8_t r, g, b; } RGB8;
static const RGB8 TINTS[5] = { {255,105,180}, {90,220,230}, {170,255,120}, {255,220,90}, {200,160,255} };
static const double MARK_ALPHA = 0.55;

// ─────────────────────────── state ───────────────────────────────
static HWND g_hwnd = NULL, g_parent = NULL;
static int g_w = 0, g_h = 0;            // frame size (virtual screen, physical px)
static int g_vx = 0, g_vy = 0;          // virtual screen origin
static double g_scale = 1.0;            // px per Mac point (DPI / 96)
static uint32_t *g_bg = NULL;           // gradient + glow, drawn once per size
static uint32_t *g_fb = NULL;           // frame
static HBITMAP g_dib = NULL;
static HDC g_memDC = NULL;
static RECT g_prev[MARK_COUNT];         // last frame's dirty boxes
static int g_prevCount = 0;
static volatile int g_paused = 0;       // locked / display off / full-screen
static int g_locked = 0, g_displayOff = 0;
static LARGE_INTEGER g_freq, g_t0;
static const double FPS = 24.0;   // 30 cost ~12 % of a core through the layered redirection; 24 reads the same for slow drift

static double now_seconds(void) { LARGE_INTEGER t; QueryPerformanceCounter(&t); return (double)(t.QuadPart - g_t0.QuadPart) / (double)g_freq.QuadPart; }

// ─────────────────────── background ──────────────────────────────
static void build_background(void) {
    const double top[3] = { 0.045, 0.09, 0.16 }, bot[3] = { 0.12, 0.07, 0.19 };
    // Glow: the static wallpaper's ellipse (15–85 % wide, 55–135 % tall) of
    // (120,60,190) at alpha 70/255, blurred wide — approximated by a smooth
    // radial falloff from its centre.
    const double gcx = 0.5 * g_w, gcy = 0.95 * g_h, grx = 0.35 * g_w + 0.07 * g_w, gry = 0.40 * g_h + 0.09 * g_h;
    for (int y = 0; y < g_h; y++) {
        double t = (double)y / (g_h > 1 ? g_h - 1 : 1);
        double r0 = top[0] + (bot[0] - top[0]) * t, g0 = top[1] + (bot[1] - top[1]) * t, b0 = top[2] + (bot[2] - top[2]) * t;
        for (int x = 0; x < g_w; x++) {
            double dx = (x - gcx) / grx, dy = (y - gcy) / gry, d = dx * dx + dy * dy;
            double a = d >= 1.0 ? 0.0 : (70.0 / 255.0) * (1.0 - d) * (1.0 - d);   // smooth edge, like a wide blur
            double r = r0 * (1 - a) + (120 / 255.0) * a, g = g0 * (1 - a) + (60 / 255.0) * a, b = b0 * (1 - a) + (190 / 255.0) * a;
            g_bg[y * g_w + x] = 0xFF000000u | ((uint32_t)(r * 255 + 0.5) << 16) | ((uint32_t)(g * 255 + 0.5) << 8) | (uint32_t)(b * 255 + 0.5);
        }
    }
}

static void ensure_surface(void) {
    if (g_dib) { DeleteObject(g_dib); g_dib = NULL; }
    if (!g_memDC) g_memDC = CreateCompatibleDC(NULL);
    BITMAPINFO bi; memset(&bi, 0, sizeof bi);
    bi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
    bi.bmiHeader.biWidth = g_w; bi.bmiHeader.biHeight = -g_h;
    bi.bmiHeader.biPlanes = 1; bi.bmiHeader.biBitCount = 32; bi.bmiHeader.biCompression = BI_RGB;
    g_dib = CreateDIBSection(g_memDC, &bi, DIB_RGB_COLORS, (void **)&g_fb, NULL, 0);
    SelectObject(g_memDC, g_dib);
    free(g_bg); g_bg = (uint32_t *)malloc((size_t)g_w * g_h * 4);
    build_background();
    memcpy(g_fb, g_bg, (size_t)g_w * g_h * 4);
    g_prevCount = 0;
}

// ───────────────────────── drawing ───────────────────────────────
// Draw one mark: sprite scaled to `width` px wide, rotated by `angle`, tinted,
// centred at (cx, cy). Returns its bounding box (clipped) in *box.
static void draw_mark(double cx, double cy, double width, double angle, RGB8 tint, RECT *box) {
    double scale = width / PALS_W;                       // dest px per sprite px
    double hw = width / 2, hh = (PALS_H * scale) / 2;
    double rad = sqrt(hw * hw + hh * hh) + 1;
    int x0 = (int)floor(cx - rad), x1 = (int)ceil(cx + rad), y0 = (int)floor(cy - rad), y1 = (int)ceil(cy + rad);
    if (x0 < 0) x0 = 0; if (y0 < 0) y0 = 0; if (x1 > g_w) x1 = g_w; if (y1 > g_h) y1 = g_h;
    box->left = x0; box->top = y0; box->right = x1; box->bottom = y1;
    if (x1 <= x0 || y1 <= y0) return;
    float c = (float)cos(angle), sn = (float)sin(angle), inv = (float)(1.0 / scale);
    float du = c * inv, dv = -sn * inv;                  // sprite-space step per dest pixel along x
    float tr = (float)(tint.r * MARK_ALPHA), tg = (float)(tint.g * MARK_ALPHA), tb = (float)(tint.b * MARK_ALPHA);
    const float k = (float)MARK_ALPHA / 255.0f;
    for (int y = y0; y < y1; y++) {
        float dy = (float)(y + 0.5 - cy), dx0 = (float)(x0 + 0.5 - cx);
        float u = (c * dx0 + sn * dy) * inv + PALS_W * 0.5f, v = (-sn * dx0 + c * dy) * inv + PALS_H * 0.5f;
        uint32_t *row = g_fb + (size_t)y * g_w;
        for (int x = x0; x < x1; x++, u += du, v += dv) {
            if (u < 0 || v < 0 || u >= PALS_W || v >= PALS_H) continue;
            unsigned a8 = PALS_ALPHA[(int)v * PALS_W + (int)u];
            if (!a8) continue;
            float a = a8 * k, ia = 1.0f - a, af = a8 / 255.0f;
            uint32_t p = row[x];
            uint32_t r = (uint32_t)(((p >> 16) & 0xFF) * ia + tr * af + 0.5f);
            uint32_t g = (uint32_t)(((p >> 8) & 0xFF) * ia + tg * af + 0.5f);
            uint32_t b = (uint32_t)((p & 0xFF) * ia + tb * af + 0.5f);
            row[x] = 0xFF000000u | (r << 16) | (g << 8) | b;
        }
    }
}

static void restore_box(const RECT *r) {
    if (r->right <= r->left || r->bottom <= r->top) return;
    size_t bytes = (size_t)(r->right - r->left) * 4;
    for (int y = r->top; y < r->bottom; y++) memcpy(g_fb + (size_t)y * g_w + r->left, g_bg + (size_t)y * g_w + r->left, bytes);
}

static void blit_box(HDC dc, const RECT *r) {
    if (r->right <= r->left || r->bottom <= r->top) return;
    BitBlt(dc, r->left, r->top, r->right - r->left, r->bottom - r->top, g_memDC, r->left, r->top, SRCCOPY);
}

static void render_frame(void) {
    if (!g_fb || !g_hwnd) return;
    double t = now_seconds();
    RECT cur[MARK_COUNT]; int curCount = 0;
    // Put the background back where last frame's marks were.
    for (int i = 0; i < g_prevCount; i++) restore_box(&g_prev[i]);
    for (int i = 0; i < MARK_COUNT; i++) {
        const MarkSpec *m = &SPECS[i];
        double width = m->width * g_scale;
        double rise = fmod(t / m->riseSeconds + m->phase, 1.0);           // 0 = below bottom, 1 = above top
        double span = g_h + 2 * width;
        double cy = g_h + width - rise * span;
        double cx = m->phase * g_w + m->sway * g_scale * sin(2 * M_PI * t / (m->riseSeconds * 0.37) + m->phase * 6.283);
        double angle = 2 * M_PI * t / m->spinSeconds * (m->reverse ? -1 : 1);
        if (cy + width < 0 || cy - width > g_h) continue;
        draw_mark(cx, cy, width, angle, TINTS[m->variant % 5], &cur[curCount]);
        if (cur[curCount].right > cur[curCount].left && cur[curCount].bottom > cur[curCount].top) curCount++;
    }
    HDC dc = GetDC(g_hwnd);
    if (dc) {
        for (int i = 0; i < g_prevCount; i++) blit_box(dc, &g_prev[i]);
        for (int i = 0; i < curCount; i++) blit_box(dc, &cur[i]);
        ReleaseDC(g_hwnd, dc);
    }
    memcpy(g_prev, cur, sizeof(RECT) * curCount); g_prevCount = curCount;
}

// ───────────────────── behind the icons ──────────────────────────
static HWND g_workerW = NULL;
static BOOL CALLBACK find_worker(HWND top, LPARAM lp) {
    (void)lp;
    HWND defView = FindWindowExW(top, NULL, L"SHELLDLL_DefView", NULL);
    if (defView) { HWND next = FindWindowExW(NULL, top, L"WorkerW", NULL); if (next) g_workerW = next; }
    return TRUE;
}

static void measure(void) {
    g_vx = GetSystemMetrics(SM_XVIRTUALSCREEN); g_vy = GetSystemMetrics(SM_YVIRTUALSCREEN);
    g_w = GetSystemMetrics(SM_CXVIRTUALSCREEN); g_h = GetSystemMetrics(SM_CYVIRTUALSCREEN);
    if (g_w < 16) g_w = 16; if (g_h < 16) g_h = 16;
    UINT dpi = GetDpiForWindow(g_hwnd ? g_hwnd : GetDesktopWindow()); if (dpi < 72) dpi = 96;
    g_scale = dpi / 96.0;
}

static void wp_log(const wchar_t *fmt, ...) {
    wchar_t path[MAX_PATH]; GetTempPathW(MAX_PATH, path); wcscat_s(path, MAX_PATH, L"slab-wallpaper.log");
    FILE *f = NULL; _wfopen_s(&f, path, L"a"); if (!f) return;
    SYSTEMTIME st; GetLocalTime(&st); fwprintf(f, L"%02d:%02d:%02d ", st.wHour, st.wMinute, st.wSecond);
    va_list ap; va_start(ap, fmt); vfwprintf(f, fmt, ap); va_end(ap); fwprintf(f, L"\n"); fclose(f);
}

static void log_children(HWND parent, int depth) {
    for (HWND c = GetWindow(parent, GW_CHILD); c; c = GetWindow(c, GW_HWNDNEXT)) {
        wchar_t cls[64]; GetClassNameW(c, cls, 64); RECT r; GetWindowRect(c, &r);
        wp_log(L"%*s%p %s vis=%d rect=%d,%d,%d,%d style=%08lX ex=%08lX", depth * 2, L"", (void *)c, cls, IsWindowVisible(c), r.left, r.top, r.right, r.bottom,
               (unsigned long)GetWindowLongPtrW(c, GWL_STYLE), (unsigned long)GetWindowLongPtrW(c, GWL_EXSTYLE));
        if (depth < 2) log_children(c, depth + 1);
    }
}

static int g_float = 0;   // --float: ordinary bottom-most window, for checking that rendering works at all
static int g_mode = 4;    // --mode N: 4 layered Progman child under DefView (24H2 default), 2 WorkerW child, 1 plain Progman child, 3 DefView child

static void attach(void) {
    HWND progman = FindWindowW(L"Progman", NULL);
    if (!progman) { wp_log(L"attach: no Progman"); return; }
    if (g_float) {
        g_parent = NULL;
        SetWindowPos(g_hwnd, HWND_BOTTOM, g_vx, g_vy, g_w, g_h, SWP_NOACTIVATE | SWP_SHOWWINDOW);
        wp_log(L"attach: --float, top-level bottom-most");
        return;
    }
    wp_log(L"Progman %p children before:", (void *)progman); log_children(progman, 1);
    // Top-level neighbours: every WorkerW and where Progman sits among them.
    for (HWND t = GetTopWindow(NULL); t; t = GetWindow(t, GW_HWNDNEXT)) {
        wchar_t cls[64]; GetClassNameW(t, cls, 64);
        if (!wcscmp(cls, L"WorkerW") || t == progman) {
            RECT r; GetWindowRect(t, &r);
            wp_log(L"top-level %p %s vis=%d rect=%d,%d,%d,%d style=%08lX ex=%08lX children:", (void *)t, cls, IsWindowVisible(t), r.left, r.top, r.right, r.bottom,
                   (unsigned long)GetWindowLongPtrW(t, GWL_STYLE), (unsigned long)GetWindowLongPtrW(t, GWL_EXSTYLE));
            log_children(t, 2);
        }
    }
    // Explorer reference-counts this request: lParam 1 spawns the wallpaper
    // WorkerW (a child of Progman on 24H2+, a top-level sibling before), lParam 0
    // RELEASES it and Explorer destroys it a moment later — so only the legacy
    // zero form is sent when the first produced nothing.
    DWORD_PTR res = 0;
    SendMessageTimeoutW(progman, 0x052C, 0xD, 0x1, SMTO_NORMAL, 1000, &res);
    Sleep(50);
    HWND defViewInProgman = FindWindowExW(progman, NULL, L"SHELLDLL_DefView", NULL);
    HWND wwChild = FindWindowExW(progman, NULL, L"WorkerW", NULL);
    if (!wwChild && !defViewInProgman) { SendMessageTimeoutW(progman, 0x052C, 0, 0, SMTO_NORMAL, 1000, &res); Sleep(50); }
    SetWindowLongPtrW(g_hwnd, GWL_STYLE, WS_CHILD | WS_VISIBLE);
    if (defViewInProgman && g_mode == 4) {
        // Raised desktop (24H2+): Progman has no redirection bitmap, so only a
        // LAYERED child gets composed. Opaque layered child of Progman, z-ordered
        // just beneath the icons' DefView, with Explorer's wallpaper WorkerW
        // pushed to the bottom.
        g_parent = progman;
        HWND prevParent = SetParent(g_hwnd, progman);
        SetWindowLongPtrW(g_hwnd, GWL_EXSTYLE, GetWindowLongPtrW(g_hwnd, GWL_EXSTYLE) | WS_EX_LAYERED);
        SetLayeredWindowAttributes(g_hwnd, 0, 255, LWA_ALPHA);
        RECT pr; GetWindowRect(progman, &pr);
        SetWindowPos(g_hwnd, defViewInProgman, g_vx - pr.left, g_vy - pr.top, g_w, g_h, SWP_NOACTIVATE | SWP_SHOWWINDOW);
        if (wwChild) SetWindowPos(wwChild, HWND_BOTTOM, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE);
        wp_log(L"attach: mode 4, layered child of Progman %p beneath DefView %p, WorkerW %p to bottom (SetParent returned %p)", (void *)progman, (void *)defViewInProgman, (void *)wwChild, (void *)prevParent);
        wp_log(L"Progman children after:"); log_children(progman, 1);
        InvalidateRect(g_hwnd, NULL, FALSE); return;
    }
    if (defViewInProgman && g_mode == 2) {
        // 24H2+: live inside Explorer's own wallpaper layer, the WorkerW child of
        // Progman that sits below the icons' DefView.
        if (wwChild) {
            g_parent = wwChild;
            HWND prevParent = SetParent(g_hwnd, wwChild);
            RECT pr; GetWindowRect(wwChild, &pr);
            SetWindowPos(g_hwnd, HWND_TOP, g_vx - pr.left, g_vy - pr.top, g_w, g_h, SWP_NOACTIVATE | SWP_SHOWWINDOW);
            wp_log(L"attach: mode 2, child of WorkerW %p (SetParent returned %p) at %d,%d %dx%d", (void *)wwChild, (void *)prevParent, g_vx - pr.left, g_vy - pr.top, g_w, g_h);
            InvalidateRect(g_hwnd, NULL, FALSE); return;
        }
        wp_log(L"attach: mode 2 wanted but no WorkerW child; falling back to mode 1");
    }
    if (defViewInProgman && g_mode == 3) {
        // Child of DefView, beneath its icon list view (whose background is transparent).
        HWND lv = FindWindowExW(defViewInProgman, NULL, L"SysListView32", NULL);
        g_parent = defViewInProgman; SetParent(g_hwnd, defViewInProgman); RECT pr; GetWindowRect(defViewInProgman, &pr);
        SetWindowPos(g_hwnd, lv ? lv : HWND_BOTTOM, g_vx - pr.left, g_vy - pr.top, g_w, g_h, SWP_NOACTIVATE | SWP_SHOWWINDOW);
        wp_log(L"attach: mode 3, child of DefView %p beneath listview %p", (void *)defViewInProgman, (void *)lv); InvalidateRect(g_hwnd, NULL, FALSE); return;
    }
    if (defViewInProgman) {
        // Windows 11 24H2+: the icon layer (DefView) and the wallpaper WorkerW are
        // both children of Progman. Sit between them: child of Progman, placed
        // directly beneath DefView in z-order, so icons stay on top of us and we
        // cover the wallpaper WorkerW.
        g_parent = progman;
        SetParent(g_hwnd, progman);
        RECT pr; GetWindowRect(progman, &pr);
        SetWindowPos(g_hwnd, defViewInProgman, g_vx - pr.left, g_vy - pr.top, g_w, g_h, SWP_NOACTIVATE | SWP_SHOWWINDOW);
        wp_log(L"attach: 24H2 layout, child of Progman %p beneath DefView %p at %d,%d %dx%d", (void *)progman, (void *)defViewInProgman, g_vx - pr.left, g_vy - pr.top, g_w, g_h);
        wp_log(L"Progman children after:"); log_children(progman, 1);
    } else {
        // Classic layout: a top-level WorkerW appears right after the one that
        // holds DefView; we become its child.
        g_workerW = NULL; EnumWindows(find_worker, 0);
        HWND host = g_workerW ? g_workerW : progman;
        g_parent = host;
        SetParent(g_hwnd, host);
        RECT pr; GetWindowRect(host, &pr);
        SetWindowPos(g_hwnd, NULL, g_vx - pr.left, g_vy - pr.top, g_w, g_h, SWP_NOACTIVATE | SWP_SHOWWINDOW | SWP_NOZORDER);
        wp_log(L"attach: classic layout, child of %s %p at %d,%d %dx%d", g_workerW ? L"WorkerW" : L"Progman", (void *)host, g_vx - pr.left, g_vy - pr.top, g_w, g_h);
    }
    InvalidateRect(g_hwnd, NULL, FALSE);
}

static int fullscreen_app_in_front(void) {
    QUERY_USER_NOTIFICATION_STATE st;
    if (SUCCEEDED(SHQueryUserNotificationState(&st)))
        return st == QUNS_BUSY || st == QUNS_RUNNING_D3D_FULL_SCREEN || st == QUNS_PRESENTATION_MODE;
    return 0;
}

// ─────────────────────────── window ──────────────────────────────
#define IDT_FRAME 1
#define IDT_WATCH 2
static const GUID GUID_CONSOLE_DISPLAY_STATE_ = { 0x6fe69556, 0x704a, 0x47a0, { 0x8f, 0x24, 0xc2, 0x8d, 0x93, 0x6f, 0xda, 0x47 } };

static LRESULT CALLBACK wndproc(HWND hwnd, UINT msg, WPARAM wp, LPARAM lp) {
    switch (msg) {
    case WM_PAINT: {
        PAINTSTRUCT ps; HDC dc = BeginPaint(hwnd, &ps);
        if (g_fb) BitBlt(dc, ps.rcPaint.left, ps.rcPaint.top, ps.rcPaint.right - ps.rcPaint.left, ps.rcPaint.bottom - ps.rcPaint.top, g_memDC, ps.rcPaint.left, ps.rcPaint.top, SRCCOPY);
        EndPaint(hwnd, &ps);
        return 0;
    }
    case WM_ERASEBKGND:
        return 1;
    case WM_TIMER:
        if (wp == IDT_FRAME) { if (!g_paused) render_frame(); return 0; }
        if (wp == IDT_WATCH) {
            int fs = fullscreen_app_in_front();
            g_paused = g_locked || g_displayOff || fs;
            // Explorer restarted: our host is gone, re-attach.
            if (!g_float && (!g_parent || !IsWindow(g_parent))) { measure(); attach(); }
            return 0;
        }
        return 0;
    case WM_WTSSESSION_CHANGE:
        if (wp == WTS_SESSION_LOCK) g_locked = 1;
        if (wp == WTS_SESSION_UNLOCK) { g_locked = 0; InvalidateRect(hwnd, NULL, FALSE); }
        return 0;
    case WM_POWERBROADCAST:
        if (wp == PBT_POWERSETTINGCHANGE) {
            POWERBROADCAST_SETTING *s = (POWERBROADCAST_SETTING *)lp;
            if (IsEqualGUID(&s->PowerSetting, &GUID_CONSOLE_DISPLAY_STATE_)) g_displayOff = (s->Data[0] == 0);
        }
        return TRUE;
    case WM_DISPLAYCHANGE:
    case WM_DPICHANGED:
        measure(); ensure_surface(); attach();
        return 0;
    case WM_CLOSE:
        DestroyWindow(hwnd);
        return 0;
    case WM_DESTROY:
        WTSUnRegisterSessionNotification(hwnd);
        PostQuitMessage(0);
        return 0;
    }
    return DefWindowProcW(hwnd, msg, wp, lp);
}

int WINAPI wWinMain(HINSTANCE hInst, HINSTANCE prev, PWSTR cmdline, int show) {
    (void)prev; (void)show;
    g_float = cmdline && wcsstr(cmdline, L"--float") != NULL;
    { const wchar_t *m = cmdline ? wcsstr(cmdline, L"--mode") : NULL; if (m) g_mode = _wtoi(m + 6); if (g_mode < 1 || g_mode > 4) g_mode = 4; }
    SetProcessDpiAwarenessContext(DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2);
    HANDLE single = CreateMutexW(NULL, TRUE, L"computer.aesthetic.slab.wallpaper");
    if (GetLastError() == ERROR_ALREADY_EXISTS) return 0;
    QueryPerformanceFrequency(&g_freq); QueryPerformanceCounter(&g_t0);

    WNDCLASSW wc; memset(&wc, 0, sizeof wc);
    wc.lpfnWndProc = wndproc; wc.hInstance = hInst; wc.lpszClassName = L"SlabWallpaper";
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    RegisterClassW(&wc);
    g_hwnd = CreateWindowExW(WS_EX_TOOLWINDOW | WS_EX_NOACTIVATE | WS_EX_LAYERED, L"SlabWallpaper", L"slab wallpaper", WS_POPUP,
                             0, 0, 16, 16, NULL, NULL, hInst, NULL);
    SetLayeredWindowAttributes(g_hwnd, 0, 255, LWA_ALPHA);
    measure();
    ensure_surface();
    attach();
    WTSRegisterSessionNotification(g_hwnd, NOTIFY_FOR_THIS_SESSION);
    RegisterPowerSettingNotification(g_hwnd, &GUID_CONSOLE_DISPLAY_STATE_, DEVICE_NOTIFY_WINDOW_HANDLE);
    SetTimer(g_hwnd, IDT_FRAME, (UINT)(1000.0 / FPS), NULL);
    SetTimer(g_hwnd, IDT_WATCH, 1000, NULL);

    MSG m;
    while (GetMessageW(&m, NULL, 0, 0)) { TranslateMessage(&m); DispatchMessageW(&m); }
    CloseHandle(single);
    return 0;
}
