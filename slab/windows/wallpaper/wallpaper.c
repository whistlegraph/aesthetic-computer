// wallpaper.c — the slab wallpaper for Windows 11.
//
// The Windows counterpart of slab/blueberry-wallpaper (the Mac fleet's
// desktop). The background is the static scattered-pals composition
// (~\Pictures\ac-wallpaper.bmp, converted from the PNG by deploy.sh; the slab
// gradient is drawn if it is missing). On top, ONE animated element: a
// lavender pal mark in the lower right that turns once every 40 s, rendered
// with bilinear sampling at a subpixel position so it never jitters, and
// repainting only its own rectangle — the rest of the screen never redraws.
//
// Windows has no wallpaper API that animates, so this is a window parked
// BEHIND the desktop icons: on the Windows 11 24H2+ "raised desktop" Progman
// has no redirection bitmap, so the window must be a LAYERED child of Progman
// z-ordered beneath SHELLDLL_DefView, with Explorer's wallpaper WorkerW pushed
// to the bottom. 30 fps on AC, 15 on battery; pauses while locked, display
// off, or a full-screen app is in front. `--static` skips the animation.
// Single instance. Build: build.ps1 (VS 2022 Build Tools).

#define UNICODE
#define _UNICODE
#define WIN32_LEAN_AND_MEAN
#define _USE_MATH_DEFINES
#define _CRT_SECURE_NO_WARNINGS
#include <windows.h>
#include <shellapi.h>
#include <wtsapi32.h>
#include <shellscalingapi.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include "pals_sprite.h"

typedef struct { uint8_t r, g, b; } RGB8;
static const RGB8 LAVENDER = { 200, 160, 255 };
static const RGB8 VIOLET_INK = { 118, 74, 206 };   // the mark on the light ground
static int g_light = 0;                            // Windows light theme (SystemUsesLightTheme)

static void read_theme(void) {
    HKEY k; DWORD v = 0, size = sizeof v, type; g_light = 0;
    if (RegOpenKeyExW(HKEY_CURRENT_USER, L"Software\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize", 0, KEY_READ, &k) == ERROR_SUCCESS) {
        if (RegQueryValueExW(k, L"SystemUsesLightTheme", NULL, &type, (BYTE *)&v, &size) == ERROR_SUCCESS && type == REG_DWORD) g_light = v != 0;
        RegCloseKey(k);
    }
}
static const double MARK_ALPHA = 0.55;
static const double MARK_WIDTH_PT = 110;     // Mac points; × DPI scale
static const double MARK_CX = 0.86, MARK_CY = 0.80;   // fraction of the screen
static const double TURN_SECONDS = 40.0;

// ─────────────────────────── state ───────────────────────────────
static HWND g_hwnd = NULL, g_parent = NULL;
static int g_w = 0, g_h = 0, g_vx = 0, g_vy = 0;
static double g_scale = 1.0;
static uint32_t *g_bg = NULL;           // the still composition
static uint32_t *g_fb = NULL;           // frame (DIB)
static HBITMAP g_dib = NULL;
static HDC g_memDC = NULL;
static int g_locked = 0, g_displayOff = 0, g_paused = 0;
static int g_static = 0, g_float = 0;
static double g_fps = 30.0, g_fpsOverride = 0;
static LARGE_INTEGER g_freq, g_t0;
static double g_tRender = 0, g_tBlit = 0, g_lastStat = 0; static int g_frames = 0;
static RECT g_markBox = { 0, 0, 0, 0 };
static HDC g_winDC = NULL;

static double now_seconds(void) { LARGE_INTEGER t; QueryPerformanceCounter(&t); return (double)(t.QuadPart - g_t0.QuadPart) / (double)g_freq.QuadPart; }

static void wp_log(const wchar_t *fmt, ...) {
    wchar_t path[MAX_PATH]; GetTempPathW(MAX_PATH, path); wcscat_s(path, MAX_PATH, L"slab-wallpaper.log");
    FILE *f = NULL; _wfopen_s(&f, path, L"a"); if (!f) return;
    SYSTEMTIME st; GetLocalTime(&st); fwprintf(f, L"%02d:%02d:%02d ", st.wHour, st.wMinute, st.wSecond);
    va_list ap; va_start(ap, fmt); vfwprintf(f, fmt, ap); va_end(ap); fwprintf(f, L"\n"); fclose(f);
}

// ─────────────────────── background ──────────────────────────────
// The slab gradient. Dark: night blue to plum with a violet glow low in the
// middle. Light: the same shape in daylight — paper white to pale lilac.
static void draw_gradient_as(uint32_t *dst, int light) {
    const double topD[3] = { 0.045, 0.09, 0.16 }, botD[3] = { 0.12, 0.07, 0.19 }, glowD[3] = { 120 / 255.0, 60 / 255.0, 190 / 255.0 };
    const double topL[3] = { 0.968, 0.962, 0.985 }, botL[3] = { 0.930, 0.910, 0.968 }, glowL[3] = { 206 / 255.0, 176 / 255.0, 246 / 255.0 };
    const double *top = light ? topL : topD, *bot = light ? botL : botD, *glow = light ? glowL : glowD;
    const double glowA = (light ? 110.0 : 70.0) / 255.0;
    const double gcx = 0.5 * g_w, gcy = 0.95 * g_h, grx = 0.42 * g_w, gry = 0.49 * g_h;
    for (int y = 0; y < g_h; y++) {
        double t = (double)y / (g_h > 1 ? g_h - 1 : 1);
        double r0 = top[0] + (bot[0] - top[0]) * t, g0 = top[1] + (bot[1] - top[1]) * t, b0 = top[2] + (bot[2] - top[2]) * t;
        for (int x = 0; x < g_w; x++) {
            double dx = (x - gcx) / grx, dy = (y - gcy) / gry, d = dx * dx + dy * dy;
            double a = d >= 1.0 ? 0.0 : glowA * (1.0 - d) * (1.0 - d);
            double r = r0 * (1 - a) + glow[0] * a, g = g0 * (1 - a) + glow[1] * a, b = b0 * (1 - a) + glow[2] * a;
            dst[y * g_w + x] = 0xFF000000u | ((uint32_t)(r * 255 + 0.5) << 16) | ((uint32_t)(g * 255 + 0.5) << 8) | (uint32_t)(b * 255 + 0.5);
        }
    }
}

static void draw_gradient(uint32_t *dst) { draw_gradient_as(dst, g_light); }

// Daylight: the composition is the dark gradient plus the pals' outlines, so
// subtracting the gradient recovers the ink. Lay that ink, deepened, onto the
// light gradient — same pals, same places, dark lines on paper.
static void lighten_composition(void) {
    size_t n = (size_t)g_w * g_h;
    uint32_t *dark = (uint32_t *)malloc(n * 4), *light = (uint32_t *)malloc(n * 4);
    if (!dark || !light) { free(dark); free(light); return; }
    draw_gradient_as(dark, 0); draw_gradient_as(light, 1);
    for (size_t i = 0; i < n; i++) {
        uint32_t p = g_bg[i], d = dark[i], l = light[i];
        int pr = (p >> 16) & 0xFF, pg = (p >> 8) & 0xFF, pb = p & 0xFF;
        int dr = pr - (int)((d >> 16) & 0xFF), dg = pg - (int)((d >> 8) & 0xFF), db = pb - (int)(d & 0xFF);
        int dev = dr > dg ? dr : dg; if (db > dev) dev = db; dev -= 6;
        if (dev <= 0) { g_bg[i] = l; continue; }
        double a = dev / 100.0; if (a > 1) a = 1;
        double ir = pr * 0.62, ig = pg * 0.62, ib = pb * 0.62;               // deepen the ink for paper
        double lr = (l >> 16) & 0xFF, lg = (l >> 8) & 0xFF, lb = l & 0xFF;
        g_bg[i] = 0xFF000000u | ((uint32_t)(lr + (ir - lr) * a) << 16) | ((uint32_t)(lg + (ig - lg) * a) << 8) | (uint32_t)(lb + (ib - lb) * a);
    }
    free(dark); free(light);
    wp_log(L"composition: lightened for the light theme");
}

// ~\Pictures\ac-wallpaper.bmp stretched to the screen; the gradient if absent.
static int load_composition(void) {
    wchar_t path[MAX_PATH]; DWORD n = GetEnvironmentVariableW(L"USERPROFILE", path, MAX_PATH);
    if (!n) return 0;
    wcscat_s(path, MAX_PATH, L"\\Pictures\\ac-wallpaper.bmp");
    HBITMAP bm = (HBITMAP)LoadImageW(NULL, path, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_CREATEDIBSECTION);
    if (!bm) { wp_log(L"composition: %s not loadable (%lu)", path, GetLastError()); return 0; }
    BITMAP info; GetObject(bm, sizeof info, &info);
    HDC src = CreateCompatibleDC(NULL); HGDIOBJ old = SelectObject(src, bm);
    SetStretchBltMode(g_memDC, HALFTONE); SetBrushOrgEx(g_memDC, 0, 0, NULL);
    StretchBlt(g_memDC, 0, 0, g_w, g_h, src, 0, 0, info.bmWidth, info.bmHeight, SRCCOPY);
    SelectObject(src, old); DeleteDC(src); DeleteObject(bm);
    GdiFlush();
    for (int i = 0; i < g_w * g_h; i++) g_bg[i] = g_fb[i] | 0xFF000000u;
    wp_log(L"composition: %s %dx%d -> %dx%d", path, info.bmWidth, info.bmHeight, g_w, g_h);
    return 1;
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
    read_theme();
    if (!load_composition()) draw_gradient(g_bg);
    else if (g_light) lighten_composition();
    memcpy(g_fb, g_bg, (size_t)g_w * g_h * 4);
    g_markBox.left = g_markBox.right = 0;
}

// ─────────────────────── the one mark ────────────────────────────
// The sprite with a 2-px zero border, so the bilinear taps never need a
// bounds check: index (u + 2, v + 2) for sprite coordinate (u, v), u in -2..W+1.
#define PW (PALS_W + 4)
#define PH (PALS_H + 4)
static uint8_t g_pad[PW * PH];
static void build_padded_sprite(void) {
    memset(g_pad, 0, sizeof g_pad);
    for (int y = 0; y < PALS_H; y++) memcpy(g_pad + (y + 2) * PW + 2, PALS_ALPHA + y * PALS_W, PALS_W);
}

// Draws the mark at a subpixel centre, rotated by `angle`, over g_bg into g_fb.
// Bilinear in 16.16 fixed point. Returns the rectangle touched (the rotated
// rectangle's bounding box plus a 2-px apron).
static RECT draw_mark(double cx, double cy, double width, double angle) {
    double scale = width / PALS_W, hw = width / 2, hh = PALS_H * scale / 2;
    double c = cos(angle), sn = sin(angle);
    double ex = hw * fabs(c) + hh * fabs(sn) + 2, ey = hw * fabs(sn) + hh * fabs(c) + 2;
    int x0 = (int)floor(cx - ex), x1 = (int)ceil(cx + ex), y0 = (int)floor(cy - ey), y1 = (int)ceil(cy + ey);
    if (x0 < 0) x0 = 0; if (y0 < 0) y0 = 0; if (x1 > g_w) x1 = g_w; if (y1 > g_h) y1 = g_h;
    RECT box = { x0, y0, x1, y1 };
    if (x1 <= x0 || y1 <= y0) { box.right = box.left; return box; }
    double inv = 1.0 / scale;
    // Sprite-space steps per destination pixel, 16.16.
    int32_t du = (int32_t)(c * inv * 65536.0), dv = (int32_t)(-sn * inv * 65536.0);
    const int wgt = (int)(MARK_ALPHA * 256 + 0.5);                 // 141: the mark's 55 %
    const RGB8 ink = g_light ? VIOLET_INK : LAVENDER;
    const int tr = ink.r, tg = ink.g, tb = ink.b;
    // Valid sampling window in padded coordinates: iu in [0, PW-2], iv in [0, PH-2].
    const int32_t uMin = 0, uMax = (PW - 2) << 16, vMin = 0, vMax = (PH - 2) << 16;
    for (int y = y0; y < y1; y++) {
        double dy = y + 0.5 - cy, dx0 = x0 + 0.5 - cx;
        // Padded, bilinear-centred coordinates: sprite (u - 0.5) + 2.
        double u0d = (c * dx0 + sn * dy) * inv + PALS_W * 0.5 + 1.5, v0d = (-sn * dx0 + c * dy) * inv + PALS_H * 0.5 + 1.5;
        // Row span where both coordinates stay inside the padded window.
        double lo = 0, hi = (double)(x1 - x0);
        double dud = c * inv, dvd = -sn * inv;
        if (dud > 1e-9) { double a = (0 - u0d) / dud, b = ((PW - 2) - u0d) / dud; if (a > lo) lo = a; if (b < hi) hi = b; }
        else if (dud < -1e-9) { double a = ((PW - 2) - u0d) / dud, b = (0 - u0d) / dud; if (a > lo) lo = a; if (b < hi) hi = b; }
        else if (u0d < 0 || u0d >= PW - 2) continue;
        if (dvd > 1e-9) { double a = (0 - v0d) / dvd, b = ((PH - 2) - v0d) / dvd; if (a > lo) lo = a; if (b < hi) hi = b; }
        else if (dvd < -1e-9) { double a = ((PH - 2) - v0d) / dvd, b = (0 - v0d) / dvd; if (a > lo) lo = a; if (b < hi) hi = b; }
        else if (v0d < 0 || v0d >= PH - 2) continue;
        int i0 = (int)ceil(lo), i1 = (int)floor(hi) - 1; if (i0 < 0) i0 = 0; if (i1 > x1 - x0 - 1) i1 = x1 - x0 - 1;
        if (i1 < i0) continue;
        int32_t u = (int32_t)((u0d + dud * i0) * 65536.0), v = (int32_t)((v0d + dvd * i0) * 65536.0);
        uint32_t *row = g_fb + (size_t)y * g_w + x0;
        for (int i = i0; i <= i1; i++, u += du, v += dv) {
            if (u < uMin || u >= uMax || v < vMin || v >= vMax) continue;      // float→fixed edge guard
            int iu = u >> 16, iv = v >> 16, fu = (u >> 8) & 0xFF, fv = (v >> 8) & 0xFF;
            const uint8_t *p0 = g_pad + iv * PW + iu, *p1 = p0 + PW;
            int a = (p0[0] * (256 - fu) * (256 - fv) + p0[1] * fu * (256 - fv) + p1[0] * (256 - fu) * fv + p1[1] * fu * fv) >> 16;
            if (!a) continue;
            int w = (a * wgt) >> 8, iw = 256 - w;
            uint32_t p = row[i];
            uint32_t r = (((p >> 16) & 0xFF) * iw + tr * w) >> 8;
            uint32_t g = (((p >> 8) & 0xFF) * iw + tg * w) >> 8;
            uint32_t b = ((p & 0xFF) * iw + tb * w) >> 8;
            row[i] = 0xFF000000u | (r << 16) | (g << 8) | b;
        }
    }
    return box;
}

static void restore_box(const RECT *r) {
    if (r->right <= r->left || r->bottom <= r->top) return;
    size_t bytes = (size_t)(r->right - r->left) * 4;
    for (int y = r->top; y < r->bottom; y++) memcpy(g_fb + (size_t)y * g_w + r->left, g_bg + (size_t)y * g_w + r->left, bytes);
}

static void render_frame(void) {
    if (!g_fb || !g_hwnd) return;
    double t = now_seconds(), tr0 = t;
    double width = MARK_WIDTH_PT * g_scale;
    double angle = 2 * M_PI * t / TURN_SECONDS;
    restore_box(&g_markBox);
    RECT prev = g_markBox;
    g_markBox = draw_mark(MARK_CX * g_w, MARK_CY * g_h, width, angle);
    RECT u = g_markBox;
    if (prev.right > prev.left) { if (prev.left < u.left) u.left = prev.left; if (prev.top < u.top) u.top = prev.top; if (prev.right > u.right) u.right = prev.right; if (prev.bottom > u.bottom) u.bottom = prev.bottom; }
    double tb0 = now_seconds(); g_tRender += tb0 - tr0;
    if (!g_winDC) g_winDC = GetDC(g_hwnd);   // CS_OWNDC: held for the window's life
    if (g_winDC && u.right > u.left && u.bottom > u.top) BitBlt(g_winDC, u.left, u.top, u.right - u.left, u.bottom - u.top, g_memDC, u.left, u.top, SRCCOPY);
    g_tBlit += now_seconds() - tb0; g_frames++;
    if (tb0 - g_lastStat > 10) {
        wp_log(L"stats: fps=%.0f frames=%d box=%dx%d render=%.2fms/f blit=%.2fms/f", g_fps, g_frames, u.right - u.left, u.bottom - u.top,
               g_frames ? g_tRender * 1000 / g_frames : 0, g_frames ? g_tBlit * 1000 / g_frames : 0);
        g_tRender = g_tBlit = 0; g_frames = 0; g_lastStat = tb0;
    }
}

// ───────────────────── behind the icons ──────────────────────────
static void log_children(HWND parent, int depth) {
    for (HWND c = GetWindow(parent, GW_CHILD); c; c = GetWindow(c, GW_HWNDNEXT)) {
        wchar_t cls[64]; GetClassNameW(c, cls, 64); RECT r; GetWindowRect(c, &r);
        wp_log(L"%*s%p %s vis=%d rect=%d,%d,%d,%d ex=%08lX", depth * 2, L"", (void *)c, cls, IsWindowVisible(c), r.left, r.top, r.right, r.bottom, (unsigned long)GetWindowLongPtrW(c, GWL_EXSTYLE));
    }
}

static HWND g_workerW = NULL;
static BOOL CALLBACK find_worker(HWND top, LPARAM lp) {
    (void)lp;
    if (FindWindowExW(top, NULL, L"SHELLDLL_DefView", NULL)) { HWND next = FindWindowExW(NULL, top, L"WorkerW", NULL); if (next) g_workerW = next; }
    return TRUE;
}

static BOOL CALLBACK log_monitor(HMONITOR mon, HDC dc, LPRECT r, LPARAM lp) {
    (void)dc; (void)lp;
    MONITORINFOEXW mi; mi.cbSize = sizeof mi; GetMonitorInfoW(mon, (MONITORINFO *)&mi);
    UINT dx = 96, dy = 96; GetDpiForMonitor(mon, MDT_EFFECTIVE_DPI, &dx, &dy);
    wp_log(L"monitor %s rect=%d,%d,%d,%d (%dx%d) work=%d,%d,%d,%d dpi=%u%s", mi.szDevice, r->left, r->top, r->right, r->bottom, r->right - r->left, r->bottom - r->top,
           mi.rcWork.left, mi.rcWork.top, mi.rcWork.right, mi.rcWork.bottom, dx, (mi.dwFlags & MONITORINFOF_PRIMARY) ? L" primary" : L"");
    return TRUE;
}

// Physical pixels: the process is PER_MONITOR_AWARE_V2, so these metrics are
// the real panel (1920x1200 on the MiniBook), not the 150 %-virtualised
// 1280x800, and the virtual-screen rect is the full monitor, not the work area.
static void measure(void) {
    g_vx = GetSystemMetrics(SM_XVIRTUALSCREEN); g_vy = GetSystemMetrics(SM_YVIRTUALSCREEN);
    g_w = GetSystemMetrics(SM_CXVIRTUALSCREEN); g_h = GetSystemMetrics(SM_CYVIRTUALSCREEN);
    if (g_w < 16) g_w = 16; if (g_h < 16) g_h = 16;
    UINT dpi = GetDpiForWindow(g_hwnd ? g_hwnd : GetDesktopWindow()); if (dpi < 72) dpi = 96;
    g_scale = dpi / 96.0;
    DPI_AWARENESS_CONTEXT ctx = GetThreadDpiAwarenessContext();
    wp_log(L"measure: virtual screen %d,%d %dx%d, window dpi %u, per-monitor-v2=%d", g_vx, g_vy, g_w, g_h, dpi, AreDpiAwarenessContextsEqual(ctx, DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2));
    EnumDisplayMonitors(NULL, NULL, log_monitor, 0);
}

static void attach(void) {
    HWND progman = FindWindowW(L"Progman", NULL);
    if (!progman) { wp_log(L"attach: no Progman"); return; }
    if (g_float) { g_parent = NULL; SetWindowPos(g_hwnd, HWND_BOTTOM, g_vx, g_vy, g_w, g_h, SWP_NOACTIVATE | SWP_SHOWWINDOW); wp_log(L"attach: --float"); return; }
    // Explorer reference-counts this request: lParam 1 spawns the wallpaper
    // WorkerW; the legacy (0xD,0)/(0,0) forms RELEASE it and Explorer destroys
    // it a moment later, so only the spawning form is sent.
    DWORD_PTR res = 0;
    SendMessageTimeoutW(progman, 0x052C, 0xD, 0x1, SMTO_NORMAL, 1000, &res);
    Sleep(50);
    HWND defView = FindWindowExW(progman, NULL, L"SHELLDLL_DefView", NULL);
    HWND wwChild = FindWindowExW(progman, NULL, L"WorkerW", NULL);
    SetWindowLongPtrW(g_hwnd, GWL_STYLE, WS_CHILD | WS_VISIBLE);
    if (defView) {
        // Raised desktop (24H2+): Progman has no redirection bitmap, so only a
        // LAYERED child gets composed. Opaque layered child of Progman just
        // beneath the icons, with Explorer's wallpaper WorkerW pushed under us.
        g_parent = progman;
        SetParent(g_hwnd, progman);
        SetWindowLongPtrW(g_hwnd, GWL_EXSTYLE, GetWindowLongPtrW(g_hwnd, GWL_EXSTYLE) | WS_EX_LAYERED);
        SetLayeredWindowAttributes(g_hwnd, 0, 255, LWA_ALPHA);
        RECT pr; GetWindowRect(progman, &pr);
        SetWindowPos(g_hwnd, defView, g_vx - pr.left, g_vy - pr.top, g_w, g_h, SWP_NOACTIVATE | SWP_SHOWWINDOW);
        if (wwChild) SetWindowPos(wwChild, HWND_BOTTOM, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE);
        wp_log(L"attach: raised desktop, layered child of Progman %p beneath DefView %p", (void *)progman, (void *)defView);
    } else {
        // Classic layout: a top-level WorkerW after the one holding DefView.
        g_workerW = NULL; EnumWindows(find_worker, 0);
        HWND host = g_workerW ? g_workerW : progman;
        g_parent = host; SetParent(g_hwnd, host);
        RECT pr; GetWindowRect(host, &pr);
        SetWindowPos(g_hwnd, NULL, g_vx - pr.left, g_vy - pr.top, g_w, g_h, SWP_NOACTIVATE | SWP_SHOWWINDOW | SWP_NOZORDER);
        wp_log(L"attach: classic layout, child of %s %p", g_workerW ? L"WorkerW" : L"Progman", (void *)host);
    }
    log_children(progman, 1);
    { RECT wr; GetWindowRect(g_hwnd, &wr); wp_log(L"window rect on screen: %d,%d,%d,%d (%dx%d)", wr.left, wr.top, wr.right, wr.bottom, wr.right - wr.left, wr.bottom - wr.top); }
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

static int g_frameTimerOn = 0;
static void choose_fps(HWND hwnd) {
    if (g_static) return;
    SYSTEM_POWER_STATUS sps; double want = g_fps;
    if (g_fpsOverride > 0) want = g_fpsOverride;
    else if (GetSystemPowerStatus(&sps)) want = (sps.ACLineStatus == 1) ? 30.0 : 15.0;
    if (want != g_fps || !g_frameTimerOn) { g_fps = want; SetTimer(hwnd, IDT_FRAME, (UINT)(1000.0 / g_fps), NULL); g_frameTimerOn = 1; }
}

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
            g_paused = g_locked || g_displayOff || fullscreen_app_in_front();
            choose_fps(hwnd);
            if (!g_float && (!g_parent || !IsWindow(g_parent))) { measure(); attach(); }   // Explorer restarted
            // As a child of Progman this window never sees the ImmersiveColorSet
            // broadcast (those reach top-level windows only), so the watch polls.
            { int was = g_light; read_theme(); if (was != g_light) { wp_log(L"theme -> %s", g_light ? L"light" : L"dark"); ensure_surface(); InvalidateRect(hwnd, NULL, FALSE); } }
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
        if (wp == PBT_APMPOWERSTATUSCHANGE) choose_fps(hwnd);
        return TRUE;
    case WM_SETTINGCHANGE:
        // Light/dark flips arrive as ImmersiveColorSet: rebuild the ground.
        if (lp && !wcscmp((const wchar_t *)lp, L"ImmersiveColorSet")) {
            int was = g_light; read_theme();
            if (was != g_light) { wp_log(L"theme -> %s", g_light ? L"light" : L"dark"); ensure_surface(); InvalidateRect(hwnd, NULL, FALSE); }
        }
        return 0;
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
    g_static = cmdline && wcsstr(cmdline, L"--static") != NULL;
    g_float = cmdline && wcsstr(cmdline, L"--float") != NULL;
    { const wchar_t *f = cmdline ? wcsstr(cmdline, L"--fps") : NULL; if (f) g_fpsOverride = _wtof(f + 5); }
    SetProcessDpiAwarenessContext(DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2);
    HANDLE single = CreateMutexW(NULL, TRUE, L"computer.aesthetic.slab.wallpaper");
    if (GetLastError() == ERROR_ALREADY_EXISTS) return 0;
    QueryPerformanceFrequency(&g_freq); QueryPerformanceCounter(&g_t0);

    WNDCLASSW wc; memset(&wc, 0, sizeof wc);
    wc.lpfnWndProc = wndproc; wc.hInstance = hInst; wc.lpszClassName = L"SlabWallpaper";
    wc.hCursor = LoadCursor(NULL, IDC_ARROW); wc.style = CS_OWNDC;
    RegisterClassW(&wc);
    g_hwnd = CreateWindowExW(WS_EX_TOOLWINDOW | WS_EX_NOACTIVATE | WS_EX_LAYERED, L"SlabWallpaper", L"slab wallpaper", WS_POPUP,
                             0, 0, 16, 16, NULL, NULL, hInst, NULL);
    SetLayeredWindowAttributes(g_hwnd, 0, 255, LWA_ALPHA);
    build_padded_sprite();
    measure();
    ensure_surface();
    if (!g_static) render_frame();
    attach();
    WTSRegisterSessionNotification(g_hwnd, NOTIFY_FOR_THIS_SESSION);
    RegisterPowerSettingNotification(g_hwnd, &GUID_CONSOLE_DISPLAY_STATE_, DEVICE_NOTIFY_WINDOW_HANDLE);
    choose_fps(g_hwnd);
    SetTimer(g_hwnd, IDT_WATCH, 3000, NULL);
    wp_log(L"start: %s, %dx%d scale %.2f, fps %.0f", g_static ? L"static" : L"one turning mark", g_w, g_h, g_scale, g_fps);

    MSG m;
    while (GetMessageW(&m, NULL, 0, 0)) { TranslateMessage(&m); DispatchMessageW(&m); }
    CloseHandle(single);
    return 0;
}
