#define _GNU_SOURCE
#include "pty.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <pty.h>
#include <termios.h>

extern void ac_log(const char *fmt, ...);

// Standard ANSI 16-color palette
static const uint8_t ansi_colors[16][3] = {
    {  0,   0,   0},   // 0  black
    {170,   0,   0},   // 1  red
    {  0, 170,   0},   // 2  green
    {170,  85,   0},   // 3  yellow/brown
    {  0,   0, 170},   // 4  blue
    {170,   0, 170},   // 5  magenta
    {  0, 170, 170},   // 6  cyan
    {170, 170, 170},   // 7  white (light gray)
    { 85,  85,  85},   // 8  bright black (dark gray)
    {255,  85,  85},   // 9  bright red
    { 85, 255,  85},   // 10 bright green
    {255, 255,  85},   // 11 bright yellow
    { 85,  85, 255},   // 12 bright blue
    {255,  85, 255},   // 13 bright magenta
    { 85, 255, 255},   // 14 bright cyan
    {255, 255, 255},   // 15 bright white
};

void pty_color_to_rgb(int color_index, int bold, uint8_t *r, uint8_t *g, uint8_t *b) {
    if (color_index < 0 || color_index > 15) {
        // Default: white fg or black bg
        *r = *g = *b = (color_index == PTY_COLOR_DEFAULT_FG) ? 170 : 0;
        return;
    }
    // Bold promotes colors 0-7 to 8-15
    int idx = color_index;
    if (bold && idx < 8) idx += 8;
    *r = ansi_colors[idx][0];
    *g = ansi_colors[idx][1];
    *b = ansi_colors[idx][2];
}

void pty_clear(ACPty *pty) {
    for (int y = 0; y < pty->rows; y++) {
        for (int x = 0; x < pty->cols; x++) {
            pty->grid[y][x] = (ACPtyCell){' ', PTY_COLOR_DEFAULT_FG, PTY_COLOR_DEFAULT_BG, 0, 1};
        }
    }
    pty->cursor_x = 0;
    pty->cursor_y = 0;
    pty->grid_dirty = 1;
}

static void scroll_up(ACPty *pty) {
    int top = pty->scroll_top;
    int bot = pty->scroll_bottom;
    // Move rows up by 1 within scroll region
    for (int y = top; y < bot; y++) {
        memcpy(pty->grid[y], pty->grid[y + 1], sizeof(ACPtyCell) * pty->cols);
        for (int x = 0; x < pty->cols; x++) pty->grid[y][x].dirty = 1;
    }
    // Clear bottom row
    for (int x = 0; x < pty->cols; x++) {
        pty->grid[bot][x] = (ACPtyCell){' ', PTY_COLOR_DEFAULT_FG, PTY_COLOR_DEFAULT_BG, 0, 1};
    }
    pty->grid_dirty = 1;
}

static void scroll_down(ACPty *pty) {
    int top = pty->scroll_top;
    int bot = pty->scroll_bottom;
    for (int y = bot; y > top; y--) {
        memcpy(pty->grid[y], pty->grid[y - 1], sizeof(ACPtyCell) * pty->cols);
        for (int x = 0; x < pty->cols; x++) pty->grid[y][x].dirty = 1;
    }
    for (int x = 0; x < pty->cols; x++) {
        pty->grid[top][x] = (ACPtyCell){' ', PTY_COLOR_DEFAULT_FG, PTY_COLOR_DEFAULT_BG, 0, 1};
    }
    pty->grid_dirty = 1;
}

static void put_char(ACPty *pty, uint32_t ch) {
    if (pty->cursor_x >= pty->cols) {
        pty->cursor_x = 0;
        pty->cursor_y++;
        if (pty->cursor_y > pty->scroll_bottom) {
            pty->cursor_y = pty->scroll_bottom;
            scroll_up(pty);
        }
    }
    if (pty->cursor_y < 0) pty->cursor_y = 0;
    if (pty->cursor_y < pty->rows && pty->cursor_x < pty->cols) {
        ACPtyCell *c = &pty->grid[pty->cursor_y][pty->cursor_x];
        c->ch = ch;
        c->fg = pty->cur_fg;
        c->bg = pty->cur_bg;
        c->bold = pty->cur_bold;
        c->fg_r = pty->cur_fg_r; c->fg_g = pty->cur_fg_g; c->fg_b = pty->cur_fg_b;
        c->bg_r = pty->cur_bg_r; c->bg_g = pty->cur_bg_g; c->bg_b = pty->cur_bg_b;
        c->dirty = 1;
    }
    pty->cursor_x++;
    pty->grid_dirty = 1;
}

// Parse CSI parameters: "1;2;3" → array of ints
static int parse_csi_params(const char *buf, int len, int *params, int max_params) {
    int count = 0;
    int val = 0;
    int has_val = 0;
    for (int i = 0; i < len && count < max_params; i++) {
        if (buf[i] >= '0' && buf[i] <= '9') {
            val = val * 10 + (buf[i] - '0');
            has_val = 1;
        } else if (buf[i] == ';') {
            params[count++] = has_val ? val : 0;
            val = 0;
            has_val = 0;
        }
    }
    if (has_val || count == 0) params[count++] = val;
    return count;
}

static void handle_sgr(ACPty *pty, int *params, int count) {
    for (int i = 0; i < count; i++) {
        int p = params[i];
        if (p == 0) {
            pty->cur_fg = PTY_COLOR_DEFAULT_FG;
            pty->cur_bg = PTY_COLOR_DEFAULT_BG;
            pty->cur_bold = 0;
        } else if (p == 1) {
            pty->cur_bold = 1;
        } else if (p == 22) {
            pty->cur_bold = 0;
        } else if (p >= 30 && p <= 37) {
            pty->cur_fg = p - 30;
        } else if (p == 39) {
            pty->cur_fg = PTY_COLOR_DEFAULT_FG;
        } else if (p >= 40 && p <= 47) {
            pty->cur_bg = p - 40;
        } else if (p == 49) {
            pty->cur_bg = PTY_COLOR_DEFAULT_BG;
        } else if (p >= 90 && p <= 97) {
            pty->cur_fg = p - 90 + 8;
        } else if (p >= 100 && p <= 107) {
            pty->cur_bg = p - 100 + 8;
        }
        // 256-color and truecolor support
        else if (p == 38 || p == 48) {
            if (i + 1 < count && params[i + 1] == 5) {
                // 256-color: \e[38;5;Nm
                if (i + 2 < count) {
                    int c = params[i + 2];
                    if (c < 16) {
                        if (p == 38) pty->cur_fg = c;
                        else pty->cur_bg = c;
                    } else if (c < 232) {
                        // 216-color cube (16-231): map to RGB
                        int ci = c - 16;
                        int cr = ci / 36, cg = (ci / 6) % 6, cb = ci % 6;
                        uint8_t r = cr ? 55 + cr * 40 : 0;
                        uint8_t g = cg ? 55 + cg * 40 : 0;
                        uint8_t b = cb ? 55 + cb * 40 : 0;
                        if (p == 38) { pty->cur_fg = 255; pty->cur_fg_r = r; pty->cur_fg_g = g; pty->cur_fg_b = b; }
                        else { pty->cur_bg = 255; pty->cur_bg_r = r; pty->cur_bg_g = g; pty->cur_bg_b = b; }
                    } else {
                        // Grayscale (232-255): 24 shades
                        uint8_t v = 8 + (c - 232) * 10;
                        if (p == 38) { pty->cur_fg = 255; pty->cur_fg_r = pty->cur_fg_g = pty->cur_fg_b = v; }
                        else { pty->cur_bg = 255; pty->cur_bg_r = pty->cur_bg_g = pty->cur_bg_b = v; }
                    }
                    i += 2;
                }
            } else if (i + 1 < count && params[i + 1] == 2) {
                // Truecolor: \e[38;2;R;G;Bm
                if (i + 4 < count) {
                    uint8_t r = params[i + 2], g = params[i + 3], b = params[i + 4];
                    if (p == 38) { pty->cur_fg = 255; pty->cur_fg_r = r; pty->cur_fg_g = g; pty->cur_fg_b = b; }
                    else { pty->cur_bg = 255; pty->cur_bg_r = r; pty->cur_bg_g = g; pty->cur_bg_b = b; }
                }
                i += 4;
            }
        }
    }
}

static void erase_in_line(ACPty *pty, int mode) {
    int y = pty->cursor_y;
    if (y < 0 || y >= pty->rows) return;
    int start = 0, end = pty->cols;
    if (mode == 0) start = pty->cursor_x;       // cursor to end
    else if (mode == 1) end = pty->cursor_x + 1; // start to cursor
    // mode == 2: entire line
    for (int x = start; x < end; x++) {
        pty->grid[y][x] = (ACPtyCell){' ', pty->cur_fg, pty->cur_bg, 0, 1};
    }
    pty->grid_dirty = 1;
}

static void erase_in_display(ACPty *pty, int mode) {
    if (mode == 0) {
        // Cursor to end of screen
        erase_in_line(pty, 0);
        for (int y = pty->cursor_y + 1; y < pty->rows; y++) {
            for (int x = 0; x < pty->cols; x++)
                pty->grid[y][x] = (ACPtyCell){' ', pty->cur_fg, pty->cur_bg, 0, 1};
        }
    } else if (mode == 1) {
        // Start to cursor
        for (int y = 0; y < pty->cursor_y; y++) {
            for (int x = 0; x < pty->cols; x++)
                pty->grid[y][x] = (ACPtyCell){' ', pty->cur_fg, pty->cur_bg, 0, 1};
        }
        erase_in_line(pty, 1);
    } else if (mode == 2 || mode == 3) {
        // Entire screen
        for (int y = 0; y < pty->rows; y++) {
            for (int x = 0; x < pty->cols; x++)
                pty->grid[y][x] = (ACPtyCell){' ', pty->cur_fg, pty->cur_bg, 0, 1};
        }
    }
    pty->grid_dirty = 1;
}

static void handle_csi(ACPty *pty, char final) {
    int params[16] = {0};
    int count = parse_csi_params(pty->csi_buf, pty->csi_len, params, 16);

    switch (final) {
    case 'A': // Cursor Up
        pty->cursor_y -= (params[0] ? params[0] : 1);
        if (pty->cursor_y < pty->scroll_top) pty->cursor_y = pty->scroll_top;
        break;
    case 'B': // Cursor Down
        pty->cursor_y += (params[0] ? params[0] : 1);
        if (pty->cursor_y > pty->scroll_bottom) pty->cursor_y = pty->scroll_bottom;
        break;
    case 'C': // Cursor Forward
        pty->cursor_x += (params[0] ? params[0] : 1);
        if (pty->cursor_x >= pty->cols) pty->cursor_x = pty->cols - 1;
        break;
    case 'D': // Cursor Back
        pty->cursor_x -= (params[0] ? params[0] : 1);
        if (pty->cursor_x < 0) pty->cursor_x = 0;
        break;
    case 'E': // Cursor Next Line
        pty->cursor_x = 0;
        pty->cursor_y += (params[0] ? params[0] : 1);
        if (pty->cursor_y > pty->scroll_bottom) pty->cursor_y = pty->scroll_bottom;
        break;
    case 'F': // Cursor Previous Line
        pty->cursor_x = 0;
        pty->cursor_y -= (params[0] ? params[0] : 1);
        if (pty->cursor_y < pty->scroll_top) pty->cursor_y = pty->scroll_top;
        break;
    case 'G': // Cursor Horizontal Absolute
        pty->cursor_x = (params[0] ? params[0] - 1 : 0);
        if (pty->cursor_x >= pty->cols) pty->cursor_x = pty->cols - 1;
        break;
    case 'H': case 'f': // Cursor Position
        pty->cursor_y = (count > 0 && params[0] ? params[0] - 1 : 0);
        pty->cursor_x = (count > 1 && params[1] ? params[1] - 1 : 0);
        if (pty->cursor_y >= pty->rows) pty->cursor_y = pty->rows - 1;
        if (pty->cursor_x >= pty->cols) pty->cursor_x = pty->cols - 1;
        break;
    case 'J': // Erase in Display
        erase_in_display(pty, params[0]);
        break;
    case 'K': // Erase in Line
        erase_in_line(pty, params[0]);
        break;
    case 'L': { // Insert Lines
        int n = params[0] ? params[0] : 1;
        for (int i = 0; i < n; i++) scroll_down(pty);
        break;
    }
    case 'M': { // Delete Lines
        int n = params[0] ? params[0] : 1;
        for (int i = 0; i < n; i++) scroll_up(pty);
        break;
    }
    case 'P': { // Delete Characters
        int n = params[0] ? params[0] : 1;
        int y = pty->cursor_y;
        if (y >= 0 && y < pty->rows) {
            for (int x = pty->cursor_x; x < pty->cols - n; x++)
                pty->grid[y][x] = pty->grid[y][x + n];
            for (int x = pty->cols - n; x < pty->cols; x++)
                pty->grid[y][x] = (ACPtyCell){' ', pty->cur_fg, pty->cur_bg, 0, 1};
        }
        pty->grid_dirty = 1;
        break;
    }
    case 'S': { // Scroll Up
        int n = params[0] ? params[0] : 1;
        for (int i = 0; i < n; i++) scroll_up(pty);
        break;
    }
    case 'T': { // Scroll Down
        int n = params[0] ? params[0] : 1;
        for (int i = 0; i < n; i++) scroll_down(pty);
        break;
    }
    case 'd': // Cursor Vertical Absolute
        pty->cursor_y = (params[0] ? params[0] - 1 : 0);
        if (pty->cursor_y >= pty->rows) pty->cursor_y = pty->rows - 1;
        break;
    case 'm': // SGR (Select Graphic Rendition)
        handle_sgr(pty, params, count);
        break;
    case 'r': // Set Scroll Region
        pty->scroll_top = (count > 0 && params[0] ? params[0] - 1 : 0);
        pty->scroll_bottom = (count > 1 && params[1] ? params[1] - 1 : pty->rows - 1);
        if (pty->scroll_top < 0) pty->scroll_top = 0;
        if (pty->scroll_bottom >= pty->rows) pty->scroll_bottom = pty->rows - 1;
        pty->cursor_x = 0;
        pty->cursor_y = pty->scroll_top;
        break;
    case 'h': case 'l': // Set/Reset Mode (ignore most, just eat them)
        break;
    case '@': { // Insert Characters
        int n = params[0] ? params[0] : 1;
        int y = pty->cursor_y;
        if (y >= 0 && y < pty->rows) {
            for (int x = pty->cols - 1; x >= pty->cursor_x + n; x--)
                pty->grid[y][x] = pty->grid[y][x - n];
            for (int x = pty->cursor_x; x < pty->cursor_x + n && x < pty->cols; x++)
                pty->grid[y][x] = (ACPtyCell){' ', pty->cur_fg, pty->cur_bg, 0, 1};
        }
        pty->grid_dirty = 1;
        break;
    }
    case 'X': { // Erase Characters
        int n = params[0] ? params[0] : 1;
        int y = pty->cursor_y;
        if (y >= 0 && y < pty->rows) {
            for (int x = pty->cursor_x; x < pty->cursor_x + n && x < pty->cols; x++)
                pty->grid[y][x] = (ACPtyCell){' ', pty->cur_fg, pty->cur_bg, 0, 1};
        }
        pty->grid_dirty = 1;
        break;
    }
    default:
        // Unknown CSI — ignore
        break;
    }
}

// Process a single byte through the VT100 state machine
static void process_byte(ACPty *pty, uint8_t b) {
    switch (pty->state) {
    case 0: // Normal
        if (b == 0x1B) {
            pty->state = 1; // ESC
        } else if (b == '\n') {
            pty->cursor_y++;
            if (pty->cursor_y > pty->scroll_bottom) {
                pty->cursor_y = pty->scroll_bottom;
                scroll_up(pty);
            }
        } else if (b == '\r') {
            pty->cursor_x = 0;
        } else if (b == '\t') {
            pty->cursor_x = (pty->cursor_x + 8) & ~7;
            if (pty->cursor_x >= pty->cols) pty->cursor_x = pty->cols - 1;
        } else if (b == '\b') {
            if (pty->cursor_x > 0) pty->cursor_x--;
        } else if (b == 0x07) {
            // BEL — ignore
        } else if (b >= 0xC0 && b < 0xFE) {
            // UTF-8 multi-byte sequence start
            if (b < 0xE0) { pty->utf8_cp = b & 0x1F; pty->utf8_remaining = 1; }
            else if (b < 0xF0) { pty->utf8_cp = b & 0x0F; pty->utf8_remaining = 2; }
            else { pty->utf8_cp = b & 0x07; pty->utf8_remaining = 3; }
        } else if (b >= 0x80 && b < 0xC0 && pty->utf8_remaining > 0) {
            // UTF-8 continuation byte
            pty->utf8_cp = (pty->utf8_cp << 6) | (b & 0x3F);
            pty->utf8_remaining--;
            if (pty->utf8_remaining == 0) {
                put_char(pty, pty->utf8_cp);
            }
        } else if (b >= 0x20 && b < 0x80) {
            // Printable ASCII
            put_char(pty, b);
        }
        break;

    case 1: // ESC received
        if (b == '[') {
            pty->state = 2; // CSI
            pty->csi_len = 0;
        } else if (b == ']') {
            pty->state = 3; // OSC
            pty->osc_len = 0;
        } else if (b == '7') {
            // Save cursor
            pty->saved_x = pty->cursor_x;
            pty->saved_y = pty->cursor_y;
            pty->saved_fg = pty->cur_fg;
            pty->saved_bg = pty->cur_bg;
            pty->saved_bold = pty->cur_bold;
            pty->state = 0;
        } else if (b == '8') {
            // Restore cursor
            pty->cursor_x = pty->saved_x;
            pty->cursor_y = pty->saved_y;
            pty->cur_fg = pty->saved_fg;
            pty->cur_bg = pty->saved_bg;
            pty->cur_bold = pty->saved_bold;
            pty->state = 0;
        } else if (b == 'M') {
            // Reverse Index (scroll down)
            if (pty->cursor_y == pty->scroll_top) {
                scroll_down(pty);
            } else if (pty->cursor_y > 0) {
                pty->cursor_y--;
            }
            pty->state = 0;
        } else if (b == 'D') {
            // Index (scroll up)
            if (pty->cursor_y == pty->scroll_bottom) {
                scroll_up(pty);
            } else {
                pty->cursor_y++;
            }
            pty->state = 0;
        } else if (b == 'c') {
            // Full Reset
            pty_clear(pty);
            pty->cur_fg = PTY_COLOR_DEFAULT_FG;
            pty->cur_bg = PTY_COLOR_DEFAULT_BG;
            pty->cur_bold = 0;
            pty->scroll_top = 0;
            pty->scroll_bottom = pty->rows - 1;
            pty->state = 0;
        } else {
            pty->state = 0; // Unknown ESC sequence
        }
        break;

    case 2: // CSI
        if (b >= 0x40 && b <= 0x7E) {
            // Final byte
            handle_csi(pty, (char)b);
            pty->state = 0;
        } else if (pty->csi_len < (int)sizeof(pty->csi_buf) - 1) {
            pty->csi_buf[pty->csi_len++] = (char)b;
        }
        break;

    case 3: // OSC
        if (b == 0x07 || b == 0x1B) {
            // BEL or ESC terminates OSC — ignore the content
            pty->state = (b == 0x1B) ? 1 : 0;
        } else if (b == '\\' && pty->osc_len > 0 && pty->osc_buf[pty->osc_len - 1] == 0x1B) {
            // ST (ESC \) terminates OSC
            pty->state = 0;
        } else if (pty->osc_len < (int)sizeof(pty->osc_buf) - 1) {
            pty->osc_buf[pty->osc_len++] = (char)b;
        }
        break;
    }
}

int pty_spawn(ACPty *pty, int cols, int rows, const char *cmd, char *const argv[]) {
    memset(pty, 0, sizeof(*pty));
    pty->cols = (cols > 0 && cols <= PTY_MAX_COLS) ? cols : 80;
    pty->rows = (rows > 0 && rows <= PTY_MAX_ROWS) ? rows : 24;
    pty->cur_fg = PTY_COLOR_DEFAULT_FG;
    pty->cur_bg = PTY_COLOR_DEFAULT_BG;
    pty->scroll_top = 0;
    pty->scroll_bottom = pty->rows - 1;
    pty_clear(pty);

    // Pre-flight checks: log what we're about to spawn
    ac_log("[pty] spawning: cmd='%s' cols=%d rows=%d\n", cmd, pty->cols, pty->rows);
    for (int i = 0; argv[i]; i++) ac_log("[pty]   argv[%d]='%s'\n", i, argv[i]);

    // Check if command exists before forking
    if (access(cmd, X_OK) != 0 && cmd[0] != '/') {
        // Not an absolute path — check PATH
        const char *path = getenv("PATH");
        char check[512];
        int found = 0;
        if (path) {
            char pathcopy[1024];
            snprintf(pathcopy, sizeof(pathcopy), "%s", path);
            char *saveptr;
            for (char *dir = strtok_r(pathcopy, ":", &saveptr); dir; dir = strtok_r(NULL, ":", &saveptr)) {
                snprintf(check, sizeof(check), "%s/%s", dir, cmd);
                if (access(check, X_OK) == 0) {
                    ac_log("[pty] found: %s\n", check);
                    found = 1;
                    break;
                }
            }
        }
        if (!found) {
            ac_log("[pty] WARNING: '%s' not found in PATH=%s\n", cmd, path ? path : "(null)");
        }
    }

    // Check /dev/pts mount
    if (access("/dev/pts", F_OK) != 0) {
        ac_log("[pty] WARNING: /dev/pts does not exist — devpts not mounted?\n");
    }

    struct winsize ws = {
        .ws_row = pty->rows,
        .ws_col = pty->cols,
    };

    pid_t pid = forkpty(&pty->master_fd, NULL, NULL, &ws);
    if (pid < 0) {
        ac_log("[pty] forkpty failed: %s (errno=%d)\n", strerror(errno), errno);
        ac_log("[pty]   /dev/pts exists: %s\n", access("/dev/pts", F_OK) == 0 ? "yes" : "NO");
        ac_log("[pty]   /dev/ptmx exists: %s\n", access("/dev/ptmx", F_OK) == 0 ? "yes" : "NO");
        return -1;
    }

    if (pid == 0) {
        // Child process
        setenv("TERM", "xterm-256color", 1);
        setenv("HOME", "/tmp", 0);
        setenv("LANG", "en_US.UTF-8", 1);
        setenv("PATH", "/bin:/sbin:/usr/bin:/usr/sbin", 0);
        execvp(cmd, argv);
        // exec failed — write error to stderr (flows through PTY to parent)
        int err = errno;
        fprintf(stderr, "\r\n[pty-child] exec '%s' failed: %s (errno=%d)\r\n", cmd, strerror(err), err);
        if (err == ENOENT) {
            fprintf(stderr, "[pty-child] command not found in PATH\r\n");
            // Check common dependencies
            if (access("/bin/sh", X_OK) != 0)
                fprintf(stderr, "[pty-child]   /bin/sh: MISSING\r\n");
            if (access("/bin/bash", X_OK) != 0)
                fprintf(stderr, "[pty-child]   /bin/bash: MISSING\r\n");
            if (access("/bin/node", X_OK) != 0)
                fprintf(stderr, "[pty-child]   /bin/node: MISSING\r\n");
            if (access("/bin/claude", X_OK) != 0)
                fprintf(stderr, "[pty-child]   /bin/claude: MISSING\r\n");
            if (access("/opt/claude-code/cli.js", R_OK) != 0)
                fprintf(stderr, "[pty-child]   /opt/claude-code/cli.js: MISSING\r\n");
        } else if (err == EACCES) {
            fprintf(stderr, "[pty-child] permission denied on '%s'\r\n", cmd);
        }
        _exit(127);
    }

    // Parent
    pty->child_pid = pid;
    pty->alive = 1;

    // Make master non-blocking
    int flags = fcntl(pty->master_fd, F_GETFL, 0);
    fcntl(pty->master_fd, F_SETFL, flags | O_NONBLOCK);

    ac_log("[pty] spawned pid=%d cmd=%s cols=%d rows=%d\n", pid, cmd, cols, rows);
    return 0;
}

int pty_pump(ACPty *pty) {
    if (pty->master_fd < 0) return 0;

    uint8_t buf[4096];
    int total = 0;

    for (;;) {
        ssize_t n = read(pty->master_fd, buf, sizeof(buf));
        if (n <= 0) break;
        for (ssize_t i = 0; i < n; i++) {
            process_byte(pty, buf[i]);
        }
        total += (int)n;
        if (total > 32768) break; // don't block too long per frame
    }

    return total;
}

int pty_write(ACPty *pty, const char *data, int len) {
    if (pty->master_fd < 0 || !data || len <= 0) return -1;
    ssize_t written = write(pty->master_fd, data, len);
    return (int)written;
}

int pty_resize(ACPty *pty, int cols, int rows) {
    if (cols <= 0 || cols > PTY_MAX_COLS) cols = 80;
    if (rows <= 0 || rows > PTY_MAX_ROWS) rows = 24;

    // Resize the terminal
    struct winsize ws = { .ws_row = rows, .ws_col = cols };
    if (pty->master_fd >= 0) {
        ioctl(pty->master_fd, TIOCSWINSZ, &ws);
    }

    // Expand or shrink grid
    int old_rows = pty->rows;
    int old_cols = pty->cols;
    pty->rows = rows;
    pty->cols = cols;
    pty->scroll_bottom = rows - 1;

    // Clear new cells
    for (int y = 0; y < rows; y++) {
        int start = (y < old_rows) ? old_cols : 0;
        for (int x = start; x < cols; x++) {
            pty->grid[y][x] = (ACPtyCell){' ', PTY_COLOR_DEFAULT_FG, PTY_COLOR_DEFAULT_BG, 0, 1};
        }
    }

    if (pty->cursor_x >= cols) pty->cursor_x = cols - 1;
    if (pty->cursor_y >= rows) pty->cursor_y = rows - 1;
    pty->grid_dirty = 1;

    ac_log("[pty] resized %dx%d -> %dx%d\n", old_cols, old_rows, cols, rows);
    return 0;
}

int pty_check_alive(ACPty *pty) {
    if (!pty->alive) return 0;
    int status;
    pid_t result = waitpid(pty->child_pid, &status, WNOHANG);
    if (result == pty->child_pid) {
        pty->alive = 0;
        if (WIFEXITED(status)) {
            int code = WEXITSTATUS(status);
            pty->exit_code = code;
            if (code == 127) {
                ac_log("[pty] child exited: command not found (exit 127)\n");
            } else if (code == 126) {
                ac_log("[pty] child exited: permission denied (exit 126)\n");
            } else {
                ac_log("[pty] child exited (code=%d)\n", code);
            }
        } else if (WIFSIGNALED(status)) {
            int sig = WTERMSIG(status);
            pty->exit_code = 128 + sig;
            ac_log("[pty] child killed by signal %d (%s)\n", sig,
                   sig == SIGSEGV ? "SIGSEGV" :
                   sig == SIGABRT ? "SIGABRT" :
                   sig == SIGTERM ? "SIGTERM" :
                   sig == SIGKILL ? "SIGKILL" : "other");
        } else {
            pty->exit_code = -1;
            ac_log("[pty] child exited (unknown status=%d)\n", status);
        }
    }
    return pty->alive;
}

void pty_destroy(ACPty *pty) {
    if (pty->child_pid > 0 && pty->alive) {
        kill(pty->child_pid, SIGTERM);
        usleep(100000); // 100ms grace
        kill(pty->child_pid, SIGKILL);
        waitpid(pty->child_pid, NULL, 0);
    }
    if (pty->master_fd >= 0) {
        close(pty->master_fd);
        pty->master_fd = -1;
    }
    pty->alive = 0;
    pty->child_pid = 0;
    ac_log("[pty] destroyed\n");
}
