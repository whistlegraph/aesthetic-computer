#ifndef AC_PTY_H
#define AC_PTY_H

#include <stdint.h>
#include <sys/types.h>

// Terminal grid dimensions (matching common terminal sizes)
#define PTY_MAX_COLS 256
#define PTY_MAX_ROWS 128

// ANSI color palette (standard 16 colors)
#define PTY_COLOR_DEFAULT_FG 7   // white
#define PTY_COLOR_DEFAULT_BG 0   // black

typedef struct {
    uint32_t ch;     // Unicode codepoint
    uint8_t  fg;     // foreground color index (0-15, 16=default, 255=truecolor)
    uint8_t  bg;     // background color index (0-15, 16=default, 255=truecolor)
    uint8_t  bold;   // bold attribute
    uint8_t  dirty;  // changed since last read
    uint8_t  fg_r, fg_g, fg_b;  // truecolor foreground (when fg==255)
    uint8_t  bg_r, bg_g, bg_b;  // truecolor background (when bg==255)
} ACPtyCell;

typedef struct {
    // PTY file descriptors
    int master_fd;       // master side (we read/write this)
    pid_t child_pid;     // child process (e.g., claude)

    // Terminal grid
    ACPtyCell grid[PTY_MAX_ROWS][PTY_MAX_COLS];
    int cols, rows;      // current grid size
    int cursor_x, cursor_y;

    // Parser state
    int state;           // 0=normal, 1=ESC, 2=CSI, 3=OSC
    char csi_buf[64];    // CSI parameter accumulator
    int csi_len;
    char osc_buf[256];   // OSC string accumulator
    int osc_len;

    // Current text attributes
    uint8_t cur_fg, cur_bg, cur_bold;
    uint8_t cur_fg_r, cur_fg_g, cur_fg_b;  // truecolor fg (when cur_fg==255)
    uint8_t cur_bg_r, cur_bg_g, cur_bg_b;  // truecolor bg (when cur_bg==255)

    // UTF-8 decoder state
    uint32_t utf8_cp;      // codepoint being assembled
    int utf8_remaining;    // bytes remaining in sequence

    // Scroll region
    int scroll_top, scroll_bottom;

    // Status
    int alive;           // 1 = child process running
    int exit_code;       // exit code when child exits (127=not found, 128+sig=signal)
    int grid_dirty;      // 1 = grid changed since last JS read

    // Saved cursor position (for ESC 7 / ESC 8)
    int saved_x, saved_y;
    uint8_t saved_fg, saved_bg, saved_bold;
} ACPty;

// Create a PTY and spawn a command (e.g., "claude")
// Returns 0 on success, -1 on error
int pty_spawn(ACPty *pty, int cols, int rows, const char *cmd, char *const argv[]);

// Read available output from the PTY and update the grid
// Non-blocking. Returns number of bytes processed.
int pty_pump(ACPty *pty);

// Write input to the PTY (keyboard data from user)
int pty_write(ACPty *pty, const char *data, int len);

// Resize the PTY (TIOCSWINSZ)
int pty_resize(ACPty *pty, int cols, int rows);

// Check if child is still alive (updates pty->alive)
int pty_check_alive(ACPty *pty);

// Kill the child and close the PTY
void pty_destroy(ACPty *pty);

// Reset grid to empty
void pty_clear(ACPty *pty);

// Standard ANSI color to RGB (for rendering)
void pty_color_to_rgb(int color_index, int bold, uint8_t *r, uint8_t *g, uint8_t *b);

#endif
