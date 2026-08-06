#include <SDL3/SDL.h>
#include <arpa/inet.h>
#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <netinet/in.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#include "font-matrix-chunky8.h"
#include "piece-vm-native.h"

#define LOGICAL_W 640
#define LOGICAL_H 360
#define TILE_W (LOGICAL_W / 4)
#define TILE_H (LOGICAL_H / 3)
#define TILE_PERIMETER (2 * TILE_W + 2 * TILE_H - 4)
#define MAX_PROGRAMS 12
#define MAX_VALUES 32
#define RASTER_W 128
#define RASTER_H 128
#define RASTER_PIXELS (RASTER_W * RASTER_H)
#define RASTER_BYTES (RASTER_PIXELS * 3)
#define MIN_RASTER_SIDE 32
#define MAX_RASTER_SIDE 256
#define MAX_RASTER_PIXELS (MAX_RASTER_SIDE * MAX_RASTER_SIDE)
#define MAX_RASTER_BYTES (MAX_RASTER_PIXELS * 3)
#define BYTECODE_STRIDE 24
#define BYTECODE_MAX (8 * BYTECODE_STRIDE)
#define PIECE_VM_BYTECODE_MAX (512 * 8)
#define RESPONSE_CAP (2 * 1024 * 1024)
#define AUDIO_VOICES 5
#define AUDIO_RATE 48000
#define WAVETABLE_SIZE 64
#define VOLUME_DEPTH 8
#define PERMA_MARGIN 16
#define PERMA_SIDE (RASTER_W + PERMA_MARGIN * 2)
#define PERMA_PIXELS (PERMA_SIDE * PERMA_SIDE)
#define PERMA_CELLS (PERMA_PIXELS - RASTER_PIXELS)
#define GROOVE_BYTES (PERMA_CELLS * 3)
#define GROOVE_VERSION 1
#define GROOVE_HEADER_BASE 0
#define GROOVE_SEQUENCE_BASE 64
#define GROOVE_FUNCTION_BASE 128
#define GROOVE_BODY_BASE 192
#define GROOVE_PROJECTION_BASE 480
#define GROOVE_LIFECYCLE_BASE 528
#define GROOVE_STATE_BASE 656
#define GROOVE_SPRITE_BASE 720
#define GROOVE_PROPOSAL_BASE 4820
#define GROOVE_SOURCE_BASE 5108
#define GROOVE_FRINGE_BASE 5300
#define GROOVE_INSTRUCTION_PIXELS 8
#define GROOVE_VECTOR_CELLS 16
#define VM_SEQUENCE_HZ 30
#define VM_READER_HZ (VM_SEQUENCE_HZ * 8)
#define MARGIN_INSTRUCTION_CELLS 3
#define MARGIN_MAX_INSTRUCTIONS (BYTECODE_MAX / BYTECODE_STRIDE)
#define MARGIN_PROGRAM_CELLS (MARGIN_MAX_INSTRUCTIONS * MARGIN_INSTRUCTION_CELLS)
#define MARGIN_FUNCTION_BASE MARGIN_PROGRAM_CELLS
#define MARGIN_FUNCTION_CELLS 18
#define MAX_RASTER_OPCODE 19
#define MARGIN_CAMERA_BASE (MARGIN_FUNCTION_BASE + MARGIN_FUNCTION_CELLS)
#define MARGIN_CAMERA_CELLS 24
#define MARGIN_ALGEBRA_BASE (MARGIN_CAMERA_BASE + MARGIN_CAMERA_CELLS)
#define MARGIN_ALGEBRA_CELLS 14
#define MARGIN_CORE_CELLS (MARGIN_ALGEBRA_BASE + MARGIN_ALGEBRA_CELLS)
#define MARGIN_SPRITE_SIZE 32
#define MARGIN_SPRITE_SLOTS 4
#define MARGIN_SPRITE_BYTES (MARGIN_SPRITE_SIZE * MARGIN_SPRITE_SIZE * 3)
#define MARGIN_SPRITE_CELLS (1 + (MARGIN_SPRITE_BYTES + 7) / 8)
#define MARGIN_SPRITE_BASE MARGIN_CORE_CELLS
#define MARGIN_BODY_CELLS 16
#define MARGIN_RASTER_BODY_BASE (MARGIN_SPRITE_BASE + MARGIN_SPRITE_SLOTS * MARGIN_SPRITE_CELLS)
#define MARGIN_RASTER_BODY_SLOTS MARGIN_FUNCTION_CELLS
#define MARGIN_PROJECTION_AXES 3
#define MARGIN_PROJECTION_BODY_BASE (MARGIN_RASTER_BODY_BASE + MARGIN_RASTER_BODY_SLOTS * MARGIN_BODY_CELLS)
#define MARGIN_PROPOSAL_BASE (MARGIN_PROJECTION_BODY_BASE + MARGIN_PROJECTION_AXES * MARGIN_BODY_CELLS)
#define MARGIN_FRINGE_BASE MARGIN_PROPOSAL_BASE /* proposals are written into the mutable fringe */
#define MARGIN_WARP_GRID 17

_Static_assert(MARGIN_CORE_CELLS == 80, "margin core layout must stay stable");
_Static_assert(MARGIN_FRINGE_BASE < PERMA_CELLS, "margin sprite sheets must leave a fringe");

typedef struct { uint8_t r, g, b, a; } Color;

typedef struct {
    char id[24];
    char origin[24];
    char status[24];
    char parent[24];
    char source[160];
    char domain[16];
    char address[4];
    int generation;
    int operations;
    bool retained;
    float novelty;
    float quality;
    int values[MAX_VALUES];
    int value_count;
    uint8_t raster[RASTER_BYTES];
    int raster_count;
    int raster_width;
    int raster_height;
    uint8_t bytecode[BYTECODE_MAX];
    int bytecode_count;
    uint8_t groove[GROOVE_BYTES];
    int groove_count;
    uint8_t piece_vm[PIECE_VM_BYTECODE_MAX];
    int piece_vm_count;
    int piece_vm_resolution;
    bool piece_vm_probe_carrier;
    char piece_vm_source_id[16];
    char piece_vm_role[16];
} Program;

typedef struct {
    uint64_t iteration;
    uint64_t accepted;
    uint64_t rejected;
    uint64_t checkpoint_next;
    uint64_t checkpoint_remaining;
    uint64_t checkpoint_ms;
    int coverage;
    int capacity;
    int raster_coverage;
    int raster_capacity;
    int resident_bytes;
    int active_reads;
    int active_writes;
    float evaluations_per_second;
    uint64_t utc_ms;
    float musical_bpm;
    bool clock_synced;
    int visual_reviews;
    int visual_retain;
    int visual_watch;
    int visual_reject;
    int git_editions;
    char git_head[9];
    uint64_t git_iteration;
    int piece_vm_generation;
    int piece_vm_accepted;
    int piece_vm_rejected;
    int piece_vm_lineage;
    int piece_vm_crossovers;
    int piece_vm_functions;
    int piece_vm_arguments;
    int piece_vm_layouts;
    int piece_vm_layout_bytes;
    int piece_vm_registers;
    int piece_vm_calls;
    int piece_vm_memory;
    int piece_vm_senses;
    float piece_vm_score;
    bool piece_vm_half_verified;
    bool piece_vm_standard_verified;
    bool piece_vm_double_verified;
    char piece_vm_mutation[24];
    char piece_vm_id[16];
    char piece_vm_environment_capability[16];
    char piece_vm_environment_donor[16];
    char margin_probe_id[24];
    int margin_probe_address;
    char margin_probe_track[16];
    char margin_probe_capability[16];
    char margin_probe_requested_by[24];
    char margin_probe_status[16];
    char margin_probe_descendant_id[16];
    int margin_probe_attempts;
    char margin_probe_descendant_state[16];
    int margin_probe_descendants;
    int margin_probe_generation;
    int margin_probe_children;
    int margin_probe_propagation_descendants;
    int margin_probe_propagation_residents;
    int margin_probe_propagation_generation;
    char margin_probe_propagation_frontier[16];
    bool margin_probe_champion_carrier;
    char piece_vm_selection_parent[16];
    int piece_vm_phenotype_reports;
    bool piece_vm_phenotype_ready;
    float piece_vm_phenotype_score;
    float piece_vm_phenotype_bias;
    int piece_vm_phenotype_voices;
    char piece_vm_phenotype_role[16];
    char piece_vm_policy_bonus[24];
    int piece_vm_policy_trials[3];
    float piece_vm_policy_reward[3];
    float piece_vm_policy_admission[3];
    float piece_vm_policy_capability[3];
    char piece_vm_operator_bonus[16];
    int piece_vm_operator_trials[3];
    float piece_vm_operator_reward[3];
    float piece_vm_operator_capability[3];
    char piece_vm_mutation_bonus[32];
    bool piece_vm_curriculum_lead;
    int piece_vm_curriculum_trials;
    int piece_vm_curriculum_advancements;
    int piece_vm_curriculum_compound;
    int piece_vm_curriculum_max_breadth;
    int piece_vm_development_breadth;
    char piece_vm_development_signature[8];
    char selected[24];
    Program programs[MAX_PROGRAMS];
    int program_count;
    uint64_t received_at;
    bool connected;
} FarmState;

typedef struct {
    uint32_t *pixels;
} Canvas;

typedef struct {
    SDL_Window *window;
    SDL_Renderer *renderer;
    SDL_Texture *texture;
    uint32_t *pixels;
    Canvas canvas;
    SDL_Rect bounds;
    SDL_DisplayID display;
} Panel;

typedef struct {
    Panel board;
    Panel soup;
} Stage;

typedef struct {
    float frequency;
    float pan;
    float gain;
    float brightness;
    float sub;
    float overtone;
    float percussion;
    uint8_t family;
    float wavetable[WAVETABLE_SIZE];
    uint64_t generation;
} SonicVoice;

typedef struct {
    pthread_mutex_t lock;
    SonicVoice target[AUDIO_VOICES];
    SonicVoice current[AUDIO_VOICES];
    float wave_mix[AUDIO_VOICES];
    float transient[AUDIO_VOICES];
    uint8_t transient_stage[AUDIO_VOICES];
    double phase[AUDIO_VOICES];
    double sub_phase[AUDIO_VOICES];
    uint64_t sample_clock;
    float buffer[4096 * 2];
    float spectrum[32];
    int probe_voices;
    char probe_source_id[16];
    char probe_address[4];
} SonicField;

/* A non-rasterized, typed 16-byte Lisp cell in the uniform 16px permamargin. */
typedef struct {
    uint8_t tag;
    uint8_t flags;
    uint16_t arity;
    uint32_t link;
    uint64_t payload;
} MetaCell;

typedef struct {
    char id[24];
    int resolution;
    int pixel_count;
    int byte_count;
    uint8_t seed[MAX_RASTER_BYTES];
    uint8_t pixels[MAX_RASTER_BYTES];
    uint8_t scratch[MAX_RASTER_BYTES];
    uint8_t display[2][MAX_RASTER_BYTES];
    uint8_t volume[VOLUME_DEPTH][MAX_RASTER_BYTES];
    uint8_t projection[2][MAX_RASTER_BYTES];
    uint32_t queue[MAX_RASTER_PIXELS];
    uint8_t visited[MAX_RASTER_PIXELS];
    uint8_t sanctuary[MAX_RASTER_PIXELS];
    MetaCell permastore[PERMA_CELLS];
    uint8_t groove[GROOVE_BYTES];
    int bytecode_count;
    uint64_t born_at;
    int frames;
    uint32_t previous_hash;
    uint32_t two_back_hash;
    int still_steps;
    int flicker_steps;
    int dead_steps;
    int failed_reprobes;
    int recovery_steps;
    int probe_epoch;
    int life_state;
    float actual_energy;
    float potential_energy;
    float spatial_energy;
    float variance_energy;
    float noise_energy;
    float temporal_coherence;
    float muddiness;
    float health;
    float ema_actual;
    float ema_noise;
    float ema_coherence;
    float ema_muddiness;
    uint64_t lifetime_steps;
    uint64_t healthy_steps;
    float rgb[3];
    int display_index;
    int published_life_state;
    float published_actual_energy;
    float published_potential_energy;
    float published_spatial_energy;
    float published_variance_energy;
    float published_noise_energy;
    float published_temporal_coherence;
    float published_muddiness;
    float published_health;
    int volume_head;
    int projection_index;
    int projection_mode;
    int projection_candidate;
    int projection_stability;
    float projection_mix;
    int published_projection_mode;
    int projection_generation;
    int published_projection_generation;
    int current_opcode;
    int published_opcode;
    uint64_t next_synthesis_step;
    float curvature;
    float direction_x;
    float direction_y;
    char console_lines[4][48];
    char published_console[4][48];
    int console_head;
    int published_console_head;
    PieceVmNative *piece_vm;
    uint32_t piece_vm_hash;
    char piece_vm_source_id[16];
    char piece_vm_role[16];
    bool piece_vm_probe_carrier;
    int sonic_voices;
    bool occupied;
} LiveField;

typedef struct {
    char id[24];
    char strategy[16];
    char donor[24];
    bool occupied;
} ProdAction;

static pthread_mutex_t state_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t live_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t action_lock = PTHREAD_MUTEX_INITIALIZER;
static FarmState shared_state;
static volatile bool running = true;
static int server_port = 8788;
static double display_fps = 0;
static uint64_t vm_deadline_misses = 0;
static const char *snapshot_dir = NULL;
static SonicField sonic_field = {.lock = PTHREAD_MUTEX_INITIALIZER};
static bool audio_online = false;
static LiveField live_fields[MAX_PROGRAMS];
static uint64_t live_step_at = 0;
static uint64_t live_authority_utc_ms = 0;
static float live_musical_bpm = 60;
static int live_channels = 0;
static uint64_t live_writes_per_second = 0;
static int alive_channels = 0, dormant_channels = 0, collapsed_channels = 0, flicker_channels = 0;
static uint64_t health_report_successes = 0, health_report_failures = 0;
static Program stable_programs[MAX_PROGRAMS];
static LiveField *find_live_field(const Program *program);
static int stable_missing[MAX_PROGRAMS];
static bool stable_occupied[MAX_PROGRAMS];
static ProdAction pending_actions[MAX_PROGRAMS];

static const Color INK = {238, 242, 221, 255};
static const Color MINT = {151, 232, 197, 255};
static const Color GOLD = {255, 216, 95, 255};
static const Color PINK = {238, 92, 203, 255};
static const Color CYAN = {91, 210, 224, 255};
static const Color RED = {239, 68, 68, 255};
static const Color ORANGE = {255, 122, 26, 255};
static const Color YELLOW = {250, 204, 21, 255};
static const Color GREEN = {74, 222, 128, 255};
static const Color DIM = {77, 108, 102, 255};
static const Color BLACK = {4, 8, 13, 255};
static const Color HEALTH_BLACK = {0, 0, 0, 255};
static Color color_mix(Color a, Color b, float amount) {
    amount = fminf(1, fmaxf(0, amount));
    return (Color){
        (uint8_t)lroundf(a.r + (b.r - a.r) * amount),
        (uint8_t)lroundf(a.g + (b.g - a.g) * amount),
        (uint8_t)lroundf(a.b + (b.b - a.b) * amount), 255,
    };
}

static Color health_scale_color(float amount) {
    amount = fminf(1, fmaxf(0, amount));
    if (amount < .34f) return color_mix(RED, ORANGE, amount / .34f);
    if (amount < .67f) return color_mix(ORANGE, YELLOW, (amount - .34f) / .33f);
    return color_mix(YELLOW, GREEN, (amount - .67f) / .33f);
}

static LiveField *live_field(const Program *program);
static LiveField *find_live_field(const Program *program);
static void advance_margin_fringe(LiveField *field);
static const char *opcode_name(int opcode);
static void live_console(LiveField *field, const char *message);
static int request(const char *method, const char *path, const char *payload,
                   char *response, size_t capacity);

static uint32_t hash_text(const char *text) {
    uint32_t h = 5381;
    for (const unsigned char *p = (const unsigned char *)text; p && *p; p++)
        h = ((h << 5) + h) ^ *p;
    return h;
}

static uint32_t packed(Color c) {
    return ((uint32_t)c.r << 24) | ((uint32_t)c.g << 16) |
           ((uint32_t)c.b << 8) | c.a;
}

static uint32_t blend(uint32_t below, Color over) {
    if (over.a == 255) return packed(over);
    uint32_t a = over.a, inverse = 255 - a;
    uint32_t br = below >> 24, bg = below >> 16 & 255, bb = below >> 8 & 255;
    return (((over.r * a + br * inverse) / 255) << 24) |
           (((over.g * a + bg * inverse) / 255) << 16) |
           (((over.b * a + bb * inverse) / 255) << 8) | 255;
}

static void fill(Canvas *canvas, float fx, float fy, float fw, float fh, Color c) {
    int x0 = (int)floorf(fx), y0 = (int)floorf(fy);
    int x1 = (int)ceilf(fx + fw), y1 = (int)ceilf(fy + fh);
    if (x0 < 0) x0 = 0;
    if (y0 < 0) y0 = 0;
    if (x1 > LOGICAL_W) x1 = LOGICAL_W;
    if (y1 > LOGICAL_H) y1 = LOGICAL_H;
    uint32_t value = packed(c);
    for (int y = y0; y < y1; y++) {
        uint32_t *row = canvas->pixels + y * LOGICAL_W;
        if (c.a == 255) for (int x = x0; x < x1; x++) row[x] = value;
        else for (int x = x0; x < x1; x++) row[x] = blend(row[x], c);
    }
}

static void stroke(Canvas *canvas, float x, float y, float w, float h, Color c) {
    fill(canvas, x, y, w, 1, c); fill(canvas, x, y + h - 1, w, 1, c);
    fill(canvas, x, y, 1, h, c); fill(canvas, x + w - 1, y, 1, h, c);
}

static int text_width(const char *text, int scale) {
    int width = 0;
    for (const unsigned char *p = (const unsigned char *)text; p && *p && *p != '\n'; p++) {
        unsigned char c = (*p < 32 || *p > 126) ? '?' : *p;
        width += matrix_chunky8_glyphs[c - 32].dwidth * scale;
    }
    return width;
}

static int draw_text(Canvas *canvas, const char *text, int x, int y, int scale, Color color) {
    if (!text) return x;
    int start_x = x;
    for (const unsigned char *p = (const unsigned char *)text; *p; p++) {
        unsigned char c = *p;
        if (c == '\n') { x = start_x; y += 9 * scale; continue; }
        if (c < 32 || c > 126) c = '?';
        const BDFGlyph *g = &matrix_chunky8_glyphs[c - 32];
        int gx = x + g->xoff * scale;
        int gy = y + (matrix_chunky8_ascent - g->yoff - g->height) * scale;
        for (int row = 0; row < g->height; row++) {
            uint8_t bits = g->rows[row];
            for (int col = 0; col < g->width; col++) {
                if (bits & (0x80 >> col))
                    fill(canvas, gx + col * scale, gy + row * scale, scale, scale, color);
            }
        }
        x += g->dwidth * scale;
    }
    return x;
}

static int draw_text_shadow(Canvas *canvas, const char *text, int x, int y, int scale, Color color) {
    draw_text(canvas, text, x + scale, y + scale, scale, (Color){0, 0, 0, 190});
    return draw_text(canvas, text, x, y, scale, color);
}

static int draw_wrapped(Canvas *canvas, const char *text, int x, int y,
                        int max_width, int scale, int max_lines, Color color) {
    char line[512] = "";
    char word[128] = "";
    const char *cursor = text;
    int lines = 0;
    while (*cursor && lines < max_lines) {
        while (*cursor == ' ') cursor++;
        int n = 0;
        while (*cursor && *cursor != ' ' && n < (int)sizeof(word) - 1)
            word[n++] = *cursor++;
        word[n] = '\0';
        if (!n) break;
        char trial[768];
        snprintf(trial, sizeof(trial), "%s%s%s", line, line[0] ? " " : "", word);
        if (line[0] && text_width(trial, scale) > max_width) {
            draw_text(canvas, line, x, y + lines * 9 * scale, scale, color);
            lines++;
            snprintf(line, sizeof(line), "%s", word);
        } else {
            size_t trial_length = strlen(trial);
            if (trial_length >= sizeof(line)) trial_length = sizeof(line) - 1;
            memcpy(line, trial, trial_length);
            line[trial_length] = '\0';
        }
    }
    if (line[0] && lines < max_lines) {
        draw_text(canvas, line, x, y + lines * 9 * scale, scale, color);
        lines++;
    }
    return lines;
}

static void resample_rgb(uint8_t *target, int target_side, const uint8_t *source, int source_side);

static void memory_wavetable(float table[WAVETABLE_SIZE], const uint8_t *pixels,
                             int family, float brightness) {
    const double tau = 6.283185307179586;
    float raw[WAVETABLE_SIZE], mean = 0;
    for (int i = 0; i < WAVETABLE_SIZE; i++) {
        int x = i * RASTER_W / WAVETABLE_SIZE;
        int y = (i * 37 + pixels[(i * 769) % RASTER_BYTES]) & (RASTER_H - 1);
        int at = (y * RASTER_W + x) * 3;
        raw[i] = (pixels[at] * .50f + pixels[at + 1] * .31f + pixels[at + 2] * .19f) / 255.0f;
        mean += raw[i];
    }
    mean /= WAVETABLE_SIZE;
    float peak = 0;
    memset(table, 0, sizeof(float) * WAVETABLE_SIZE);
    for (int harmonic = 1; harmonic <= 12; harmonic++) {
        double cosine = 0, sine = 0;
        for (int i = 0; i < WAVETABLE_SIZE; i++) {
            double phase = tau * harmonic * i / WAVETABLE_SIZE;
            cosine += (raw[i] - mean) * cos(phase);
            sine += (raw[i] - mean) * sin(phase);
        }
        cosine *= 2.0 / WAVETABLE_SIZE; sine *= 2.0 / WAVETABLE_SIZE;
        float rolloff;
        if (family == 0) rolloff = 1.0f / powf((float)harmonic, 1.75f); /* warm */
        else if (family == 1) rolloff = (harmonic & 1 ? 1.0f : .16f) / powf((float)harmonic, 1.25f); /* hollow */
        else if (family == 2) rolloff = (.45f + brightness * .55f) / powf((float)harmonic, 1.02f); /* glass */
        else rolloff = (harmonic == 1 ? 1.0f : harmonic == 2 ? .48f : harmonic == 3 ? .62f : .30f) /
            powf((float)harmonic, 1.18f); /* organ */
        for (int i = 0; i < WAVETABLE_SIZE; i++) {
            double phase = tau * harmonic * i / WAVETABLE_SIZE;
            table[i] += (float)((cosine * cos(phase) + sine * sin(phase)) * rolloff);
        }
    }
    for (int i = 0; i < WAVETABLE_SIZE; i++) if (fabsf(table[i]) > peak) peak = fabsf(table[i]);
    for (int i = 0; i < WAVETABLE_SIZE; i++) {
        float phase = (float)(tau * i / WAVETABLE_SIZE);
        float basis = family == 0 ? sinf(phase) :
            family == 1 ? sinf(phase) * .82f + sinf(phase * 3) * .18f :
            family == 2 ? sinf(phase) * .68f + sinf(phase * 2) * .20f + sinf(phase * 5) * .12f :
            sinf(phase) * .64f + sinf(phase * 2) * .18f + sinf(phase * 3) * .18f;
        table[i] = peak < .0001f ? basis : table[i] / peak * .78f + basis * .22f;
    }
    /* Two circular low-pass passes retain the resident's timbre without wirey corners. */
    for (int pass = 0; pass < 2; pass++) {
        float smooth[WAVETABLE_SIZE];
        for (int i = 0; i < WAVETABLE_SIZE; i++)
            smooth[i] = table[(i - 1) & (WAVETABLE_SIZE - 1)] * .22f + table[i] * .56f + table[(i + 1) & (WAVETABLE_SIZE - 1)] * .22f;
        memcpy(table, smooth, sizeof(smooth));
    }
}

static SonicVoice sonic_voice(const Program *program, int slot, bool selected, uint64_t musical_tick) {
    static const int bedtime_dorian[] = {0, 2, 3, 5, 7, 9, 10};
    SonicVoice voice = {0};
    float spatial = 0, variance = 0, actual = 0, coherence = 0, color_axis = 0;
    int life_state = 0;
    uint8_t memory[RASTER_BYTES]; memset(memory, 0, sizeof(memory));
    pthread_mutex_lock(&live_lock);
    LiveField *field = find_live_field(program);
    if (field) {
        spatial = field->published_spatial_energy;
        variance = field->published_variance_energy;
        actual = field->published_actual_energy;
        coherence = field->temporal_coherence;
        color_axis = field->rgb[2] - field->rgb[0];
        life_state = field->published_life_state;
        resample_rgb(memory, RASTER_W, field->display[field->display_index], field->resolution);
    }
    pthread_mutex_unlock(&live_lock);
    uint32_t timbre_seed = hash_text(program->id) ^ ((uint32_t)memory[17] << 16) ^
        ((uint32_t)memory[211] << 8) ^ memory[977];
    voice.family = (uint8_t)(timbre_seed & 3u);
    int degree = (int)lroundf(fminf(34, fmaxf(0,
        spatial * 32 + variance * 24 + (color_axis + 1) * 4 + slot * 4)));
    int note = bedtime_dorian[degree % 7] + 12 * (degree / 7);
    /* Deep-night register: keep the same Dorian relationships and memory
       mapping, but transpose the complete farm down another octave. */
    voice.frequency = 6.875f * powf(2.0f, note / 12.0f);
    if (selected) voice.frequency = fmaxf(6.875f, voice.frequency * .5f);
    int column = slot % 4, row = slot / 4;
    voice.pan = (column / 1.5f - 1.0f) * .82f + color_axis * .18f;
    float distance = 1.0f + row * .16f;
    voice.gain = (.002f + actual * .016f + spatial * .004f) / distance;
    if (life_state == 1) voice.gain *= .22f;
    if (life_state == 2) voice.gain *= .10f;
    if (life_state == 3) voice.gain *= .06f;
    if (selected) voice.gain *= 1.35f;
    /* Eighth-note authority with sparse quarter-note consonant entrances. */
    bool sounded = selected || (slot == 1 && musical_tick % 2 == 0) ||
        (slot == 2 && musical_tick % 4 == 1) || (slot == 3 && musical_tick % 8 == 3) ||
        (slot >= 4 && (musical_tick % 8 == 0 || musical_tick % 8 == 5));
    if (!sounded) voice.gain *= .22f;
    if (!selected && slot >= 4 && sounded) voice.percussion = .34f + fminf(.20f, spatial);
    voice.pan = fmaxf(-1, fminf(1, voice.pan));
    voice.gain = fminf(.014f, voice.gain);
    voice.brightness = fminf(1, variance * .45f + spatial * .55f);
    voice.sub = .08f + fminf(.30f, coherence * .18f + row * .035f);
    voice.overtone = .04f + voice.brightness * .22f + (voice.family == 2 ? .08f : 0);
    voice.generation = musical_tick;
    memory_wavetable(voice.wavetable, memory, voice.family, voice.brightness);
    return voice;
}

static void update_sonic_field(const FarmState *state, uint64_t now) {
    static uint64_t last_musical_tick = UINT64_MAX;
    uint64_t authority_ms = state->utc_ms + (now > state->received_at ? now - state->received_at : 0);
    double bpm = state->musical_bpm > 0 ? state->musical_bpm : 60;
    uint64_t subdivision_ms = (uint64_t)fmax(1, 60000.0 / bpm / 2.0);
    uint64_t musical_tick = authority_ms / subdivision_ms;
    if (musical_tick == last_musical_tick) return;
    last_musical_tick = musical_tick;
    SonicVoice next[AUDIO_VOICES] = {0};
    const Program *voice_programs[AUDIO_VOICES] = {0};
    int probe_voices = 0;
    char probe_source_id[16] = {0}, probe_address[4] = {0};
    if (state->program_count) {
        int selected_slot = 0;
        for (int i = 0; i < state->program_count; i++)
            if (!strcmp(state->programs[i].id, state->selected)) selected_slot = i;
        voice_programs[0] = &state->programs[selected_slot];
        next[0] = sonic_voice(&state->programs[selected_slot], selected_slot, true, musical_tick);
        if (state->programs[selected_slot].piece_vm_probe_carrier) {
            probe_voices++;
            snprintf(probe_source_id, sizeof(probe_source_id), "%s", state->programs[selected_slot].piece_vm_source_id);
            snprintf(probe_address, sizeof(probe_address), "%s", state->programs[selected_slot].address);
        }
        for (int voice = 1; voice < AUDIO_VOICES; voice++) {
            int slot = voice * (state->program_count - 1) / (AUDIO_VOICES - 1);
            voice_programs[voice] = &state->programs[slot];
            next[voice] = sonic_voice(&state->programs[slot], slot, false, musical_tick);
            if (state->programs[slot].piece_vm_probe_carrier) {
                probe_voices++;
                if (!probe_source_id[0]) {
                    snprintf(probe_source_id, sizeof(probe_source_id), "%s", state->programs[slot].piece_vm_source_id);
                    snprintf(probe_address, sizeof(probe_address), "%s", state->programs[slot].address);
                }
            }
        }
    }
    pthread_mutex_lock(&live_lock);
    for (int slot = 0; slot < MAX_PROGRAMS; slot++) if (live_fields[slot].occupied) live_fields[slot].sonic_voices = 0;
    for (int voice = 0; voice < AUDIO_VOICES; voice++) {
        LiveField *field = find_live_field(voice_programs[voice]);
        if (field) field->sonic_voices++;
    }
    pthread_mutex_unlock(&live_lock);
    pthread_mutex_lock(&sonic_field.lock);
    sonic_field.probe_voices = probe_voices;
    snprintf(sonic_field.probe_source_id, sizeof(sonic_field.probe_source_id), "%s", probe_source_id);
    snprintf(sonic_field.probe_address, sizeof(sonic_field.probe_address), "%s", probe_address);
    memcpy(sonic_field.target, next, sizeof(next));
    float measured[32] = {0};
    const double tau = 6.283185307179586;
    for (int voice = 0; voice < AUDIO_VOICES; voice++) for (int harmonic = 1; harmonic <= 8; harmonic++) {
        double cosine = 0, sine = 0;
        for (int sample = 0; sample < WAVETABLE_SIZE; sample++) {
            double phase = tau * harmonic * sample / WAVETABLE_SIZE;
            cosine += next[voice].wavetable[sample] * cos(phase);
            sine += next[voice].wavetable[sample] * sin(phase);
        }
        float frequency = next[voice].frequency * harmonic;
        int band = (int)floorf(log2f(fmaxf(55, frequency) / 55.0f) * 6.0f);
        if (band < 0) band = 0;
        if (band > 31) band = 31;
        measured[band] += sqrtf((float)(cosine * cosine + sine * sine)) /
                          WAVETABLE_SIZE * next[voice].gain * 90;
    }
    for (int band = 0; band < 32; band++)
        sonic_field.spectrum[band] = sonic_field.spectrum[band] * .72f + fminf(1, measured[band]) * .28f;
    pthread_mutex_unlock(&sonic_field.lock);
}

static void SDLCALL synth_audio(void *userdata, SDL_AudioStream *stream,
                                int additional_amount, int total_amount) {
    (void)userdata; (void)total_amount;
    SonicVoice target[AUDIO_VOICES];
    pthread_mutex_lock(&sonic_field.lock);
    memcpy(target, sonic_field.target, sizeof(target));
    pthread_mutex_unlock(&sonic_field.lock);
    int frames_left = additional_amount / (int)(sizeof(float) * 2);
    while (frames_left > 0) {
        int frames = frames_left > 4096 ? 4096 : frames_left;
        for (int frame = 0; frame < frames; frame++) {
            float left = 0, right = 0;
            for (int voice = 0; voice < AUDIO_VOICES; voice++) {
                SonicVoice *current = &sonic_field.current[voice];
                current->frequency += (target[voice].frequency - current->frequency) * .00012f;
                current->pan += (target[voice].pan - current->pan) * .00012f;
                current->gain += (target[voice].gain - current->gain) * .00008f;
                current->brightness += (target[voice].brightness - current->brightness) * .00005f;
                current->sub += (target[voice].sub - current->sub) * .00005f;
                current->overtone += (target[voice].overtone - current->overtone) * .00005f;
                sonic_field.phase[voice] += current->frequency / AUDIO_RATE;
                if (sonic_field.phase[voice] >= 1.0) sonic_field.phase[voice] -= 1.0;
                sonic_field.sub_phase[voice] += current->frequency * .5f / AUDIO_RATE;
                if (sonic_field.sub_phase[voice] >= 1.0) sonic_field.sub_phase[voice] -= 1.0;
                if (current->generation != target[voice].generation)
                    sonic_field.wave_mix[voice] = fminf(1, sonic_field.wave_mix[voice] + .00022f);
                if (current->generation != target[voice].generation && target[voice].percussion > 0 && sonic_field.transient_stage[voice] == 0) {
                    sonic_field.transient[voice] = 0;
                    sonic_field.transient_stage[voice] = 1;
                }
                float position = (float)sonic_field.phase[voice] * WAVETABLE_SIZE;
                int index = (int)position & (WAVETABLE_SIZE - 1), next_index = (index + 1) & (WAVETABLE_SIZE - 1);
                float fraction = position - floorf(position);
                float old_wave = current->wavetable[index] * (1 - fraction) + current->wavetable[next_index] * fraction;
                float new_wave = target[voice].wavetable[index] * (1 - fraction) + target[voice].wavetable[next_index] * fraction;
                float mix = sonic_field.wave_mix[voice];
                float tone = old_wave * (1 - mix) + new_wave * mix;
                float upper_position = fmodf((float)sonic_field.phase[voice] * 2.0f, 1.0f) * WAVETABLE_SIZE;
                int upper_index = (int)upper_position & (WAVETABLE_SIZE - 1);
                int upper_next = (upper_index + 1) & (WAVETABLE_SIZE - 1);
                float upper_fraction = upper_position - floorf(upper_position);
                float old_upper = current->wavetable[upper_index] * (1 - upper_fraction) +
                    current->wavetable[upper_next] * upper_fraction;
                float new_upper = target[voice].wavetable[upper_index] * (1 - upper_fraction) +
                    target[voice].wavetable[upper_next] * upper_fraction;
                float upper = old_upper * (1 - mix) + new_upper * mix;
                if (mix >= 1) {
                    memcpy(current->wavetable, target[voice].wavetable, sizeof(current->wavetable));
                    current->generation = target[voice].generation;
                    sonic_field.wave_mix[voice] = 0;
                }
                float sub_tone = sinf((float)(sonic_field.sub_phase[voice] * 6.283185307179586));
                float sample = (tone * .76f + sub_tone * current->sub + upper * current->overtone) * current->gain;
                if (sonic_field.transient_stage[voice] == 1) {
                    sonic_field.transient[voice] += (target[voice].percussion - sonic_field.transient[voice]) * .0014f;
                    if (sonic_field.transient[voice] >= target[voice].percussion * .82f) sonic_field.transient_stage[voice] = 2;
                } else if (sonic_field.transient_stage[voice] == 2) {
                    sonic_field.transient[voice] *= .99968f;
                    if (sonic_field.transient[voice] < .0002f) {
                        sonic_field.transient[voice] = 0;
                        sonic_field.transient_stage[voice] = 0;
                    }
                }
                if (sonic_field.transient_stage[voice]) {
                    sample += tone * target[voice].gain * sonic_field.transient[voice] * 1.7f;
                }
                float left_gain = sqrtf((1.0f - current->pan) * .5f);
                float right_gain = sqrtf((1.0f + current->pan) * .5f);
                left += sample * left_gain;
                right += sample * right_gain;
            }
            sonic_field.buffer[frame * 2] = tanhf(left);
            sonic_field.buffer[frame * 2 + 1] = tanhf(right);
            sonic_field.sample_clock++;
        }
        SDL_PutAudioStreamData(stream, sonic_field.buffer,
                               frames * (int)(sizeof(float) * 2));
        frames_left -= frames;
    }
}

static bool sonic_self_test(void) {
    Program program = {.novelty = .75f, .quality = .5f};
    snprintf(program.id, sizeof(program.id), "fixed-program");
    snprintf(program.source, sizeof(program.source), "(sort merge)");
    SonicVoice left = sonic_voice(&program, 0, false, 1);
    SonicVoice right = sonic_voice(&program, 3, false, 1);
    SonicVoice repeat = sonic_voice(&program, 0, false, 1);
    return fabsf(left.pan + .82f) < .0001f && fabsf(right.pan - .82f) < .0001f &&
           left.frequency == repeat.frequency && left.gain == repeat.gain &&
           left.frequency != right.frequency && left.frequency >= 6.875f && right.frequency <= 400.0f &&
           left.sub >= .08f && left.overtone >= .04f;
}

static int split_tabs(char *line, char **fields, int cap) {
    int count = 0;
    char *cursor = line;
    while (count < cap) {
        fields[count++] = cursor;
        char *tab = strchr(cursor, '\t');
        if (!tab) break;
        *tab = '\0';
        cursor = tab + 1;
    }
    return count;
}

static void parse_values(Program *p, const char *text) {
    char copy[512];
    snprintf(copy, sizeof(copy), "%s", text);
    char *save = NULL;
    for (char *token = strtok_r(copy, ",", &save);
         token && p->value_count < MAX_VALUES;
         token = strtok_r(NULL, ",", &save))
        p->values[p->value_count++] = atoi(token);
}

static int hex_nibble(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    return -1;
}

static int parse_hex(uint8_t *output, int capacity, const char *text) {
    int count = 0;
    size_t length = strlen(text);
    for (size_t i = 0; i + 1 < length && count < capacity; i += 2) {
        int high = hex_nibble(text[i]), low = hex_nibble(text[i + 1]);
        if (high < 0 || low < 0) return 0;
        output[count++] = (uint8_t)(high * 16 + low);
    }
    return count;
}

static bool parse_state(char *body, FarmState *out) {
    memset(out, 0, sizeof(*out));
    char *save_line = NULL;
    for (char *line = strtok_r(body, "\n", &save_line); line;
         line = strtok_r(NULL, "\n", &save_line)) {
        char *f[112];
        int n = split_tabs(line, f, 112);
        if (n >= 10 && !strcmp(f[0], "S")) {
            out->iteration = strtoull(f[1], NULL, 10);
            out->accepted = strtoull(f[2], NULL, 10);
            out->rejected = strtoull(f[3], NULL, 10);
            out->coverage = atoi(f[4]); out->capacity = atoi(f[5]);
            snprintf(out->selected, sizeof(out->selected), "%s", f[6]);
            out->checkpoint_next = strtoull(f[7], NULL, 10);
            out->checkpoint_remaining = strtoull(f[8], NULL, 10);
            out->checkpoint_ms = strtoull(f[9], NULL, 10);
            if (n >= 16) {
                out->raster_coverage = atoi(f[10]); out->raster_capacity = atoi(f[11]);
                out->resident_bytes = atoi(f[12]); out->active_reads = atoi(f[13]);
                out->active_writes = atoi(f[14]); out->evaluations_per_second = strtof(f[15], NULL);
            }
            if (n >= 19) {
                out->utc_ms = strtoull(f[16], NULL, 10); out->musical_bpm = strtof(f[17], NULL);
                out->clock_synced = atoi(f[18]) != 0;
            }
            if (n >= 23) {
                out->visual_reviews = atoi(f[19]); out->visual_retain = atoi(f[20]);
                out->visual_watch = atoi(f[21]); out->visual_reject = atoi(f[22]);
            }
            if (n >= 26) {
                out->git_editions = atoi(f[23]); snprintf(out->git_head, sizeof(out->git_head), "%.8s", f[24]);
                out->git_iteration = strtoull(f[25], NULL, 10);
            }
            if (n >= 32) {
                out->piece_vm_generation = atoi(f[26]);
                snprintf(out->piece_vm_mutation, sizeof(out->piece_vm_mutation), "%s", f[27]);
                out->piece_vm_accepted = atoi(f[28]); out->piece_vm_rejected = atoi(f[29]);
                out->piece_vm_lineage = atoi(f[30]);
                snprintf(out->piece_vm_id, sizeof(out->piece_vm_id), "%.12s", f[31]);
            }
            if (n >= 40) {
                out->piece_vm_registers = atoi(f[32]); out->piece_vm_calls = atoi(f[33]);
                out->piece_vm_memory = atoi(f[34]); out->piece_vm_senses = atoi(f[35]);
                out->piece_vm_score = strtof(f[36], NULL);
                out->piece_vm_half_verified = atoi(f[37]) != 0;
                out->piece_vm_standard_verified = atoi(f[38]) != 0;
                out->piece_vm_double_verified = atoi(f[39]) != 0;
            }
            if (n >= 41) out->piece_vm_crossovers = atoi(f[40]);
            if (n >= 47) {
                out->piece_vm_functions = atoi(f[41]); out->piece_vm_arguments = atoi(f[42]);
                out->piece_vm_layouts = atoi(f[43]); out->piece_vm_layout_bytes = atoi(f[44]);
                snprintf(out->piece_vm_environment_capability, sizeof(out->piece_vm_environment_capability), "%.15s", f[45]);
                snprintf(out->piece_vm_environment_donor, sizeof(out->piece_vm_environment_donor), "%.12s", f[46]);
            }
            out->margin_probe_address = -1;
            if (n >= 52) {
                snprintf(out->margin_probe_id, sizeof(out->margin_probe_id), "%.23s", f[47]);
                out->margin_probe_address = atoi(f[48]);
                snprintf(out->margin_probe_track, sizeof(out->margin_probe_track), "%.15s", f[49]);
                snprintf(out->margin_probe_capability, sizeof(out->margin_probe_capability), "%.15s", f[50]);
                snprintf(out->margin_probe_requested_by, sizeof(out->margin_probe_requested_by), "%.23s", f[51]);
            }
            if (n >= 59) {
                snprintf(out->margin_probe_status, sizeof(out->margin_probe_status), "%.15s", f[52]);
                snprintf(out->margin_probe_descendant_id, sizeof(out->margin_probe_descendant_id), "%.12s", f[53]);
                out->margin_probe_attempts = atoi(f[54]);
                snprintf(out->margin_probe_descendant_state, sizeof(out->margin_probe_descendant_state), "%.15s", f[55]);
                out->margin_probe_descendants = atoi(f[56]); out->margin_probe_generation = atoi(f[57]);
                out->margin_probe_children = atoi(f[58]);
            }
            if (n >= 64) {
                out->margin_probe_propagation_descendants = atoi(f[59]);
                out->margin_probe_propagation_residents = atoi(f[60]);
                out->margin_probe_propagation_generation = atoi(f[61]);
                snprintf(out->margin_probe_propagation_frontier,
                         sizeof(out->margin_probe_propagation_frontier), "%.12s", f[62]);
                out->margin_probe_champion_carrier = atoi(f[63]) != 0;
            }
            if (n >= 71) {
                snprintf(out->piece_vm_selection_parent, sizeof(out->piece_vm_selection_parent), "%.12s", f[64]);
                out->piece_vm_phenotype_reports = atoi(f[65]);
                out->piece_vm_phenotype_ready = atoi(f[66]) != 0;
                out->piece_vm_phenotype_score = strtof(f[67], NULL);
                out->piece_vm_phenotype_bias = strtof(f[68], NULL);
                out->piece_vm_phenotype_voices = atoi(f[69]);
                snprintf(out->piece_vm_phenotype_role, sizeof(out->piece_vm_phenotype_role), "%.15s", f[70]);
            }
            if (n >= 84) {
                snprintf(out->piece_vm_policy_bonus, sizeof(out->piece_vm_policy_bonus), "%.23s", f[71]);
                for (int policy = 0; policy < 3; policy++) {
                    int base = 72 + policy * 4;
                    out->piece_vm_policy_trials[policy] = atoi(f[base]);
                    out->piece_vm_policy_reward[policy] = strtof(f[base + 1], NULL);
                    out->piece_vm_policy_admission[policy] = strtof(f[base + 2], NULL);
                    out->piece_vm_policy_capability[policy] = strtof(f[base + 3], NULL);
                }
            }
            if (n >= 94) {
                snprintf(out->piece_vm_operator_bonus, sizeof(out->piece_vm_operator_bonus), "%.15s", f[84]);
                for (int family = 0; family < 3; family++) {
                    int base = 85 + family * 3;
                    out->piece_vm_operator_trials[family] = atoi(f[base]);
                    out->piece_vm_operator_reward[family] = strtof(f[base + 1], NULL);
                    out->piece_vm_operator_capability[family] = strtof(f[base + 2], NULL);
                }
            }
            if (n >= 95)
                snprintf(out->piece_vm_mutation_bonus, sizeof(out->piece_vm_mutation_bonus), "%.31s", f[94]);
            if (n >= 102) {
                out->piece_vm_curriculum_lead = atoi(f[95]) != 0;
                out->piece_vm_curriculum_trials = atoi(f[96]);
                out->piece_vm_curriculum_advancements = atoi(f[97]);
                out->piece_vm_curriculum_compound = atoi(f[98]);
                out->piece_vm_curriculum_max_breadth = atoi(f[99]);
                out->piece_vm_development_breadth = atoi(f[100]);
                snprintf(out->piece_vm_development_signature,
                         sizeof(out->piece_vm_development_signature), "%.5s", f[101]);
            }
        } else if (n >= 12 && !strcmp(f[0], "P") && out->program_count < MAX_PROGRAMS) {
            Program *p = &out->programs[out->program_count++];
            snprintf(p->id, sizeof(p->id), "%s", f[1]);
            snprintf(p->origin, sizeof(p->origin), "%s", f[2]);
            snprintf(p->status, sizeof(p->status), "%s", f[3]);
            p->generation = atoi(f[4]); p->novelty = strtof(f[5], NULL);
            p->quality = strtof(f[6], NULL); p->operations = atoi(f[7]);
            p->retained = atoi(f[8]) != 0;
            snprintf(p->parent, sizeof(p->parent), "%s", f[9]);
            snprintf(p->source, sizeof(p->source), "%s", f[10]);
            parse_values(p, f[11]);
            if (n >= 16) {
                snprintf(p->domain, sizeof(p->domain), "%s", f[12]);
                p->raster_count = parse_hex(p->raster, RASTER_BYTES, f[13]);
                p->raster_width = atoi(f[14]); p->raster_height = atoi(f[15]);
                if (n >= 17) p->bytecode_count = parse_hex(p->bytecode, BYTECODE_MAX, f[16]);
                if (n >= 18) snprintf(p->address, sizeof(p->address), "%s", f[17]);
                if (n >= 19) p->groove_count = parse_hex(p->groove, GROOVE_BYTES, f[18]);
                if (n >= 20) p->piece_vm_count = parse_hex(p->piece_vm, PIECE_VM_BYTECODE_MAX, f[19]);
                if (n >= 21) p->piece_vm_resolution = atoi(f[20]);
                if (n >= 22) p->piece_vm_probe_carrier = atoi(f[21]) != 0;
                if (n >= 23) snprintf(p->piece_vm_source_id, sizeof(p->piece_vm_source_id), "%.12s", f[22]);
                if (n >= 24) snprintf(p->piece_vm_role, sizeof(p->piece_vm_role), "%.15s", f[23]);
            }
        }
    }
    out->received_at = SDL_GetTicks();
    out->connected = out->capacity > 0;
    return out->connected;
}

static void stabilize_program_addresses(FarmState *state) {
    bool incoming_used[MAX_PROGRAMS] = {0};
    for (int i = 0; i < state->program_count; i++) {
        const char *address = state->programs[i].address;
        if (address[0] < 'A' || address[0] > 'D' || address[1] < '1' || address[1] > '3') continue;
        int slot = (address[1] - '1') * 4 + address[0] - 'A';
        stable_programs[slot] = state->programs[i];
        stable_missing[slot] = 0; stable_occupied[slot] = true; incoming_used[i] = true;
    }
    for (int slot = 0; slot < MAX_PROGRAMS; slot++) if (stable_occupied[slot]) {
        bool found = false;
        for (int i = 0; i < state->program_count; i++) {
            if (!strcmp(stable_programs[slot].id, state->programs[i].id)) {
                stable_programs[slot] = state->programs[i];
                stable_missing[slot] = 0;
                incoming_used[i] = true;
                found = true;
                break;
            }
        }
        if (!found) stable_missing[slot]++;
    }
    for (int i = 0; i < state->program_count; i++) if (!incoming_used[i]) {
        int destination = -1;
        for (int slot = 0; slot < MAX_PROGRAMS; slot++)
            if (!stable_occupied[slot]) { destination = slot; break; }
        if (destination < 0) {
            int stalest = 0;
            for (int slot = 1; slot < MAX_PROGRAMS; slot++)
                if (stable_missing[slot] > stable_missing[stalest]) stalest = slot;
            if (stable_missing[stalest] >= 120) destination = stalest;
        }
        if (destination >= 0) {
            stable_programs[destination] = state->programs[i];
            stable_missing[destination] = 0;
            stable_occupied[destination] = true;
        }
    }
    state->program_count = MAX_PROGRAMS;
    for (int slot = 0; slot < MAX_PROGRAMS; slot++) {
        if (stable_occupied[slot]) state->programs[slot] = stable_programs[slot];
        else memset(&state->programs[slot], 0, sizeof(Program));
    }
}

static int field_index(int x, int y, int channel) {
    x = (x % RASTER_W + RASTER_W) % RASTER_W;
    y = (y % RASTER_H + RASTER_H) % RASTER_H;
    return (y * RASTER_W + x) * 3 + channel;
}

static bool valid_resolution(int side) {
    return side == 32 || side == 64 || side == 128 || side == 256;
}

static int hardware_profile_code(int side) {
    return side == 32 ? 1 : side == 64 ? 2 : side == 128 ? 3 : side == 256 ? 4 : 0;
}

static int live_index(const LiveField *field, int x, int y, int channel) {
    int side = field->resolution;
    x = (x % side + side) % side;
    y = (y % side + side) % side;
    return (y * side + x) * 3 + channel;
}

static int scale_coordinate(const LiveField *field, int value) {
    return (int)lround(value * (field->resolution - 1) / (double)(RASTER_W - 1));
}

static int scale_extent(const LiveField *field, int value) {
    int scaled = (int)lround(value * field->resolution / (double)RASTER_W);
    return scaled < 1 ? 1 : scaled;
}

static int scale_offset(const LiveField *field, int value) {
    if (!value) return 0;
    int scaled = (int)lround(abs(value) * field->resolution / (double)RASTER_W);
    if (scaled < 1) scaled = 1;
    return value < 0 ? -scaled : scaled;
}

static void resample_rgb(uint8_t *target, int target_side, const uint8_t *source, int source_side) {
    for (int y = 0; y < target_side; y++) for (int x = 0; x < target_side; x++) {
        int sx = x * source_side / target_side, sy = y * source_side / target_side;
        int from = (sy * source_side + sx) * 3, to = (y * target_side + x) * 3;
        target[to] = source[from]; target[to + 1] = source[from + 1]; target[to + 2] = source[from + 2];
    }
}

static int16_t bytecode_arg(const uint8_t *instruction, int index) {
    int offset = 1 + index * 2;
    return (int16_t)((uint16_t)instruction[offset] | ((uint16_t)instruction[offset + 1] << 8));
}

static uint16_t groove_u16(const uint8_t *bytes, int offset) {
    return (uint16_t)(bytes[offset] | ((uint16_t)bytes[offset + 1] << 8));
}

static uint32_t groove_u32(const uint8_t *bytes, int offset) {
    return (uint32_t)bytes[offset] | ((uint32_t)bytes[offset + 1] << 8) |
        ((uint32_t)bytes[offset + 2] << 16) | ((uint32_t)bytes[offset + 3] << 24);
}

static void groove_set_u16(uint8_t *bytes, int offset, uint16_t value) {
    bytes[offset] = value & 255; bytes[offset + 1] = value >> 8;
}

static void groove_set_u32(uint8_t *bytes, int offset, uint32_t value) {
    bytes[offset] = value & 255; bytes[offset + 1] = value >> 8;
    bytes[offset + 2] = value >> 16; bytes[offset + 3] = value >> 24;
}

static uint32_t groove_protected_hash(const uint8_t *groove) {
    uint32_t value = 2166136261u;
    for (int offset = 0; offset < GROOVE_SEQUENCE_BASE * 3; offset++) {
        uint8_t byte = offset >= 12 && offset < 16 ? 0 : groove[offset];
        value ^= byte; value *= 16777619u;
    }
    const int ranges[][2] = {
        {GROOVE_SEQUENCE_BASE * 3, GROOVE_STATE_BASE * 3},
        {GROOVE_SOURCE_BASE * 3, GROOVE_FRINGE_BASE * 3},
    };
    for (size_t range = 0; range < sizeof(ranges) / sizeof(ranges[0]); range++)
        for (int offset = ranges[range][0]; offset < ranges[range][1]; offset++) {
            value ^= groove[offset]; value *= 16777619u;
        }
    return value;
}

static void groove_rehash(LiveField *field) {
    groove_set_u32(field->groove, 12, groove_protected_hash(field->groove));
}

static bool groove_valid(const uint8_t *groove, int count) {
    int resolution = count == GROOVE_BYTES ? groove_u16(groove, 104) : 0;
    return count == GROOVE_BYTES && !memcmp(groove, "PGR1", 4) && groove_u16(groove, 4) == GROOVE_VERSION &&
        groove_u32(groove, 8) == GROOVE_BYTES && groove_u16(groove, 20) <= 8 &&
        valid_resolution(resolution) && groove_u32(groove, 108) == (uint32_t)(resolution * resolution * 3) &&
        groove[112] == hardware_profile_code(resolution) &&
        groove_u32(groove, 12) == groove_protected_hash(groove);
}

static int groove_instruction_count(const LiveField *field) {
    return groove_u16(field->groove, 20);
}

static void groove_set_reader(LiveField *field, int pc, int vector) {
    int state = GROOVE_STATE_BASE * 3;
    field->groove[state] = 0xa0;
    groove_set_u16(field->groove, state + 1, (uint16_t)pc);
    field->groove[state + 3] = (uint8_t)vector;
    groove_set_u32(field->groove, state + 20, GROOVE_SEQUENCE_BASE + pc * GROOVE_INSTRUCTION_PIXELS);
}

static void groove_sync_microcell(LiveField *field, int meta_index) {
    int pixel = -1;
    if (meta_index >= MARGIN_RASTER_BODY_BASE && meta_index < MARGIN_RASTER_BODY_BASE + MARGIN_RASTER_BODY_SLOTS * MARGIN_BODY_CELLS)
        pixel = GROOVE_BODY_BASE + meta_index - MARGIN_RASTER_BODY_BASE;
    else if (meta_index >= MARGIN_PROJECTION_BODY_BASE && meta_index < MARGIN_PROJECTION_BODY_BASE + MARGIN_PROJECTION_AXES * MARGIN_BODY_CELLS)
        pixel = GROOVE_PROJECTION_BASE + meta_index - MARGIN_PROJECTION_BODY_BASE;
    else if (meta_index >= MARGIN_PROPOSAL_BASE && meta_index < MARGIN_PROPOSAL_BASE + MARGIN_PROJECTION_AXES * MARGIN_BODY_CELLS)
        pixel = GROOVE_PROPOSAL_BASE + meta_index - MARGIN_PROPOSAL_BASE;
    if (pixel < 0) return;
    MetaCell *cell = &field->permastore[meta_index];
    int at = pixel * 3; float literal = 0; memcpy(&literal, &cell->payload, sizeof(literal));
    field->groove[at] = cell->tag;
    field->groove[at + 1] = (uint8_t)cell->arity;
    field->groove[at + 2] = (uint8_t)(int8_t)lroundf(fmaxf(-2, fminf(1.984375f, literal)) * 64);
}

static void groove_decode_microcells(LiveField *field, int groove_pixel, int meta_base, int cells) {
    for (int index = 0; index < cells; index++) {
        int at = (groove_pixel + index) * 3;
        MetaCell *cell = &field->permastore[meta_base + index];
        memset(cell, 0, sizeof(*cell));
        cell->tag = field->groove[at]; cell->flags = cell->tag == 0x90 ? 1 : 2;
        cell->arity = field->groove[at + 1];
        float literal = (int8_t)field->groove[at + 2] / 64.0f;
        memcpy(&cell->payload, &literal, sizeof(literal));
        cell->link = (uint32_t)(index + 1 < cells ? meta_base + index + 1 : meta_base);
    }
}

static void groove_decode_program(LiveField *field) {
    field->bytecode_count = groove_instruction_count(field) * BYTECODE_STRIDE;
    for (int opcode = 1; opcode <= MARGIN_FUNCTION_CELLS; opcode++) {
        int at = (GROOVE_FUNCTION_BASE + (opcode - 1) * 3) * 3;
        MetaCell *function = &field->permastore[MARGIN_FUNCTION_BASE + opcode - 1];
        memset(function, 0, sizeof(*function));
        function->tag = field->groove[at]; function->flags = field->groove[at + 1];
        function->arity = field->groove[at + 2];
        function->payload = field->groove[at + 7];
        if (function->flags & 4) function->link = MARGIN_RASTER_BODY_BASE + (opcode - 1) * MARGIN_BODY_CELLS;
        groove_decode_microcells(field, GROOVE_BODY_BASE + (opcode - 1) * MARGIN_BODY_CELLS,
            MARGIN_RASTER_BODY_BASE + (opcode - 1) * MARGIN_BODY_CELLS, MARGIN_BODY_CELLS);
    }
    for (int axis = 0; axis < MARGIN_PROJECTION_AXES; axis++)
        groove_decode_microcells(field, GROOVE_PROJECTION_BASE + axis * MARGIN_BODY_CELLS,
            MARGIN_PROJECTION_BODY_BASE + axis * MARGIN_BODY_CELLS, MARGIN_BODY_CELLS);
}

static void margin_store_program(LiveField *field, const uint8_t *bytecode, int count) {
    field->bytecode_count = count;
    memset(field->groove, 0, sizeof(field->groove));
    memcpy(field->groove, "PGR1", 4); groove_set_u16(field->groove, 4, GROOVE_VERSION);
    groove_set_u16(field->groove, 6, GROOVE_HEADER_BASE + 64);
    groove_set_u32(field->groove, 8, GROOVE_BYTES); groove_set_u16(field->groove, 20, count / BYTECODE_STRIDE);
    size_t id_length = 0;
    while (id_length < 24 && field->id[id_length]) id_length++;
    memcpy(field->groove + 32, field->id, id_length);
    groove_set_u16(field->groove, 104, (uint16_t)field->resolution);
    groove_set_u32(field->groove, 108, (uint32_t)field->byte_count);
    field->groove[112] = (uint8_t)hardware_profile_code(field->resolution);
    memcpy(field->groove + GROOVE_SEQUENCE_BASE * 3, bytecode, (size_t)count);
    groove_set_reader(field, 0, 7);
    int instructions = count / BYTECODE_STRIDE;
    for (int opcode = 1; opcode <= MARGIN_FUNCTION_CELLS; opcode++) {
        MetaCell *function = &field->permastore[MARGIN_FUNCTION_BASE + opcode - 1];
        function->tag = 0x70;       /* verified bounded native function signature */
        function->flags = 1;        /* executable only through the verifier */
        function->arity = 10;
        function->link = 0;
        function->payload = (uint64_t)opcode | ((uint64_t)field->byte_count << 16);
        int entry = (GROOVE_FUNCTION_BASE + (opcode - 1) * 3) * 3;
        field->groove[entry] = 0x70; field->groove[entry + 1] = 1; field->groove[entry + 2] = 4;
        field->groove[entry + 3] = (uint8_t)opcode;
        groove_set_u16(field->groove, entry + 4, GROOVE_BODY_BASE + (opcode - 1) * MARGIN_BODY_CELLS);
        field->groove[entry + 6] = 0; field->groove[entry + 7] = (uint8_t)opcode;
    }
    /* Opcode 19 is a bounded native field rule. Its signature fits in the
       function directory, while the original 18 micro-bodies remain fixed. */
    int cellular_entry = (GROOVE_FUNCTION_BASE + (MAX_RASTER_OPCODE - 1) * 3) * 3;
    field->groove[cellular_entry] = 0x70; field->groove[cellular_entry + 1] = 1;
    field->groove[cellular_entry + 2] = 2; field->groove[cellular_entry + 3] = MAX_RASTER_OPCODE;
    groove_set_u16(field->groove, cellular_entry + 4, 0);
    field->groove[cellular_entry + 6] = 0; field->groove[cellular_entry + 7] = MAX_RASTER_OPCODE;
    for (int instruction = 0; instruction < instructions; instruction++) {
        int base = instruction * MARGIN_INSTRUCTION_CELLS;
        int opcode = bytecode[instruction * BYTECODE_STRIDE];
        for (int word = 0; word < MARGIN_INSTRUCTION_CELLS; word++) {
            uint64_t payload = 0;
            memcpy(&payload, bytecode + instruction * BYTECODE_STRIDE + word * 8, 8);
            field->permastore[base + word].tag = (uint8_t)(0x80 + word);
            field->permastore[base + word].flags = 1;
            field->permastore[base + word].arity = BYTECODE_STRIDE;
            field->permastore[base + word].link = (uint32_t)(MARGIN_FUNCTION_BASE + opcode - 1);
            field->permastore[base + word].payload = payload;
        }
    }
}

static bool margin_load_instruction(const LiveField *field, int instruction, uint8_t output[BYTECODE_STRIDE]) {
    if (instruction < 0 || instruction >= groove_instruction_count(field)) return false;
    memcpy(output, field->groove + (GROOVE_SEQUENCE_BASE + instruction * GROOVE_INSTRUCTION_PIXELS) * 3, BYTECODE_STRIDE);
    int opcode = output[0];
    if (opcode == MAX_RASTER_OPCODE) {
        int entry = (GROOVE_FUNCTION_BASE + (opcode - 1) * 3) * 3;
        return field->groove[entry] == 0x70 && field->groove[entry + 3] == opcode;
    }
    return opcode >= 1 && opcode <= MARGIN_FUNCTION_CELLS &&
        field->permastore[MARGIN_FUNCTION_BASE + opcode - 1].tag == 0x70;
}

static void margin_set_float(MetaCell *cell, float value, uint8_t tag) {
    cell->tag = tag; cell->arity = 1;
    cell->payload = 0; memcpy(&cell->payload, &value, sizeof(value));
}

static float margin_get_float(const MetaCell *cell, float fallback) {
    if (!cell || (cell->tag != 3 && cell->tag != 4 && cell->tag != 5)) return fallback;
    float value; memcpy(&value, &cell->payload, sizeof(value));
    return isfinite(value) ? value : fallback;
}

enum MarginMicroOp {
    MM_SELF = 1, MM_ARG, MM_X, MM_Y, MM_DEPTH, MM_TIME, MM_ENERGY, MM_CONST,
    MM_ADD, MM_SUB, MM_MUL, MM_DIV, MM_XOR, MM_AND, MM_OR,
    MM_SIN, MM_COS, MM_TANH, MM_ABS, MM_MIN, MM_MAX, MM_SOLAR, MM_RETURN,
};

typedef struct {
    float self, arg, x, y, depth, time, energy;
} MarginEval;

static void margin_microcell(MetaCell *cell, uint8_t tag, enum MarginMicroOp op, float literal) {
    memset(cell, 0, sizeof(*cell));
    cell->tag = tag;
    cell->flags = tag == 0x90 ? 1 : 2; /* protected executable or fringe proposal */
    cell->arity = (uint16_t)op;
    memcpy(&cell->payload, &literal, sizeof(literal));
}

static bool margin_verify_body(const LiveField *field, int base, uint8_t tag) {
    if (base < 0 || base + MARGIN_BODY_CELLS > PERMA_CELLS) return false;
    int depth = 0;
    for (int index = 0; index < MARGIN_BODY_CELLS; index++) {
        const MetaCell *cell = &field->permastore[base + index];
        if (cell->tag != tag || !(cell->flags & (tag == 0x90 ? 1 : 2))) return false;
        enum MarginMicroOp op = (enum MarginMicroOp)cell->arity;
        if (op >= MM_SELF && op <= MM_CONST) {
            if (++depth > 12) return false;
            if (op == MM_CONST) {
                float literal = 0; memcpy(&literal, &cell->payload, sizeof(literal));
                if (!isfinite(literal) || fabsf(literal) > 4) return false;
            }
        } else if (op >= MM_ADD && op <= MM_OR) {
            if (depth < 2) return false;
            depth--;
        } else if (op >= MM_SIN && op <= MM_ABS) {
            if (depth < 1) return false;
        } else if (op == MM_MIN || op == MM_MAX || op == MM_SOLAR) {
            if (depth < 2) return false;
            depth--;
        } else if (op == MM_RETURN) {
            return depth == 1;
        } else return false;
    }
    return false;
}

static bool margin_eval_body(const LiveField *field, int base, uint8_t tag,
                             const MarginEval *input, float *result) {
    float stack[12]; int depth = 0;
    for (int index = 0; index < MARGIN_BODY_CELLS; index++) {
        const MetaCell *cell = &field->permastore[base + index];
        if (cell->tag != tag || !(cell->flags & (tag == 0x90 ? 1 : 2))) return false;
        enum MarginMicroOp op = (enum MarginMicroOp)cell->arity;
        float value = 0;
        if (op >= MM_SELF && op <= MM_CONST) {
            if (depth >= 12) return false;
            if (op == MM_SELF) value = input->self;
            else if (op == MM_ARG) value = input->arg;
            else if (op == MM_X) value = input->x;
            else if (op == MM_Y) value = input->y;
            else if (op == MM_DEPTH) value = input->depth;
            else if (op == MM_TIME) value = input->time;
            else if (op == MM_ENERGY) value = input->energy;
            else memcpy(&value, &cell->payload, sizeof(value));
            stack[depth++] = value;
            continue;
        }
        if (op == MM_RETURN) {
            if (depth != 1 || !isfinite(stack[0])) return false;
            *result = stack[0]; return true;
        }
        if (op >= MM_SIN && op <= MM_ABS) {
            if (depth < 1) return false;
            float a = stack[depth - 1];
            stack[depth - 1] = op == MM_SIN ? sinf(a) : op == MM_COS ? cosf(a) :
                op == MM_TANH ? tanhf(a) : fabsf(a);
        } else {
            if (depth < 2) return false;
            float b = stack[--depth], a = stack[depth - 1];
            if (op == MM_ADD) value = a + b;
            else if (op == MM_SUB) value = a - b;
            else if (op == MM_MUL) value = a * b;
            else if (op == MM_DIV) value = fabsf(b) < .0001f ? a : a / b;
            else if (op == MM_XOR) value = (float)(((int)lroundf(a) & 255) ^ ((int)lroundf(b) & 255));
            else if (op == MM_AND) value = (float)(((int)lroundf(a) & 255) & ((int)lroundf(b) & 255));
            else if (op == MM_OR) value = (float)(((int)lroundf(a) & 255) | ((int)lroundf(b) & 255));
            else if (op == MM_MIN) value = fminf(a, b);
            else if (op == MM_MAX) value = fmaxf(a, b);
            else if (op == MM_SOLAR) value = a >= b ? 255 - a : a;
            else return false;
            stack[depth - 1] = value;
        }
        if (!isfinite(stack[depth - 1])) return false;
        stack[depth - 1] = fmaxf(-4096, fminf(4096, stack[depth - 1]));
    }
    return false;
}

static void margin_write_body(LiveField *field, int base, uint8_t tag,
                              const enum MarginMicroOp *ops, const float *literals, int count) {
    for (int index = 0; index < MARGIN_BODY_CELLS; index++) {
        enum MarginMicroOp op = index < count ? ops[index] : MM_RETURN;
        float literal = literals && index < count ? literals[index] : 0;
        margin_microcell(&field->permastore[base + index], tag, op, literal);
        field->permastore[base + index].link = (uint32_t)(index + 1 < MARGIN_BODY_CELLS ? base + index + 1 : base);
        groove_sync_microcell(field, base + index);
    }
}

static void margin_seed_raster_bodies(LiveField *field) {
    static const struct { int opcode; enum MarginMicroOp op; } seeds[] = {
        {1, MM_ADD}, {2, MM_XOR}, {5, MM_SOLAR}, {11, MM_AND}, {12, MM_OR},
    };
    for (size_t seed = 0; seed < sizeof(seeds) / sizeof(seeds[0]); seed++) {
        int base = MARGIN_RASTER_BODY_BASE + (seeds[seed].opcode - 1) * MARGIN_BODY_CELLS;
        enum MarginMicroOp ops[] = {MM_SELF, MM_ARG, seeds[seed].op, MM_RETURN};
        margin_write_body(field, base, 0x90, ops, NULL, 4);
        if (margin_verify_body(field, base, 0x90)) {
            MetaCell *function = &field->permastore[MARGIN_FUNCTION_BASE + seeds[seed].opcode - 1];
            function->link = (uint32_t)base;
            function->flags |= 4; /* body is margin-resident and verified */
            function->arity = 2;
            int entry = (GROOVE_FUNCTION_BASE + (seeds[seed].opcode - 1) * 3) * 3;
            field->groove[entry + 1] = function->flags;
            field->groove[entry + 2] = (uint8_t)function->arity;
            field->groove[entry + 6] = 4;
        }
    }
}

static void margin_seed_projection_bodies(LiveField *field) {
    enum MarginMicroOp x_ops[] = {MM_TIME, MM_SIN, MM_ENERGY, MM_MUL, MM_TANH, MM_RETURN};
    enum MarginMicroOp y_ops[] = {MM_TIME, MM_COS, MM_ENERGY, MM_MUL, MM_TANH, MM_RETURN};
    enum MarginMicroOp z_ops[] = {MM_DEPTH, MM_ENERGY, MM_SUB, MM_TANH, MM_RETURN};
    margin_write_body(field, MARGIN_PROJECTION_BODY_BASE, 0x90, x_ops, NULL, 6);
    margin_write_body(field, MARGIN_PROJECTION_BODY_BASE + MARGIN_BODY_CELLS, 0x90, y_ops, NULL, 6);
    margin_write_body(field, MARGIN_PROJECTION_BODY_BASE + MARGIN_BODY_CELLS * 2, 0x90, z_ops, NULL, 5);
}

static uint32_t margin_random(uint32_t *state) {
    *state ^= *state << 13; *state ^= *state >> 17; *state ^= *state << 5;
    return *state;
}

static enum MarginMicroOp margin_random_input(uint32_t *state) {
    static const enum MarginMicroOp inputs[] = {MM_X, MM_Y, MM_DEPTH, MM_TIME, MM_ENERGY, MM_CONST};
    return inputs[margin_random(state) % (sizeof(inputs) / sizeof(inputs[0]))];
}

static void margin_synthesize_projection(LiveField *field, int axis) {
    int fringe_cells = PERMA_CELLS - GROOVE_FRINGE_BASE;
    int fringe = GROOVE_FRINGE_BASE + (int)((field->lifetime_steps + axis * 977) % fringe_cells);
    int fringe_at = fringe * 3;
    uint32_t fringe_word = ((uint32_t)field->groove[fringe_at] << 16) |
        ((uint32_t)field->groove[fringe_at + 1] << 8) | field->groove[fringe_at + 2];
    uint32_t random = hash_text(field->id) ^ fringe_word ^
        (uint32_t)(field->projection_generation * 0x9e3779b9u) ^ (uint32_t)(axis * 0x85ebca6bu);
    enum MarginMicroOp ops[MARGIN_BODY_CELLS]; float literals[MARGIN_BODY_CELLS] = {0}; int count = 0;
    ops[count] = margin_random_input(&random);
    if (ops[count] == MM_CONST) literals[count] = ((int)(margin_random(&random) % 2001) - 1000) / 1000.0f;
    count++;
    if (margin_random(&random) & 1) ops[count++] = (margin_random(&random) & 1) ? MM_SIN : MM_COS;
    ops[count] = margin_random_input(&random);
    if (ops[count] == MM_CONST) literals[count] = ((int)(margin_random(&random) % 2001) - 1000) / 1000.0f;
    count++;
    if (margin_random(&random) & 1) ops[count++] = MM_TANH;
    static const enum MarginMicroOp binary[] = {MM_ADD, MM_SUB, MM_MUL, MM_DIV, MM_MIN, MM_MAX};
    ops[count++] = binary[margin_random(&random) % (sizeof(binary) / sizeof(binary[0]))];
    if (margin_random(&random) & 1) {
        ops[count] = margin_random_input(&random);
        if (ops[count] == MM_CONST) literals[count] = ((int)(margin_random(&random) % 2001) - 1000) / 1000.0f;
        count++;
        ops[count++] = binary[margin_random(&random) % (sizeof(binary) / sizeof(binary[0]))];
    }
    ops[count++] = MM_TANH; ops[count++] = MM_RETURN;
    int proposal = MARGIN_PROPOSAL_BASE + axis * MARGIN_BODY_CELLS;
    margin_write_body(field, proposal, 0x91, ops, literals, count);
    if (!margin_verify_body(field, proposal, 0x91)) return;
    float low = 1e9f, high = -1e9f;
    for (int sample = 0; sample < 64; sample++) {
        MarginEval input = {.x = (sample % 4) / 1.5f - 1, .y = ((sample / 4) % 4) / 1.5f - 1,
            .depth = ((sample / 16) % 4) / 3.0f, .time = (sample % 8) * .7f,
            .energy = .05f + (sample % 7) * .13f};
        float value = 0;
        if (!margin_eval_body(field, proposal, 0x91, &input, &value) || !isfinite(value) || fabsf(value) > 1.001f) return;
        low = fminf(low, value); high = fmaxf(high, value);
    }
    if (high - low < .008f) return;
    int active = MARGIN_PROJECTION_BODY_BASE + axis * MARGIN_BODY_CELLS;
    memcpy(&field->permastore[active], &field->permastore[proposal], sizeof(MetaCell) * MARGIN_BODY_CELLS);
    for (int index = 0; index < MARGIN_BODY_CELLS; index++) {
        field->permastore[active + index].tag = 0x90;
        field->permastore[active + index].flags = 1;
        field->permastore[active + index].link = (uint32_t)(index + 1 < MARGIN_BODY_CELLS ? active + index + 1 : active);
        groove_sync_microcell(field, active + index);
    }
    groove_rehash(field);
    field->projection_generation++;
    char event[48]; snprintf(event, sizeof(event), ">ALGEBRA PROMOTE %c G%d", 'X' + axis, field->projection_generation);
    live_console(field, event);
}

static void margin_maybe_synthesize(LiveField *field) {
    if (field->lifetime_steps < field->next_synthesis_step) return;
    margin_synthesize_projection(field, field->projection_generation % MARGIN_PROJECTION_AXES);
    field->next_synthesis_step = field->lifetime_steps + 3600 + hash_text(field->id) % 1800;
}

static void margin_seed_algebra(LiveField *field) {
    static const struct { const char *name; uint16_t arity; uint32_t link; } primitives[MARGIN_ALGEBRA_CELLS] = {
        {"vec3", 3, 0}, {"vec4", 4, 0}, {"mat4.id", 0, 0}, {"mat4.mul", 2, 2},
        {"perspect", 4, MARGIN_CAMERA_BASE}, {"rotate.x", 1, 2}, {"rotate.y", 1, 2},
        {"rotate.z", 1, 2}, {"xform.v4", 2, 1}, {"hom.div", 1, 0},
        {"ray.box", 3, MARGIN_CAMERA_BASE}, {"mobius", 3, 0}, {"rat.norm", 2, 0},
        {"look.at", 3, MARGIN_CAMERA_BASE},
    };
    for (int index = 0; index < MARGIN_ALGEBRA_CELLS; index++) {
        MetaCell *cell = &field->permastore[MARGIN_ALGEBRA_BASE + index];
        cell->tag = 0x50;       /* algebraic projection signature */
        cell->flags = 1;        /* protected, verified seed material */
        cell->arity = primitives[index].arity;
        cell->link = primitives[index].link;
        cell->payload = 0;
        size_t length = strlen(primitives[index].name);
        memcpy(&cell->payload, primitives[index].name, length < sizeof(cell->payload) ? length : sizeof(cell->payload));
    }
}

static uint8_t byte_value(float value) {
    if (value < 0) return 0;
    if (value > 255) return 255;
    return (uint8_t)lroundf(value);
}

static LiveField *find_live_field(const Program *program) {
    if (!program) return NULL;
    for (int i = 0; i < MAX_PROGRAMS; i++)
        if (live_fields[i].occupied && !strcmp(live_fields[i].id, program->id)) return &live_fields[i];
    return NULL;
}

static uint32_t piece_vm_code_hash(const uint8_t *bytes, int count) {
    uint32_t value = 2166136261u;
    for (int index = 0; index < count; index++) { value ^= bytes[index]; value *= 16777619u; }
    return value;
}

static LiveField *live_field(const Program *program) {
    bool has_groove = program && groove_valid(program->groove, program->groove_count);
    bool has_piece_vm = program && program->piece_vm_count > 0 && program->piece_vm_count % 8 == 0;
    if (!program || program->raster_count != RASTER_BYTES || (!has_groove &&
        !has_piece_vm && (program->bytecode_count <= 0 || program->bytecode_count % BYTECODE_STRIDE))) return NULL;
    pthread_mutex_lock(&live_lock);
    LiveField *empty = NULL;
    uint32_t incoming_piece_vm_hash = has_piece_vm ? piece_vm_code_hash(program->piece_vm, program->piece_vm_count) : 0;
    int incoming_piece_vm_resolution = has_piece_vm && valid_resolution(program->piece_vm_resolution)
        ? program->piece_vm_resolution : 64;
    for (int i = 0; i < MAX_PROGRAMS; i++) {
        if (live_fields[i].occupied && !strcmp(live_fields[i].id, program->id)) {
            bool same_piece_vm = has_piece_vm
                ? live_fields[i].piece_vm && live_fields[i].piece_vm_hash == incoming_piece_vm_hash &&
                    live_fields[i].resolution == incoming_piece_vm_resolution
                : !live_fields[i].piece_vm;
            if (same_piece_vm) {
                snprintf(live_fields[i].piece_vm_source_id, sizeof(live_fields[i].piece_vm_source_id),
                         "%s", program->piece_vm_source_id);
                snprintf(live_fields[i].piece_vm_role, sizeof(live_fields[i].piece_vm_role),
                         "%s", program->piece_vm_role);
                live_fields[i].piece_vm_probe_carrier = program->piece_vm_probe_carrier;
                pthread_mutex_unlock(&live_lock); return &live_fields[i];
            }
            empty = &live_fields[i]; break;
        }
        if (!live_fields[i].occupied && !empty) empty = &live_fields[i];
    }
    if (!empty) empty = &live_fields[hash_text(program->id) % MAX_PROGRAMS];
    if (empty->piece_vm) piece_vm_native_destroy(empty->piece_vm);
    memset(empty, 0, sizeof(*empty));
    snprintf(empty->id, sizeof(empty->id), "%s", program->id);
    int recorded_resolution = has_piece_vm ? incoming_piece_vm_resolution : has_groove ? groove_u16(program->groove, 104) : RASTER_W;
    empty->resolution = valid_resolution(recorded_resolution) ? recorded_resolution : RASTER_W;
    empty->pixel_count = empty->resolution * empty->resolution;
    empty->byte_count = empty->pixel_count * 3;
    resample_rgb(empty->seed, empty->resolution, program->raster, RASTER_W);
    memcpy(empty->pixels, empty->seed, (size_t)empty->byte_count);
    memcpy(empty->display[0], empty->seed, (size_t)empty->byte_count);
    memcpy(empty->display[1], empty->seed, (size_t)empty->byte_count);
    memcpy(empty->projection[0], empty->seed, (size_t)empty->byte_count);
    memcpy(empty->projection[1], empty->seed, (size_t)empty->byte_count);
    for (int depth = 0; depth < VOLUME_DEPTH; depth++)
        memcpy(empty->volume[depth], empty->seed, (size_t)empty->byte_count);
    uint32_t meta_seed = hash_text(program->id);
    for (int cell = 0; cell < PERMA_CELLS; cell++) {
        meta_seed ^= meta_seed << 13; meta_seed ^= meta_seed >> 17; meta_seed ^= meta_seed << 5;
        empty->permastore[cell].tag = (uint8_t)(1 + meta_seed % 5); /* int, rational, float, vec3, matrix */
        empty->permastore[cell].arity = empty->permastore[cell].tag == 4 ? 3 : empty->permastore[cell].tag == 5 ? 16 : 1;
        empty->permastore[cell].link = (uint32_t)((cell + 1) % PERMA_CELLS);
        empty->permastore[cell].payload = ((uint64_t)meta_seed << 32) | (meta_seed ^ (uint32_t)cell);
    }
    if (has_groove) {
        memcpy(empty->groove, program->groove, GROOVE_BYTES);
        groove_decode_program(empty);
    } else margin_store_program(empty, program->bytecode, program->bytecode_count);
    if (has_piece_vm) {
        empty->piece_vm = piece_vm_native_create(empty->resolution, program->piece_vm, (size_t)program->piece_vm_count);
        if (!empty->piece_vm) { memset(empty, 0, sizeof(*empty)); pthread_mutex_unlock(&live_lock); return NULL; }
        empty->piece_vm_hash = incoming_piece_vm_hash;
        snprintf(empty->piece_vm_source_id, sizeof(empty->piece_vm_source_id), "%s", program->piece_vm_source_id);
        snprintf(empty->piece_vm_role, sizeof(empty->piece_vm_role), "%s", program->piece_vm_role);
        empty->piece_vm_probe_carrier = program->piece_vm_probe_carrier;
    }
    empty->permastore[MARGIN_CAMERA_BASE].tag = 0x40;
    empty->permastore[MARGIN_CAMERA_BASE].arity = 7;
    empty->permastore[MARGIN_CAMERA_BASE].link = MARGIN_CAMERA_BASE + 1;
    empty->permastore[MARGIN_CAMERA_BASE].payload = 0x43414d4552413344ull; /* CAMERA3D */
    const float camera_seed[7] = {0, 0, -1.35f, 0, 0, .58f, 0};
    for (int value = 0; value < 7; value++) margin_set_float(&empty->permastore[MARGIN_CAMERA_BASE + 1 + value], camera_seed[value], value < 3 ? 4 : 3);
    for (int element = 0; element < 16; element++)
        margin_set_float(&empty->permastore[MARGIN_CAMERA_BASE + 8 + element], element % 5 == 0 ? 1 : 0, 5);
    margin_seed_algebra(empty);
    if (!has_groove) {
        margin_seed_raster_bodies(empty);
        margin_seed_projection_bodies(empty);
        groove_rehash(empty);
    }
    empty->next_synthesis_step = 1800 + hash_text(program->id) % 1800;
    for (int index = 0; index < empty->bytecode_count / BYTECODE_STRIDE; index++) {
        uint8_t instruction[BYTECODE_STRIDE];
        if (!margin_load_instruction(empty, index, instruction)) continue;
        if (instruction[0] != 16) continue;
        int bx = scale_coordinate(empty, bytecode_arg(instruction, 0));
        int by = scale_coordinate(empty, bytecode_arg(instruction, 1));
        int width = scale_extent(empty, bytecode_arg(instruction, 2));
        int height = scale_extent(empty, bytecode_arg(instruction, 3));
        if (bx + width > empty->resolution) width = empty->resolution - bx;
        if (by + height > empty->resolution) height = empty->resolution - by;
        for (int y = by; y < by + height; y++) for (int x = bx; x < bx + width; x++)
            empty->sanctuary[y * empty->resolution + x] = 1;
    }
    empty->born_at = SDL_GetTicks();
    empty->occupied = true;
    pthread_mutex_unlock(&live_lock);
    return empty;
}

static uint32_t field_hash(const uint8_t *pixels, int byte_count) {
    uint32_t hash = 2166136261u;
    for (int i = 0; i < byte_count; i++) { hash ^= pixels[i]; hash *= 16777619u; }
    return hash;
}

static float viability_score(int life_state, float actual, float variance, float spatial, int healthy_steps) {
    float state_factor = life_state == 0 ? 1.0f : life_state == 1 ? .42f : life_state == 2 ? .08f : .24f;
    float response = fminf(1, actual / .035f);
    float differentiation = fminf(1, variance * 3.2f + spatial * 1.4f);
    float continuity = .75f + .25f * fminf(1, healthy_steps / 90.0f);
    return state_factor * (.65f + response * .20f + differentiation * .15f) * continuity;
}

static bool viability_self_test(void) {
    float rich = viability_score(0, .05f, .20f, .25f, 90);
    float quiet = viability_score(0, .004f, .03f, .02f, 90);
    float dormant = viability_score(1, .05f, .20f, .25f, 90);
    float collapsed = viability_score(2, .05f, .20f, .25f, 90);
    return rich > quiet && quiet >= .65f && dormant < rich && collapsed < dormant;
}

static void measure_life(LiveField *field, const uint8_t *before, const uint8_t *after) {
    double change = 0, potential = 0, sum = 0, sum_sq = 0, spatial = 0, delta_noise = 0, chroma_noise = 0, midtones = 0, rgb[3] = {0};
    for (int i = 0; i < field->byte_count; i++) {
        change += abs((int)after[i] - before[i]) / 255.0;
        potential += (before[i] > 127 ? before[i] : 255 - before[i]) / 255.0;
        sum += after[i]; sum_sq += after[i] * after[i]; rgb[i % 3] += after[i];
        if (after[i] >= 64 && after[i] <= 192) midtones++;
    }
    for (int y = 0; y < field->resolution; y++) for (int x = 0; x < field->resolution; x++) for (int c = 0; c < 3; c++) {
        int value = after[live_index(field, x, y, c)];
        spatial += abs(value - after[live_index(field, x + 1, y, c)]) / 255.0;
        spatial += abs(value - after[live_index(field, x, y + 1, c)]) / 255.0;
        int delta = (int)after[live_index(field, x, y, c)] - before[live_index(field, x, y, c)];
        int delta_x = (int)after[live_index(field, x + 1, y, c)] - before[live_index(field, x + 1, y, c)];
        int delta_y = (int)after[live_index(field, x, y + 1, c)] - before[live_index(field, x, y + 1, c)];
        delta_noise += (abs(delta - delta_x) + abs(delta - delta_y)) / 510.0;
        if (c == 0) {
            int at = live_index(field, x, y, 0), right = live_index(field, x + 1, y, 0);
            int chroma = abs((after[at] - after[at + 1]) - (after[right] - after[right + 1]));
            chroma += abs((after[at + 2] - after[at + 1]) - (after[right + 2] - after[right + 1]));
            chroma_noise += chroma / 510.0;
        }
    }
    double mean = sum / field->byte_count;
    field->actual_energy = (float)(change / field->byte_count);
    field->potential_energy = (float)(potential / field->byte_count);
    field->variance_energy = (float)fmax(0, sum_sq / field->byte_count - mean * mean) / 16256.25f;
    field->spatial_energy = (float)(spatial / (field->pixel_count * 6));
    field->noise_energy = (float)(delta_noise / (field->pixel_count * 6) * .72 + chroma_noise / field->pixel_count * .28);
    field->temporal_coherence = field->actual_energy / fmaxf(.0001f, field->actual_energy + field->noise_energy);
    field->muddiness = fminf(1, fmaxf(0, (float)(midtones / field->byte_count) *
        (1 - fminf(1, field->spatial_energy * 3)) * (1 - fminf(1, sqrtf(field->variance_energy) * 1.5f))));
    for (int c = 0; c < 3; c++) field->rgb[c] = (float)(rgb[c] / (field->pixel_count * 255.0));
    uint32_t hash = field_hash(after, field->byte_count);
    field->still_steps = hash == field->previous_hash ? field->still_steps + 1 : 0;
    field->flicker_steps = hash == field->two_back_hash && hash != field->previous_hash ? field->flicker_steps + 1 : 0;
    field->two_back_hash = field->previous_hash; field->previous_hash = hash;
    if (field->variance_energy < .002f && field->spatial_energy < .002f) field->life_state = 2;
    else if (field->flicker_steps >= 4) field->life_state = 3;
    else if (field->still_steps >= 4) field->life_state = 1;
    else field->life_state = 0;
    field->lifetime_steps++;
    float alpha = field->lifetime_steps == 1 ? 1.0f : .08f;
    field->ema_actual += (field->actual_energy - field->ema_actual) * alpha;
    field->ema_noise += (field->noise_energy - field->ema_noise) * alpha;
    field->ema_coherence += (field->temporal_coherence - field->ema_coherence) * alpha;
    field->ema_muddiness += (field->muddiness - field->ema_muddiness) * alpha;
    if (field->life_state == 0) field->healthy_steps++;
    else if (field->healthy_steps > 0) field->healthy_steps--;
    field->recovery_steps = field->life_state == 0 ? field->recovery_steps + 1 : 0;
    if (field->recovery_steps >= 180) field->failed_reprobes = 0;
    /* HP v2 is viability, never aesthetic preference. Noise, coherence and
       muddiness remain visible traits but do not subtract from survival. */
    field->health = viability_score(field->life_state, field->ema_actual,
        field->variance_energy, field->spatial_energy, field->healthy_steps);
}

static void project_live_volume(LiveField *field) {
    int publish = 1 - field->projection_index;
    uint8_t *output = field->projection[publish];
    bool boxed = false;
    for (int index = 0; index < field->bytecode_count / BYTECODE_STRIDE; index++) {
        uint8_t instruction[BYTECODE_STRIDE];
        if (margin_load_instruction(field, index, instruction) && instruction[0] == 16) { boxed = true; break; }
    }
    int desired_mode = boxed ? 2 : field->ema_coherence > .56f && field->variance_energy > .045f ? 1 :
        field->spatial_energy > .24f ? 3 : 0;
    if (desired_mode == field->projection_candidate) field->projection_stability++;
    else { field->projection_candidate = desired_mode; field->projection_stability = 0; }
    if (field->projection_stability > 60 && desired_mode != field->projection_mode) {
        field->projection_mode = desired_mode;
        field->projection_mix = 0;
    }
    field->projection_mix = fminf(1, field->projection_mix + .018f);
    float curvature_target = fminf(.82f, fmaxf(.06f,
        field->ema_coherence * .58f + field->variance_energy * .28f - field->ema_noise * .22f));
    field->curvature += (curvature_target - field->curvature) * .035f;
    float target_x = field->rgb[0] - field->rgb[1], target_y = field->rgb[2] - field->rgb[1];
    float target_length = hypotf(target_x, target_y);
    if (target_length > .001f) { target_x /= target_length; target_y /= target_length; }
    field->direction_x += (target_x - field->direction_x) * .025f;
    field->direction_y += (target_y - field->direction_y) * .025f;
    float direction_length = fmaxf(.001f, hypotf(field->direction_x, field->direction_y));
    float ray_x = field->direction_x / direction_length, ray_y = field->direction_y / direction_length;
    float eye_x = margin_get_float(&field->permastore[MARGIN_CAMERA_BASE + 1], 0);
    float eye_y = margin_get_float(&field->permastore[MARGIN_CAMERA_BASE + 2], 0);
    float eye_z = margin_get_float(&field->permastore[MARGIN_CAMERA_BASE + 3], -1.35f);
    float camera_yaw = margin_get_float(&field->permastore[MARGIN_CAMERA_BASE + 4], 0);
    float camera_fov = margin_get_float(&field->permastore[MARGIN_CAMERA_BASE + 6], .58f);
    eye_x += (ray_x * .12f - eye_x) * .012f;
    eye_y += (ray_y * .08f - eye_y) * .012f;
    eye_z += ((-1.55f + field->ema_coherence * .35f) - eye_z) * .01f;
    camera_yaw += (atan2f(ray_x, fmaxf(.001f, ray_y)) * .18f - camera_yaw) * .012f;
    camera_fov += ((.48f + field->variance_energy * .42f) - camera_fov) * .012f;
    margin_set_float(&field->permastore[MARGIN_CAMERA_BASE + 1], eye_x, 4);
    margin_set_float(&field->permastore[MARGIN_CAMERA_BASE + 2], eye_y, 4);
    margin_set_float(&field->permastore[MARGIN_CAMERA_BASE + 3], eye_z, 4);
    margin_set_float(&field->permastore[MARGIN_CAMERA_BASE + 4], camera_yaw, 3);
    margin_set_float(&field->permastore[MARGIN_CAMERA_BASE + 6], camera_fov, 3);
    margin_set_float(&field->permastore[MARGIN_CAMERA_BASE + 7], (float)field->projection_mode, 3);
    /* AC graph3d-compatible column-major perspective matrix, resident in margin cells 48..63. */
    float perspective[16] = {0};
    float f = 1.0f / tanf(camera_fov * .5f), near = .01f, far = 10.0f, nf = 1.0f / (near - far);
    perspective[0] = perspective[5] = f; perspective[10] = (far + near) * nf;
    perspective[11] = -1; perspective[14] = 2 * far * near * nf;
    for (int element = 0; element < 16; element++)
        margin_set_float(&field->permastore[MARGIN_CAMERA_BASE + 8 + element], perspective[element], 5);
    float warp_x[MARGIN_WARP_GRID * MARGIN_WARP_GRID] = {0};
    float warp_y[MARGIN_WARP_GRID * MARGIN_WARP_GRID] = {0};
    float warp_depth[VOLUME_DEPTH] = {0};
    float phase = fmodf(field->lifetime_steps * .0065f, 6.2831853f);
    float energy = fminf(1, fmaxf(0, field->ema_actual * 8 + field->variance_energy * .35f));
    for (int gy = 0; gy < MARGIN_WARP_GRID; gy++) for (int gx = 0; gx < MARGIN_WARP_GRID; gx++) {
        MarginEval input = {.x = gx / 8.0f - 1, .y = gy / 8.0f - 1, .depth = .5f,
            .time = phase, .energy = energy};
        int at = gy * MARGIN_WARP_GRID + gx;
        margin_eval_body(field, MARGIN_PROJECTION_BODY_BASE, 0x90, &input, &warp_x[at]);
        margin_eval_body(field, MARGIN_PROJECTION_BODY_BASE + MARGIN_BODY_CELLS, 0x90, &input, &warp_y[at]);
    }
    for (int depth = 0; depth < VOLUME_DEPTH; depth++) {
        MarginEval input = {.depth = depth / (float)(VOLUME_DEPTH - 1), .time = phase, .energy = energy};
        margin_eval_body(field, MARGIN_PROJECTION_BODY_BASE + MARGIN_BODY_CELLS * 2, 0x90, &input, &warp_depth[depth]);
    }
    for (int y = 0; y < field->resolution; y++) for (int x = 0; x < field->resolution; x++) {
        float red = 0, green = 0, blue = 0, transmission = 1, previous_density = 0;
        float zx = (x + .5f) / (field->resolution * .5f) - 1, zy = (y + .5f) / (field->resolution * .5f) - 1;
        float grid_x = x * (MARGIN_WARP_GRID - 1) / (float)(field->resolution - 1);
        float grid_y = y * (MARGIN_WARP_GRID - 1) / (float)(field->resolution - 1);
        int gx = (int)grid_x, gy = (int)grid_y;
        int gx1 = gx + 1 < MARGIN_WARP_GRID ? gx + 1 : gx;
        int gy1 = gy + 1 < MARGIN_WARP_GRID ? gy + 1 : gy;
        float fx = grid_x - gx, fy = grid_y - gy;
        int g00 = gy * MARGIN_WARP_GRID + gx, g10 = gy * MARGIN_WARP_GRID + gx1;
        int g01 = gy1 * MARGIN_WARP_GRID + gx, g11 = gy1 * MARGIN_WARP_GRID + gx1;
        float wx = (warp_x[g00] * (1 - fx) + warp_x[g10] * fx) * (1 - fy) +
            (warp_x[g01] * (1 - fx) + warp_x[g11] * fx) * fy;
        float wy = (warp_y[g00] * (1 - fx) + warp_y[g10] * fx) * (1 - fy) +
            (warp_y[g01] * (1 - fx) + warp_y[g11] * fx) * fy;
        zx += wx * .085f; zy += wy * .085f;
        if (field->projection_mode == 0) {
            float radius = hypotf(zx, zy);
            if (radius >= .985f) { zx *= .985f / radius; zy *= .985f / radius; }
        }
        for (int depth = 0; depth < VOLUME_DEPTH; depth++) {
            int slice = (field->volume_head - depth + VOLUME_DEPTH) % VOLUME_DEPTH;
            float mx, my;
            if (field->projection_mode == 0) {
                float magnitude = tanhf(field->curvature * depth / (VOLUME_DEPTH * .9f));
                float ax = ray_x * magnitude, ay = ray_y * magnitude;
                /* Möbius translation (z-a)/(1-conj(a)z): a Poincaré-disk isometry. */
                float nr = zx - ax, ni = zy - ay;
                float dr = 1 - ax * zx - ay * zy, di = -(ax * zy - ay * zx);
                float denominator = fmaxf(.0001f, dr * dr + di * di);
                mx = (nr * dr + ni * di) / denominator;
                my = (ni * dr - nr * di) / denominator;
            } else if (field->projection_mode == 1) {
                /* Perspective camera rays through the temporal volume cube. */
                float distance = 1.25f + depth * .22f;
                float px = (zx - eye_x) * camera_fov * distance, py = (zy - eye_y) * camera_fov * distance;
                float pz = eye_z + distance;
                mx = px * cosf(camera_yaw) - pz * sinf(camera_yaw);
                my = py + pz * .08f;
            } else if (field->projection_mode == 2) {
                /* Rectilinear chamber: nested boxes recede toward a vanishing point. */
                float scale = .34f + depth * .105f;
                mx = zx * scale + ray_x * (VOLUME_DEPTH - 1 - depth) * .018f;
                my = zy * scale + ray_y * (VOLUME_DEPTH - 1 - depth) * .018f;
            } else {
                /* Oblique height-field/parallax stack, preserving straight horizons. */
                mx = zx + ray_x * depth * .035f;
                my = zy * .72f + .34f - depth * .092f;
            }
            mx += ray_x * warp_depth[depth] * .028f;
            my += ray_y * warp_depth[depth] * .028f;
            if (mx < -1 || mx > 1 || my < -1 || my > 1) continue;
            int sx = (int)lroundf((mx * .5f + .5f) * (field->resolution - 1));
            int sy = (int)lroundf((my * .5f + .5f) * (field->resolution - 1));
            int at = live_index(field, sx, sy, 0);
            float r = field->volume[slice][at] / 255.0f;
            float g = field->volume[slice][at + 1] / 255.0f;
            float b = field->volume[slice][at + 2] / 255.0f;
            float density = (r + g + b) / 3.0f;
            float surface = fabsf(density - previous_density);
            float weight = transmission * (.025f + density * .12f + surface * .45f);
            float luminance = r * .30f + g * .59f + b * .11f;
            red += weight * (luminance * .60f + r * .40f);
            green += weight * (luminance * .68f + g * .32f);
            blue += weight * (luminance * .72f + b * .28f);
            transmission *= 1.0f - density * .14f;
            previous_density = density;
        }
        float shade = .72f + .28f * fmaxf(0, ray_x * .6f - ray_y * .4f);
        int out = (y * field->resolution + x) * 3;
        uint8_t target[3] = {byte_value(red * 255 * shade), byte_value(green * 255 * shade), byte_value(blue * 255 * shade)};
        const uint8_t *previous = field->projection[field->projection_index] + out;
        output[out] = byte_value(previous[0] * (1 - field->projection_mix) + target[0] * field->projection_mix);
        output[out + 1] = byte_value(previous[1] * (1 - field->projection_mix) + target[1] * field->projection_mix);
        output[out + 2] = byte_value(previous[2] * (1 - field->projection_mix) + target[2] * field->projection_mix);
    }
    field->projection_index = publish;
}

static void live_console(LiveField *field, const char *message) {
    snprintf(field->console_lines[field->console_head], sizeof(field->console_lines[0]), "%s", message);
    field->console_head = (field->console_head + 1) % 4;
}

static void apply_seed_mode(LiveField *field, int mode) {
    int side = field->resolution;
    for (int y = 0; y < side; y++) for (int x = 0; x < side; x++) for (int channel = 0; channel < 3; channel++) {
        int source_x = x, source_y = y, source_channel = channel;
        if (mode == 1) { source_x = side - 1 - x; source_y = y; }
        else if (mode == 2) { source_x = y; source_y = side - 1 - x; }
        else if (mode == 3) source_channel = (channel + 1) % 3;
        else if (mode == 4) { source_x = (x + scale_extent(field, 31)) % side; source_y = (y + scale_extent(field, 47)) % side; }
        else if (mode == 5) { source_x = (x ^ (side / 2 - 1)) % side; source_y = (y ^ (side / 4 - 1)) % side; }
        uint8_t value = field->seed[live_index(field, source_x, source_y, source_channel)];
        if (mode == 1) value = 255 - value;
        if (mode == 4) value = value > 127 ? 224 : 24;
        if (!field->sanctuary[y * side + x]) field->pixels[live_index(field, x, y, channel)] = value;
    }
    for (int depth = 0; depth < VOLUME_DEPTH; depth++)
        memcpy(field->volume[depth], field->pixels, (size_t)field->byte_count);
    field->volume_head = 0; field->frames = 0; field->dead_steps = 0;
    field->previous_hash = field->two_back_hash = 0;
    field->still_steps = field->flicker_steps = 0; field->life_state = 0;
    char event[48]; snprintf(event, sizeof(event), ">REPROBE SEED %d", mode);
    live_console(field, event);
}

static void reset_live_memory(LiveField *field, const char *event) {
    for (int depth = 0; depth < VOLUME_DEPTH; depth++)
        memcpy(field->volume[depth], field->pixels, (size_t)field->byte_count);
    field->volume_head = 0; field->frames = 0; field->dead_steps = 0;
    field->previous_hash = field->two_back_hash = 0;
    field->still_steps = field->flicker_steps = 0; field->life_state = 0;
    field->ema_actual = field->ema_noise = field->ema_coherence = field->ema_muddiness = 0;
    field->healthy_steps = 0;
    live_console(field, event);
}

static void organized_box_memory(LiveField *field) {
    uint32_t seed = hash_text(field->id) ^ (uint32_t)(field->probe_epoch++ * 0x9e3779b9u);
    int side = field->resolution;
    for (int y = 0; y < side; y++) for (int x = 0; x < side; x++) {
        int cx = x * RASTER_W / side, cy = y * RASTER_H / side;
        int depth = 0;
        for (int inset = 8; inset <= 44; inset += 12)
            if (cx >= inset && cx < RASTER_W - inset && cy >= inset && cy < RASTER_H - inset) depth++;
        int membrane = 0;
        for (int inset = 8; inset <= 44; inset += 12)
            if ((cx == inset || cx == RASTER_W - 1 - inset) && cy >= inset && cy < RASTER_H - inset) membrane = 1;
            else if ((cy == inset || cy == RASTER_H - 1 - inset) && cx >= inset && cx < RASTER_W - inset) membrane = 1;
        int cell = ((cx >> 4) + (cy >> 4) * 5 + depth * 7 + (seed >> 8)) & 15;
        int wave = (int)(28 * sin((cx + depth * 13) * .11) + 24 * cos((cy - depth * 9) * .09));
        int at = live_index(field, x, y, 0);
        if (!field->sanctuary[y * side + x]) {
            field->pixels[at] = byte_value(24 + depth * 43 + cell * 5 + wave + (membrane ? 70 : 0));
            field->pixels[at + 1] = byte_value(36 + (4 - depth) * 34 + cell * 3 - wave / 2 + (membrane ? 34 : 0));
            field->pixels[at + 2] = byte_value(52 + depth * 27 + ((cx + cy) >> 3) * 4 + wave / 3 + (membrane ? 90 : 0));
        }
    }
    reset_live_memory(field, ">PROD ORGANIZED BOXES");
}

static void copy_neighbor_memory(LiveField *target, const LiveField *donor) {
    uint32_t dice = hash_text(target->id) ^ hash_text(donor->id) ^ (uint32_t)(target->probe_epoch++ * 2654435761u);
    int shift_x = 3 + (dice & 15), shift_y = 5 + ((dice >> 4) & 15);
    int rotation = (dice >> 9) % 3;
    for (int y = 0; y < target->resolution; y++) for (int x = 0; x < target->resolution; x++) for (int channel = 0; channel < 3; channel++) {
        int donor_x = x * donor->resolution / target->resolution;
        int donor_y = y * donor->resolution / target->resolution;
        int source = live_index(donor, donor_x + scale_offset(donor, shift_x), donor_y + scale_offset(donor, shift_y), (channel + rotation) % 3);
        int value = donor->pixels[source];
        int coherent_mutation = ((((x * RASTER_W / target->resolution) >> 4) ^ ((y * RASTER_H / target->resolution) >> 4) ^ (dice >> 16)) & 3) * 8 - 8;
        if (!target->sanctuary[y * target->resolution + x])
            target->pixels[live_index(target, x, y, channel)] = byte_value(value + coherent_mutation);
    }
    char event[48]; snprintf(event, sizeof(event), ">PROD NEIGHBOR %.12s", donor->id);
    reset_live_memory(target, event);
}

enum GrooveLifecycleOp {
    GL_RETURN = 0, GL_RESET_PC, GL_EXECUTE_SEQUENCE, GL_SYNTHESIZE, GL_ADVANCE_FRINGE,
    GL_INCREMENT_PROBE, GL_SELECT_MODE, GL_APPLY_SEED_MODE, GL_RESET_LIFE,
    GL_APPLY_ORGANIZED, GL_APPLY_GRAFT, GL_INCREMENT_FAILED, GL_CALL_VECTOR, GL_ZERO_RUNTIME,
};

static void run_lifecycle_vector_depth(LiveField *field, int vector, const LiveField *donor, int depth) {
    if (!field || vector < 0 || vector >= 8 || depth > 3) return;
    int mode = field->probe_epoch % 6;
    groove_set_reader(field, groove_u16(field->groove, GROOVE_STATE_BASE * 3 + 1), vector);
    for (int index = 0; index < GROOVE_VECTOR_CELLS; index++) {
        int at = (GROOVE_LIFECYCLE_BASE + vector * GROOVE_VECTOR_CELLS + index) * 3;
        int operation = field->groove[at], argument = field->groove[at + 1];
        if (operation == GL_RETURN) return;
        if (operation == GL_RESET_PC) groove_set_reader(field, 0, vector);
        else if (operation == GL_SYNTHESIZE) margin_maybe_synthesize(field);
        else if (operation == GL_ADVANCE_FRINGE) advance_margin_fringe(field);
        else if (operation == GL_INCREMENT_PROBE) {
            field->probe_epoch++;
            groove_set_u32(field->groove, GROOVE_STATE_BASE * 3 + 4, (uint32_t)field->probe_epoch);
        } else if (operation == GL_SELECT_MODE) mode = field->probe_epoch % 6;
        else if (operation == GL_APPLY_SEED_MODE) apply_seed_mode(field, mode);
        else if (operation == GL_RESET_LIFE) reset_live_memory(field, ">GROOVE RESET LIFE");
        else if (operation == GL_APPLY_ORGANIZED) organized_box_memory(field);
        else if (operation == GL_APPLY_GRAFT && donor) copy_neighbor_memory(field, donor);
        else if (operation == GL_INCREMENT_FAILED) {
            field->failed_reprobes++;
            groove_set_u32(field->groove, GROOVE_STATE_BASE * 3 + 8, (uint32_t)field->failed_reprobes);
        } else if (operation == GL_CALL_VECTOR) run_lifecycle_vector_depth(field, argument, donor, depth + 1);
        else if (operation == GL_ZERO_RUNTIME) {
            memset(field->pixels, 0, (size_t)field->byte_count);
            reset_live_memory(field, ">GROOVE ZERO RUNTIME");
        }
    }
}

static void run_lifecycle_vector(LiveField *field, int vector, const LiveField *donor) {
    run_lifecycle_vector_depth(field, vector, donor, 0);
}

static void reprobe_live_field(LiveField *field) {
    run_lifecycle_vector(field, 2, NULL);
}

static void queue_prod(const char *id, const char *strategy, const char *donor) {
    pthread_mutex_lock(&action_lock);
    for (int i = 0; i < MAX_PROGRAMS; i++) if (!pending_actions[i].occupied) {
        snprintf(pending_actions[i].id, sizeof(pending_actions[i].id), "%s", id);
        snprintf(pending_actions[i].strategy, sizeof(pending_actions[i].strategy), "%s", strategy);
        snprintf(pending_actions[i].donor, sizeof(pending_actions[i].donor), "%s", donor);
        pending_actions[i].occupied = true;
        break;
    }
    pthread_mutex_unlock(&action_lock);
}

static void apply_pending_prods(void) {
    ProdAction actions[MAX_PROGRAMS] = {0};
    pthread_mutex_lock(&action_lock);
    memcpy(actions, pending_actions, sizeof(actions));
    memset(pending_actions, 0, sizeof(pending_actions));
    pthread_mutex_unlock(&action_lock);
    for (int action = 0; action < MAX_PROGRAMS; action++) if (actions[action].occupied) {
        LiveField *target = NULL, *donor = NULL;
        for (int field = 0; field < MAX_PROGRAMS; field++) if (live_fields[field].occupied) {
            if (!strcmp(live_fields[field].id, actions[action].id)) target = &live_fields[field];
            if (!strcmp(live_fields[field].id, actions[action].donor)) donor = &live_fields[field];
        }
        if (!target) continue;
        if (!strcmp(actions[action].strategy, "copy") && donor) run_lifecycle_vector(target, 4, donor);
        else if (!strcmp(actions[action].strategy, "organized")) run_lifecycle_vector(target, 3, NULL);
        else run_lifecycle_vector(target, 2, NULL);
    }
}

static void live_paint(LiveField *field, uint8_t *pixels, int x, int y, int r, int g, int b) {
    if (x < 0 || x >= field->resolution || y < 0 || y >= field->resolution) return;
    int at = live_index(field, x, y, 0); pixels[at] = byte_value(r); pixels[at + 1] = byte_value(g); pixels[at + 2] = byte_value(b);
}

static void live_line(LiveField *field, uint8_t *pixels, int x0, int y0, int x1, int y1, int r, int g, int b) {
    int dx = abs(x1 - x0), sx = x0 < x1 ? 1 : -1;
    int dy = -abs(y1 - y0), sy = y0 < y1 ? 1 : -1, error = dx + dy;
    for (;;) {
        live_paint(field, pixels, x0, y0, r, g, b); if (x0 == x1 && y0 == y1) break;
        int twice = error * 2;
        if (twice >= dy) { error += dy; x0 += sx; }
        if (twice <= dx) { error += dx; y0 += sy; }
    }
}

static void margin_copy_sprite(LiveField *field, const uint8_t *pixels, int x0, int y0, int width, int height, int slot) {
    if (slot < 0 || slot >= MARGIN_SPRITE_SLOTS || width < 1 || width > MARGIN_SPRITE_SIZE || height < 1 || height > MARGIN_SPRITE_SIZE) return;
    int base = GROOVE_SPRITE_BASE + slot * (1 + MARGIN_SPRITE_SIZE * MARGIN_SPRITE_SIZE);
    memset(field->groove + base * 3, 0, (size_t)(1 + MARGIN_SPRITE_SIZE * MARGIN_SPRITE_SIZE) * 3);
    field->groove[base * 3] = 0x60; field->groove[base * 3 + 1] = (uint8_t)width; field->groove[base * 3 + 2] = (uint8_t)height;
    for (int y = 0; y < height; y++) for (int x = 0; x < width; x++) {
        int destination = (base + 1 + y * MARGIN_SPRITE_SIZE + x) * 3;
        for (int channel = 0; channel < 3; channel++)
            field->groove[destination + channel] = pixels[live_index(field, x0 + x, y0 + y, channel)];
    }
}

static void margin_paste_sprite(LiveField *field, const uint8_t *current, uint8_t *next, int slot, int x0, int y0, int mode) {
    if (slot < 0 || slot >= MARGIN_SPRITE_SLOTS) return;
    int base = GROOVE_SPRITE_BASE + slot * (1 + MARGIN_SPRITE_SIZE * MARGIN_SPRITE_SIZE);
    if (field->groove[base * 3] != 0x60) return;
    int width = field->groove[base * 3 + 1], height = field->groove[base * 3 + 2];
    for (int y = 0; y < height; y++) for (int x = 0; x < width; x++) {
        if (x0 + x < 0 || x0 + x >= field->resolution || y0 + y < 0 || y0 + y >= field->resolution) continue;
        for (int channel = 0; channel < 3; channel++) {
            int at = live_index(field, x0 + x, y0 + y, channel);
            int source = field->groove[(base + 1 + y * MARGIN_SPRITE_SIZE + x) * 3 + channel];
            int destination = current[at], value = source;
            if (mode == 1) value = destination ^ source;
            else if (mode == 2) value = destination + source;
            else if (mode == 3) value = source > 127 ? source : destination;
            next[at] = byte_value(value);
        }
    }
}

static void advance_margin_fringe(LiveField *field) {
    int side = field->resolution;
    int phase = (int)(field->lifetime_steps % (side * 4));
    int x, y;
    if (phase < side) { x = phase; y = 0; }
    else if (phase < side * 2) { x = side - 1; y = phase - side; }
    else if (phase < side * 3) { x = side * 3 - 1 - phase; y = side - 1; }
    else { x = 0; y = side * 4 - 1 - phase; }
    int at = live_index(field, x, y, 0);
    uint32_t edge = ((uint32_t)field->pixels[at] << 16) | ((uint32_t)field->pixels[at + 1] << 8) | field->pixels[at + 2];
    int fringe_cells = PERMA_CELLS - GROOVE_FRINGE_BASE;
    int destination = GROOVE_FRINGE_BASE + field->lifetime_steps % fringe_cells;
    int previous = destination == GROOVE_FRINGE_BASE ? PERMA_CELLS - 1 : destination - 1;
    int previous_at = previous * 3, destination_at = destination * 3;
    uint32_t previous_word = ((uint32_t)field->groove[previous_at] << 16) |
        ((uint32_t)field->groove[previous_at + 1] << 8) | field->groove[previous_at + 2];
    uint32_t outside = (uint32_t)live_authority_utc_ms ^ (uint32_t)(live_authority_utc_ms >> 32) ^ hash_text(field->id);
    uint32_t word = (previous_word << 7) ^ (previous_word >> 3) ^ edge ^
        (uint32_t)field->lifetime_steps ^ outside;
    field->groove[destination_at] = (uint8_t)(word >> 16);
    field->groove[destination_at + 1] = (uint8_t)(word >> 8);
    field->groove[destination_at + 2] = (uint8_t)word;
}

static bool execute_piece_vm_field(LiveField *field) {
    if (!field->piece_vm) return false;
    double beats = live_authority_utc_ms / 60000.0 * fmax(1.0, live_musical_bpm);
    double beat_phase = beats - floor(beats);
    double bar_phase = fmod(beats, 4.0) / 4.0;
    int fringe = GROOVE_FRINGE_BASE * 3 + field->lifetime_steps % ((PERMA_CELLS - GROOVE_FRINGE_BASE) * 3);
    uint8_t senses[8] = {
        byte_value((float)(beat_phase * 255)), byte_value((float)(bar_phase * 255)),
        byte_value(field->health * 255), byte_value(field->actual_energy * 255),
        byte_value(field->potential_energy * 255), byte_value(field->spatial_energy * 255),
        byte_value(field->temporal_coherence * 255), field->groove[fringe],
    };
    int fuel_used = 0;
    if (!piece_vm_native_set_senses(field->piece_vm, senses, 8) || !piece_vm_native_step(field->piece_vm, &fuel_used)) {
        field->health = 0; field->life_state = 2; field->published_health = 0;
        return true;
    }
    const uint8_t *frame = piece_vm_native_front(field->piece_vm);
    measure_life(field, field->pixels, frame);
    memcpy(field->pixels, frame, (size_t)field->byte_count);
    field->frames++; field->current_opcode = 20;
    field->volume_head = (field->volume_head + 1) % VOLUME_DEPTH;
    memcpy(field->volume[field->volume_head], frame, (size_t)field->byte_count);
    pthread_mutex_lock(&live_lock);
    int publish = 1 - field->display_index;
    memcpy(field->display[publish], frame, (size_t)field->byte_count);
    memcpy(field->projection[publish], frame, (size_t)field->byte_count);
    field->display_index = publish; field->projection_index = publish;
    field->published_life_state = field->life_state;
    field->published_actual_energy = field->actual_energy;
    field->published_potential_energy = field->potential_energy;
    field->published_spatial_energy = field->spatial_energy;
    field->published_variance_energy = field->variance_energy;
    field->published_noise_energy = field->noise_energy;
    field->published_temporal_coherence = field->temporal_coherence;
    field->published_muddiness = field->muddiness;
    field->published_health = field->health;
    field->published_opcode = field->current_opcode;
    pthread_mutex_unlock(&live_lock);
    return true;
}

static void execute_live_field(LiveField *field) {
    if (execute_piece_vm_field(field)) return;
    static const char *orders[] = {"rgb", "rbg", "grb", "gbr", "brg", "bgr"};
    uint8_t *current = field->pixels, *next = field->scratch;
    for (int index = 0; index < field->bytecode_count / BYTECODE_STRIDE; index++) {
        groove_set_reader(field, index, 1);
        uint8_t instruction[BYTECODE_STRIDE];
        if (!margin_load_instruction(field, index, instruction)) {
            live_console(field, ">MARGIN VERIFY / QUARANTINE");
            continue;
        }
        int opcode = instruction[0];
        field->current_opcode = opcode;
        int a = bytecode_arg(instruction, 0), b = bytecode_arg(instruction, 1), c = bytecode_arg(instruction, 2);
        bool margin_primitive = opcode == 1 || opcode == 2 || opcode == 5 || opcode == 11 || opcode == 12;
        int primitive_body = margin_primitive ? (int)field->permastore[MARGIN_FUNCTION_BASE + opcode - 1].link : 0;
        if (margin_primitive && (!(field->permastore[MARGIN_FUNCTION_BASE + opcode - 1].flags & 4) ||
            !margin_verify_body(field, primitive_body, 0x90))) {
            live_console(field, ">BODY VERIFY / QUARANTINE");
            continue;
        }
        if (opcode >= 13 && opcode <= 18) {
            memcpy(next, current, (size_t)field->byte_count);
            if (opcode == 13) live_line(field, next,
                scale_coordinate(field, a), scale_coordinate(field, b), scale_coordinate(field, c), scale_coordinate(field, bytecode_arg(instruction, 3)),
                bytecode_arg(instruction, 4), bytecode_arg(instruction, 5), bytecode_arg(instruction, 6));
            else if (opcode == 14) {
                int x0 = scale_coordinate(field, a), y0 = scale_coordinate(field, b);
                int x1 = scale_coordinate(field, bytecode_arg(instruction, 2)), y1 = scale_coordinate(field, bytecode_arg(instruction, 3));
                int x2 = scale_coordinate(field, bytecode_arg(instruction, 4)), y2 = scale_coordinate(field, bytecode_arg(instruction, 5));
                int r = bytecode_arg(instruction, 6), g = bytecode_arg(instruction, 7), bl = bytecode_arg(instruction, 8);
                live_line(field, next, x0, y0, x1, y1, r, g, bl); live_line(field, next, x1, y1, x2, y2, r, g, bl); live_line(field, next, x2, y2, x0, y0, r, g, bl);
            } else if (opcode == 15) {
                int start_x = scale_coordinate(field, a), start_y = scale_coordinate(field, b);
                int tolerance = c, r = bytecode_arg(instruction, 3), g = bytecode_arg(instruction, 4), bl = bytecode_arg(instruction, 5);
                memset(field->visited, 0, (size_t)field->pixel_count);
                int start = start_y * field->resolution + start_x, target_at = live_index(field, start_x, start_y, 0), head = 0, tail = 0;
                int target[3] = {current[target_at], current[target_at + 1], current[target_at + 2]};
                field->queue[tail++] = (uint32_t)start; field->visited[start] = 1;
                while (head < tail) {
                    int pixel = (int)field->queue[head++], x = pixel % field->resolution, y = pixel / field->resolution, at = live_index(field, x, y, 0);
                    int delta = abs(current[at] - target[0]);
                    if (abs(current[at + 1] - target[1]) > delta) delta = abs(current[at + 1] - target[1]);
                    if (abs(current[at + 2] - target[2]) > delta) delta = abs(current[at + 2] - target[2]);
                    if (delta > tolerance) continue;
                    live_paint(field, next, x, y, r, g, bl);
                    const int nx[4] = {x - 1, x + 1, x, x}, ny[4] = {y, y, y - 1, y + 1};
                    for (int n = 0; n < 4; n++) if (nx[n] >= 0 && nx[n] < field->resolution && ny[n] >= 0 && ny[n] < field->resolution) {
                        int key = ny[n] * field->resolution + nx[n]; if (!field->visited[key]) { field->visited[key] = 1; field->queue[tail++] = (uint32_t)key; }
                    }
                }
            } else if (opcode == 16) {
                int bx = scale_coordinate(field, a), by = scale_coordinate(field, b);
                int width = scale_extent(field, c), height = scale_extent(field, bytecode_arg(instruction, 3));
                if (bx + width > field->resolution) width = field->resolution - bx;
                if (by + height > field->resolution) height = field->resolution - by;
                int permeability = bytecode_arg(instruction, 4), rule = bytecode_arg(instruction, 5);
                float amount = permeability / 255.0f;
                for (int y = by; y < by + height; y++) for (int x = bx; x < bx + width; x++) for (int channel = 0; channel < 3; channel++) {
                    bool boundary = x == bx || x == bx + width - 1 || y == by || y == by + height - 1;
                    int left_x = x - 1 < bx ? bx : x - 1, right_x = x + 1 >= bx + width ? bx + width - 1 : x + 1;
                    int up_y = y - 1 < by ? by : y - 1, down_y = y + 1 >= by + height ? by + height - 1 : y + 1;
                    float value;
                    if (boundary) {
                        int ox = x == bx ? x - 1 : x == bx + width - 1 ? x + 1 : x;
                        int oy = y == by ? y - 1 : y == by + height - 1 ? y + 1 : y;
                        value = current[live_index(field, x, y, channel)] * (1 - amount) + current[live_index(field, ox, oy, channel)] * amount;
                    } else if (rule == 0) {
                        value = (current[live_index(field, left_x, y, channel)] + current[live_index(field, right_x, y, channel)] +
                                 current[live_index(field, x, up_y, channel)] + current[live_index(field, x, down_y, channel)]) / 4.0f;
                    } else if (rule == 1) {
                        value = current[live_index(field, left_x, y, channel)] ^ current[live_index(field, right_x, y, channel)] ^ current[live_index(field, x, up_y, channel)];
                    } else if (rule == 2) {
                        int local_x = x - bx, local_y = y - by;
                        int source_x = bx + local_y * width / height;
                        int source_y = by + height - 1 - local_x * height / width;
                        value = current[live_index(field, source_x, source_y, channel)];
                    } else {
                        int neighborhood = current[live_index(field, left_x, y, channel)] + current[live_index(field, right_x, y, channel)] +
                                           current[live_index(field, x, up_y, channel)] + current[live_index(field, x, down_y, channel)];
                        value = neighborhood / 4 >= current[live_index(field, x, y, channel)] ? 224 : 24;
                    }
                    next[live_index(field, x, y, channel)] = byte_value(value);
                }
            } else if (opcode == 17) {
                int width = scale_extent(field, c), height = scale_extent(field, bytecode_arg(instruction, 3));
                if (width > MARGIN_SPRITE_SIZE) width = MARGIN_SPRITE_SIZE;
                if (height > MARGIN_SPRITE_SIZE) height = MARGIN_SPRITE_SIZE;
                margin_copy_sprite(field, current, scale_coordinate(field, a), scale_coordinate(field, b), width, height, bytecode_arg(instruction, 4));
            } else margin_paste_sprite(field, current, next, a, scale_coordinate(field, b), scale_coordinate(field, c), bytecode_arg(instruction, 3));
        } else for (int y = 0; y < field->resolution; y++) for (int x = 0; x < field->resolution; x++) for (int channel = 0; channel < 3; channel++) {
            int at = live_index(field, x, y, channel); float value = current[at];
            if (opcode == 19) {
                int neighbors = 0, sum = 0;
                for (int oy = -1; oy <= 1; oy++) for (int ox = -1; ox <= 1; ox++) {
                    if (ox == 0 && oy == 0) continue;
                    int neighbor = live_index(field, x + ox, y + oy, 0);
                    int brightness = (current[neighbor] + current[neighbor + 1] + current[neighbor + 2]) / 3;
                    if (brightness >= 128) neighbors++;
                    sum += current[live_index(field, x + ox, y + oy, channel)];
                }
                int self = (current[live_index(field, x, y, 0)] + current[live_index(field, x, y, 1)] + current[live_index(field, x, y, 2)]) / 3;
                int mask = self >= 128 ? b : a;
                value = mask & (1 << neighbors) ? fminf(255, sum / 8.0f + 24) : current[at] * .18f;
            } else if (margin_primitive) {
                MarginEval input = {.self = current[at], .arg = opcode == 5 ? a : channel == 0 ? a : channel == 1 ? b : c,
                    .x = x * 2.0f / (field->resolution - 1) - 1, .y = y * 2.0f / (field->resolution - 1) - 1,
                    .time = field->lifetime_steps * .0065f, .energy = field->ema_actual};
                if (!margin_eval_body(field, primitive_body, 0x90, &input, &value)) value = current[at];
            } else if (opcode == 3) value = current[live_index(field, x - scale_offset(field, a), y - scale_offset(field, b), channel)];
            else if (opcode == 4) { float amount = c / 255.0f; value = current[at] * (1 - amount) + current[live_index(field, x + scale_offset(field, a), y + scale_offset(field, b), channel)] * amount; }
            else if (opcode == 5) value = current[at] >= a ? 255 - current[at] : current[at];
            else if (opcode == 6) { value = 0; for (int oy = -1; oy <= 1; oy++) for (int ox = -1; ox <= 1; ox++) value += current[live_index(field, x + ox, y + oy, channel)] / 9.0f; }
            else if (opcode == 7) { int original = current[at]; value = abs(original - current[live_index(field, x + 1, y, channel)]) + abs(original - current[live_index(field, x, y + 1, channel)]); }
            else if (opcode == 8) value = current[live_index(field, y, field->resolution - 1 - x, channel)];
            else if (opcode == 9) value = current[live_index(field, a == 0 ? field->resolution - 1 - x : x, a == 1 ? field->resolution - 1 - y : y, channel)];
            else if (opcode == 10 && a >= 0 && a < 6) { const char *order = orders[a]; int input_channel = order[channel] == 'r' ? 0 : order[channel] == 'g' ? 1 : 2; value = current[live_index(field, x, y, input_channel)]; }
            next[at] = byte_value(value);
        }
        if (opcode != 16) for (int pixel = 0; pixel < field->pixel_count; pixel++) if (field->sanctuary[pixel])
            memcpy(next + pixel * 3, current + pixel * 3, 3);
        measure_life(field, current, next);
        if (field->lifetime_steps % 64 == 0) {
            char event[48];
            snprintf(event, sizeof(event), ">%s HP%02d D%.2f N%.2f", opcode_name(opcode),
                     (int)lroundf(field->health * 100), field->actual_energy, field->noise_energy);
            live_console(field, event);
        }
        uint8_t *swap = current; current = next; next = swap;
    }
    if (current != field->pixels) memcpy(field->pixels, current, (size_t)field->byte_count);
    int state_at = GROOVE_STATE_BASE * 3;
    groove_set_u32(field->groove, state_at + 12, groove_u32(field->groove, state_at + 12) + 1);
    groove_set_reader(field, 0, 1);
    field->frames++;
    field->dead_steps = field->life_state == 0 ? 0 : field->dead_steps + 1;
    if (field->dead_steps >= 6) run_lifecycle_vector(field, 5, NULL);
    else if (field->frames >= 300) reprobe_live_field(field);
    field->volume_head = (field->volume_head + 1) % VOLUME_DEPTH;
    memcpy(field->volume[field->volume_head], field->pixels, (size_t)field->byte_count);
    margin_maybe_synthesize(field);
    project_live_volume(field);
    pthread_mutex_lock(&live_lock);
    advance_margin_fringe(field);
    int publish = 1 - field->display_index;
    memcpy(field->display[publish], field->pixels, (size_t)field->byte_count);
    field->display_index = publish;
    field->published_life_state = field->life_state;
    field->published_actual_energy = field->actual_energy;
    field->published_potential_energy = field->potential_energy;
    field->published_spatial_energy = field->spatial_energy;
    field->published_variance_energy = field->variance_energy;
    field->published_noise_energy = field->noise_energy;
    field->published_temporal_coherence = field->temporal_coherence;
    field->published_muddiness = field->muddiness;
    field->published_health = field->health;
    field->published_projection_mode = field->projection_mode;
    field->published_projection_generation = field->projection_generation;
    field->published_opcode = field->current_opcode;
    memcpy(field->published_console, field->console_lines, sizeof(field->published_console));
    field->published_console_head = field->console_head;
    pthread_mutex_unlock(&live_lock);
}

static void advance_live_fields(const FarmState *state, uint64_t now) {
    if (!live_step_at) live_step_at = now;
    pthread_mutex_lock(&live_lock);
    for (int slot = 0; slot < MAX_PROGRAMS; slot++) if (live_fields[slot].occupied) {
        bool present = false;
        for (int i = 0; i < state->program_count; i++)
            if (!strcmp(live_fields[slot].id, state->programs[i].id)) { present = true; break; }
        if (!present) {
            piece_vm_native_destroy(live_fields[slot].piece_vm);
            live_fields[slot].piece_vm = NULL;
            live_fields[slot].occupied = false;
        }
    }
    pthread_mutex_unlock(&live_lock);
    int steps = 0;
    while (now - live_step_at >= 1000 / VM_SEQUENCE_HZ && steps < 2) {
        int channels = 0, alive = 0, dormant = 0, collapsed = 0, flicker = 0;
        uint64_t writes = 0;
        live_authority_utc_ms = state->utc_ms + (now > state->received_at ? now - state->received_at : 0);
        live_musical_bpm = state->musical_bpm;
        for (int i = 0; i < state->program_count; i++) {
            LiveField *field = live_field(&state->programs[i]);
            if (!field) continue;
            execute_live_field(field);
            channels++;
            writes += (uint64_t)(field->bytecode_count / BYTECODE_STRIDE) *
                (uint64_t)field->byte_count * VM_SEQUENCE_HZ;
            if (field->life_state == 0) alive++;
            else if (field->life_state == 1) dormant++;
            else if (field->life_state == 2) collapsed++;
            else flicker++;
        }
        pthread_mutex_lock(&live_lock);
        live_channels = channels; live_writes_per_second = writes;
        alive_channels = alive; dormant_channels = dormant; collapsed_channels = collapsed;
        flicker_channels = flicker;
        pthread_mutex_unlock(&live_lock);
        live_step_at += 1000 / VM_SEQUENCE_HZ;
        steps++;
    }
    if (now - live_step_at > 1000) live_step_at = now;
}

static void *run_live_runtime(void *unused) {
    (void)unused;
    uint64_t next_tick = SDL_GetTicksNS();
    const uint64_t interval = 1000000000ull / VM_SEQUENCE_HZ;
    while (running) {
        FarmState state;
        pthread_mutex_lock(&state_lock); state = shared_state; pthread_mutex_unlock(&state_lock);
        uint64_t now = SDL_GetTicks();
        if (state.connected) {
            apply_pending_prods();
            advance_live_fields(&state, now);
            update_sonic_field(&state, now);
        }
        next_tick += interval;
        uint64_t now_ns = SDL_GetTicksNS();
        if (next_tick > now_ns) SDL_DelayPrecise(next_tick - now_ns);
        else { vm_deadline_misses++; next_tick = now_ns; }
    }
    return NULL;
}

static bool raster_replay_self_test(void) {
    memset(live_fields, 0, sizeof(live_fields));
    Program program = {.raster_count = RASTER_BYTES, .raster_width = RASTER_W,
                       .raster_height = RASTER_H, .bytecode_count = BYTECODE_STRIDE * 4};
    snprintf(program.id, sizeof(program.id), "raster-replay-test");
    for (int y = 0; y < RASTER_H; y++) for (int x = 0; x < RASTER_W; x++) for (int c = 0; c < 3; c++)
        program.raster[field_index(x, y, c)] = (uint8_t)(x + y + c);
    program.bytecode[0] = 17;                 /* copy 0 0 4 4 -> sprite slot 0 */
    program.bytecode[5] = 4;
    program.bytecode[7] = 4;
    program.bytecode[BYTECODE_STRIDE] = 3;    /* shift 1 0 */
    program.bytecode[BYTECODE_STRIDE + 1] = 1;
    program.bytecode[BYTECODE_STRIDE * 2] = 18; /* paste slot 0 at 20 20 */
    program.bytecode[BYTECODE_STRIDE * 2 + 3] = 20;
    program.bytecode[BYTECODE_STRIDE * 2 + 5] = 20;
    program.bytecode[BYTECODE_STRIDE * 3] = 1; /* margin-resident add body */
    program.bytecode[BYTECODE_STRIDE * 3 + 1] = 1;
    program.bytecode[BYTECODE_STRIDE * 3 + 3] = 2;
    program.bytecode[BYTECODE_STRIDE * 3 + 5] = 3;
    LiveField *field = live_field(&program);
    uint8_t first_instruction[BYTECODE_STRIDE] = {0};
    bool margin_valid = field && margin_load_instruction(field, 0, first_instruction) &&
        first_instruction[0] == 17 && field->permastore[MARGIN_CAMERA_BASE].tag == 0x40 &&
        field->permastore[MARGIN_ALGEBRA_BASE].tag == 0x50 &&
        (field->permastore[MARGIN_FUNCTION_BASE].flags & 4) &&
        margin_verify_body(field, MARGIN_RASTER_BODY_BASE, 0x90) &&
        margin_verify_body(field, MARGIN_PROJECTION_BODY_BASE, 0x90);
    margin_synthesize_projection(field, 0);
    margin_synthesize_projection(field, 1);
    margin_synthesize_projection(field, 2);
    margin_valid = margin_valid && field->projection_generation > 0;
    execute_live_field(field);
    bool valid = margin_valid && field->groove[GROOVE_SPRITE_BASE * 3] == 0x60 &&
                 field->pixels[field_index(10, 10, 1)] == program.raster[field_index(9, 10, 1)] + 2 &&
                 field->pixels[field_index(20, 20, 2)] == program.raster[field_index(0, 0, 2)] + 3;
    Program pressed = program;
    memcpy(pressed.groove, field->groove, GROOVE_BYTES); pressed.groove_count = GROOVE_BYTES;
    memset(live_fields, 0, sizeof(live_fields));
    LiveField *reloaded = live_field(&pressed);
    uint8_t reloaded_instruction[BYTECODE_STRIDE] = {0};
    valid = valid && reloaded && groove_valid(reloaded->groove, GROOVE_BYTES) &&
        margin_load_instruction(reloaded, 3, reloaded_instruction) && reloaded_instruction[0] == 1 &&
        (reloaded->permastore[MARGIN_FUNCTION_BASE].flags & 4) &&
        margin_verify_body(reloaded, MARGIN_RASTER_BODY_BASE, 0x90);
    memset(live_fields, 0, sizeof(live_fields));
    return valid;
}

static bool mixed_resolution_self_test(void) {
    Program seed = {.raster_count = RASTER_BYTES, .raster_width = RASTER_W,
                    .raster_height = RASTER_H, .bytecode_count = BYTECODE_STRIDE};
    snprintf(seed.id, sizeof(seed.id), "mixed-profile-seed");
    for (int y = 0; y < RASTER_H; y++) for (int x = 0; x < RASTER_W; x++) {
        seed.raster[field_index(x, y, 0)] = (uint8_t)x;
        seed.raster[field_index(x, y, 1)] = (uint8_t)y;
        seed.raster[field_index(x, y, 2)] = (uint8_t)(x ^ y);
    }
    seed.bytecode[0] = 3; /* canonical shift 1 0 */
    seed.bytecode[1] = 1;
    memset(live_fields, 0, sizeof(live_fields));
    LiveField *pressed = live_field(&seed);
    if (!pressed) return false;
    uint8_t base_groove[GROOVE_BYTES];
    memcpy(base_groove, pressed->groove, GROOVE_BYTES);

    static const int sides[] = {32, 64, 128, 256};
    bool valid = true;
    for (int profile = 0; profile < 4 && valid; profile++) {
        Program program = seed;
        snprintf(program.id, sizeof(program.id), "mixed-profile-%d", sides[profile]);
        memcpy(program.groove, base_groove, GROOVE_BYTES);
        program.groove_count = GROOVE_BYTES;
        groove_set_u16(program.groove, 104, (uint16_t)sides[profile]);
        groove_set_u32(program.groove, 108, (uint32_t)(sides[profile] * sides[profile] * 3));
        program.groove[112] = (uint8_t)(profile + 1);
        groove_set_u32(program.groove, 12, groove_protected_hash(program.groove));
        memset(live_fields, 0, sizeof(live_fields));
        LiveField *field = live_field(&program);
        valid = field && field->resolution == sides[profile] &&
            field->pixel_count == sides[profile] * sides[profile] &&
            field->byte_count == sides[profile] * sides[profile] * 3;
        if (!valid) break;
        int x = sides[profile] / 2, y = sides[profile] / 2;
        int offset = scale_offset(field, 1);
        uint8_t expected[3] = {
            field->seed[live_index(field, x - offset, y, 0)],
            field->seed[live_index(field, x - offset, y, 1)],
            field->seed[live_index(field, x - offset, y, 2)],
        };
        execute_live_field(field);
        valid = field->pixels[live_index(field, x, y, 0)] == expected[0] &&
            field->pixels[live_index(field, x, y, 1)] == expected[1] &&
            field->pixels[live_index(field, x, y, 2)] == expected[2] &&
            groove_valid(field->groove, GROOVE_BYTES);
    }
    memset(live_fields, 0, sizeof(live_fields));
    return valid;
}

static int request(const char *method, const char *path, const char *payload,
                   char *response, size_t capacity) {
    if (capacity) response[0] = '\0';
    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) return -1;
    /* Authority work can briefly overlap a Git edition or nursery admission.
       This runs on the polling thread, never the renderer, so let a loopback
       response finish rather than misreporting a valid health envelope. */
    struct timeval timeout = {.tv_sec = 3, .tv_usec = 0};
    setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout));
    struct sockaddr_in address = {.sin_family = AF_INET,
        .sin_port = htons((uint16_t)server_port)};
    inet_pton(AF_INET, "127.0.0.1", &address.sin_addr);
    if (connect(fd, (struct sockaddr *)&address, sizeof(address)) < 0) {
        close(fd); return -1;
    }
    char header[1024];
    size_t payload_len = payload ? strlen(payload) : 0;
    int header_len = snprintf(header, sizeof(header),
        "%s %s HTTP/1.1\r\nHost: 127.0.0.1:%d\r\nConnection: close\r\n"
        "%sContent-Length: %zu\r\n\r\n",
        method, path, server_port,
        payload ? "Content-Type: application/json\r\n" : "",
        payload_len);
    if (header_len < 0 || (size_t)header_len >= sizeof(header) ||
        send(fd, header, (size_t)header_len, 0) != header_len) { close(fd); return -1; }
    size_t sent = 0;
    while (sent < payload_len) {
        ssize_t count = send(fd, payload + sent, payload_len - sent, 0);
        if (count <= 0) { close(fd); return -1; }
        sent += (size_t)count;
    }
    size_t used = 0;
    while (used + 1 < capacity) {
        ssize_t n = recv(fd, response + used, capacity - used - 1, 0);
        if (n <= 0) break;
        used += (size_t)n;
    }
    close(fd); response[used] = '\0';
    char *body = strstr(response, "\r\n\r\n");
    if (!body || strncmp(response, "HTTP/1.1 200", 12)) return -1;
    body += 4;
    memmove(response, body, strlen(body) + 1);
    return (int)strlen(response);
}

static void report_health(uint64_t now) {
    char payload[16384], response[4096];
    size_t used = (size_t)snprintf(payload, sizeof(payload), "{\"displayFps\":%.2f,\"residents\":[", display_fps);
    int count = 0;
    pthread_mutex_lock(&live_lock);
    for (int i = 0; i < MAX_PROGRAMS && used + 768 < sizeof(payload); i++) if (live_fields[i].occupied) {
        LiveField *field = &live_fields[i];
        int state = GROOVE_STATE_BASE * 3;
        used += (size_t)snprintf(payload + used, sizeof(payload) - used,
            "%s{\"id\":\"%s\",\"hp\":%.2f,\"ageMs\":%llu,\"life\":%d,\"failedReprobes\":%d,"
            "\"pc\":%u,\"needlePixel\":%u,\"sequencePasses\":%u,\"lifecycleVector\":%u,\"resolution\":%u,"
            "\"actual\":%.6f,\"variance\":%.6f,\"spatial\":%.6f,\"noise\":%.6f,\"coherence\":%.6f,\"muddiness\":%.6f,"
            "\"vmHz\":%d,\"deadlineMisses\":%llu,\"pieceVmId\":\"%s\",\"pieceVmRole\":\"%s\","
            "\"pieceVmProbeCarrier\":%s,\"sonicVoices\":%d}",
            count++ ? "," : "", field->id,
            fminf(100, fmaxf(0, field->published_health * 100)),
            (unsigned long long)(now >= field->born_at ? now - field->born_at : 0),
            field->published_life_state, field->failed_reprobes,
            groove_u16(field->groove, state + 1), groove_u32(field->groove, state + 20),
            groove_u32(field->groove, state + 12), field->groove[state + 3], field->resolution,
            field->published_actual_energy, field->published_variance_energy, field->published_spatial_energy,
            field->published_noise_energy, field->published_temporal_coherence, field->published_muddiness, VM_READER_HZ,
            (unsigned long long)vm_deadline_misses, field->piece_vm_source_id, field->piece_vm_role,
            field->piece_vm_probe_carrier ? "true" : "false", field->sonic_voices);
    }
    pthread_mutex_unlock(&live_lock);
    snprintf(payload + used, sizeof(payload) - used, "]}");
    if (!count) return;
    if (request("POST", "/api/health", payload, response, sizeof(response)) <= 0) {
        health_report_failures++;
        if (health_report_failures == 1 || health_report_failures % 30 == 0) {
            const char *message = strstr(response, "\r\n\r\n");
            fprintf(stderr, "health report %s (%llu, %d residents, %zu bytes): %.240s\n",
                    response[0] ? "rejected" : "unavailable",
                    (unsigned long long)health_report_failures, count, strlen(payload),
                    message ? message + 4 : response);
        }
        return;
    }
    health_report_successes++;
    char *save_line = NULL;
    for (char *line = strtok_r(response, "\n", &save_line); line; line = strtok_r(NULL, "\n", &save_line)) {
        char *fields[4] = {0}; int field_count = split_tabs(line, fields, 4);
        if (field_count == 4 && !strcmp(fields[0], "PROD")) queue_prod(fields[1], fields[2], fields[3]);
    }
}

// Persist one mutable record per report. A full twelve-tile board therefore
// completes a rotation in twelve seconds without making the 240 Hz reader wait.
static void report_groove(void) {
    static int cursor = 0;
    char id[32] = {0};
    uint8_t groove[GROOVE_BYTES];
    bool found = false;
    pthread_mutex_lock(&live_lock);
    for (int step = 0; step < MAX_PROGRAMS; step++) {
        int slot = (cursor + step) % MAX_PROGRAMS;
        if (!live_fields[slot].occupied || !groove_valid(live_fields[slot].groove, GROOVE_BYTES)) continue;
        snprintf(id, sizeof(id), "%s", live_fields[slot].id);
        memcpy(groove, live_fields[slot].groove, GROOVE_BYTES);
        cursor = (slot + 1) % MAX_PROGRAMS;
        found = true;
        break;
    }
    pthread_mutex_unlock(&live_lock);
    if (!found) return;

    static const char hex[] = "0123456789abcdef";
    size_t payload_size = GROOVE_BYTES * 2 + 64;
    char *payload = malloc(payload_size), response[512];
    if (!payload) return;
    int prefix = snprintf(payload, payload_size, "{\"id\":\"%s\",\"groove\":\"", id);
    if (prefix < 0 || (size_t)prefix + GROOVE_BYTES * 2 + 3 >= payload_size) { free(payload); return; }
    size_t at = (size_t)prefix;
    for (size_t i = 0; i < GROOVE_BYTES; i++) {
        payload[at++] = hex[groove[i] >> 4]; payload[at++] = hex[groove[i] & 15];
    }
    payload[at++] = '"'; payload[at++] = '}'; payload[at] = '\0';
    request("POST", "/api/groove-state", payload, response, sizeof(response));
    free(payload);
}

static void *poll_state(void *unused) {
    (void)unused;
    char *response = malloc(RESPONSE_CAP);
    uint64_t last_health_report = 0;
    while (running) {
        if (request("GET", "/api/native", NULL, response, RESPONSE_CAP) > 0) {
            FarmState next;
            if (parse_state(response, &next)) {
                stabilize_program_addresses(&next);
                pthread_mutex_lock(&state_lock);
                shared_state = next;
                pthread_mutex_unlock(&state_lock);
            }
        } else {
            pthread_mutex_lock(&state_lock);
            /* A single 250 ms poll miss must not flash a full-screen disconnect
               warning over otherwise valid telemetry. Keep the last coherent
               snapshot through a short authority grace window. */
            uint64_t stale_for = SDL_GetTicks() > shared_state.received_at
                ? SDL_GetTicks() - shared_state.received_at : 0;
            if (!shared_state.received_at || stale_for >= 3000) shared_state.connected = false;
            pthread_mutex_unlock(&state_lock);
        }
        uint64_t now = SDL_GetTicks();
        if (now - last_health_report >= 1000) {
            report_health(now);
            report_groove();
            last_health_report = now;
        }
        SDL_Delay(250);
    }
    free(response);
    return NULL;
}

static void select_program(const char *id) {
    char payload[96], response[2048];
    snprintf(payload, sizeof(payload), "{\"id\":\"%s\"}", id);
    request("POST", "/api/select", payload, response, sizeof(response));
}

static void move_selection(int *selected_index, int delta) {
    FarmState copy;
    pthread_mutex_lock(&state_lock); copy = shared_state; pthread_mutex_unlock(&state_lock);
    if (!copy.program_count) return;
    *selected_index = (*selected_index + delta + copy.program_count) % copy.program_count;
    select_program(copy.programs[*selected_index].id);
}

static int shifted_probe_address(int address, int delta) {
    const int visible = TILE_PERIMETER * 2;
    int index;
    if (address >= 0 && address < TILE_PERIMETER) index = address;
    else if (address >= GROOVE_FRINGE_BASE && address < GROOVE_FRINGE_BASE + TILE_PERIMETER)
        index = TILE_PERIMETER + address - GROOVE_FRINGE_BASE;
    else index = GROOVE_SEQUENCE_BASE;
    index = (index + delta) % visible;
    if (index < 0) index += visible;
    return index < TILE_PERIMETER ? index : GROOVE_FRINGE_BASE + index - TILE_PERIMETER;
}

static void move_margin_probe(int selected_index, int delta, const char *requested_by) {
    FarmState copy;
    pthread_mutex_lock(&state_lock); copy = shared_state; pthread_mutex_unlock(&state_lock);
    if (!copy.program_count) return;
    const Program *program = NULL;
    for (int i = 0; i < copy.program_count; i++)
        if (!strcmp(copy.programs[i].id, copy.selected)) program = &copy.programs[i];
    if (!program) {
        if (selected_index < 0 || selected_index >= copy.program_count) selected_index = 0;
        program = &copy.programs[selected_index];
    }
    int current = !strcmp(copy.margin_probe_id, program->id)
        ? copy.margin_probe_address : GROOVE_SEQUENCE_BASE;
    int address = shifted_probe_address(current, delta);
    char payload[192], response[2048];
    snprintf(payload, sizeof(payload), "{\"id\":\"%s\",\"address\":%d,\"requestedBy\":\"%s\"}",
             program->id, address, requested_by);
    request("POST", "/api/margin-probe", payload, response, sizeof(response));
}

static bool shifted_probe_self_test(void) {
    return shifted_probe_address(64, 1) == 65 && shifted_probe_address(0, -1) == GROOVE_FRINGE_BASE + TILE_PERIMETER - 1 &&
        shifted_probe_address(TILE_PERIMETER - 1, 1) == GROOVE_FRINGE_BASE &&
        shifted_probe_address(GROOVE_FRINGE_BASE, -1) == TILE_PERIMETER - 1;
}

static const char *groove_track_name(int address) {
    if (address < 0 || address >= PERMA_CELLS) return "OUTSIDE";
    if (address < GROOVE_SEQUENCE_BASE) return "HEADER";
    if (address < GROOVE_FUNCTION_BASE) return "SEQUENCE";
    if (address < GROOVE_BODY_BASE) return "FUNCTION";
    if (address < GROOVE_PROJECTION_BASE) return "BODY";
    if (address < GROOVE_LIFECYCLE_BASE) return "PROJECTION";
    if (address < GROOVE_STATE_BASE) return "LIFECYCLE";
    if (address < GROOVE_SPRITE_BASE) return "STATE";
    if (address < GROOVE_PROPOSAL_BASE) return "SPRITE";
    if (address < GROOVE_SOURCE_BASE) return "PROPOSAL";
    if (address < GROOVE_FRINGE_BASE) return "SOURCE";
    return "FRINGE";
}

static bool groove_address_protected(int address) {
    return (address >= GROOVE_HEADER_BASE && address < GROOVE_STATE_BASE) ||
        (address >= GROOVE_SOURCE_BASE && address < GROOVE_FRINGE_BASE);
}

static void groove_rim_position(int address, char *output, size_t capacity) {
    if (address < 0 || address >= TILE_PERIMETER) {
        snprintf(output, capacity, "OFF-RIM"); return;
    }
    if (address < TILE_W) snprintf(output, capacity, "TOP+%03d", address);
    else if (address < TILE_W + TILE_H - 1)
        snprintf(output, capacity, "RIGHT+%03d", address - TILE_W + 1);
    else if (address < TILE_W * 2 + TILE_H - 2)
        snprintf(output, capacity, "BOTTOM+%03d", address - (TILE_W + TILE_H - 1));
    else snprintf(output, capacity, "LEFT+%03d", address - (TILE_W * 2 + TILE_H - 2) + 1);
}

static bool groove_microscope_self_test(void) {
    char position[24];
    groove_rim_position(64, position, sizeof(position));
    return !strcmp(groove_track_name(0), "HEADER") &&
        !strcmp(groove_track_name(64), "SEQUENCE") &&
        !strcmp(groove_track_name(128), "FUNCTION") &&
        !strcmp(groove_track_name(192), "BODY") &&
        !strcmp(groove_track_name(480), "PROJECTION") &&
        !strcmp(groove_track_name(528), "LIFECYCLE") &&
        !strcmp(groove_track_name(656), "STATE") &&
        !strcmp(groove_track_name(720), "SPRITE") &&
        !strcmp(groove_track_name(4820), "PROPOSAL") &&
        !strcmp(groove_track_name(5108), "SOURCE") &&
        !strcmp(groove_track_name(5300), "FRINGE") &&
        groove_address_protected(64) && !groove_address_protected(656) &&
        groove_address_protected(5108) && !groove_address_protected(5300) &&
        !strcmp(position, "TOP+064");
}

static int groove_opcode_at(const LiveField *field, int address) {
    if (!field || address < 0 || address >= PERMA_CELLS) return 0;
    if (address >= GROOVE_SEQUENCE_BASE && address < GROOVE_FUNCTION_BASE) {
        int instruction = (address - GROOVE_SEQUENCE_BASE) / GROOVE_INSTRUCTION_PIXELS;
        if (instruction >= groove_instruction_count(field)) return 0;
        return field->groove[(GROOVE_SEQUENCE_BASE + instruction * GROOVE_INSTRUCTION_PIXELS) * 3];
    }
    if (address >= GROOVE_FUNCTION_BASE && address < GROOVE_BODY_BASE) {
        int opcode = (address - GROOVE_FUNCTION_BASE) / 3 + 1;
        return opcode <= MAX_RASTER_OPCODE ? opcode : 0;
    }
    if (address >= GROOVE_BODY_BASE && address < GROOVE_PROJECTION_BASE) {
        int opcode = (address - GROOVE_BODY_BASE) / MARGIN_BODY_CELLS + 1;
        return opcode <= MARGIN_RASTER_BODY_SLOTS ? opcode : 0;
    }
    return 0;
}

static void render_board(Canvas *canvas, const FarmState *s, uint64_t ticks,
                         bool inspect_mode, int selected_index) {
    fill(canvas, 0, 0, LOGICAL_W, LOGICAL_H, BLACK);
    const Program *active = NULL;
    for (int i = 0; i < s->program_count; i++)
        if (s->margin_probe_id[0] && !strcmp(s->programs[i].id, s->margin_probe_id)) active = &s->programs[i];
    for (int i = 0; i < s->program_count; i++)
        if (!active && !strcmp(s->programs[i].id, s->selected)) active = &s->programs[i];
    if (!active && s->program_count) active = &s->programs[0];
    float actual = 0, potential = 0, spatial = 0, variance = 0, noise = 0, coherence = 0, muddiness = 0;
    int life = 0, projection_mode = 0, projection_generation = 0;
    int alive = 0, dormant = 0, collapsed = 0, flicker = 0;
    uint64_t writes_per_second = 0;
    int margin_address = -1, margin_pc = 0, margin_vector = 0, margin_opcode = 0;
    uint8_t margin_rgb[3] = {0};
    bool margin_is_probe = false;
    pthread_mutex_lock(&live_lock);
    LiveField *active_field = find_live_field(active);
    if (active_field) {
        actual = active_field->published_actual_energy;
        potential = active_field->published_potential_energy;
        spatial = active_field->published_spatial_energy;
        variance = active_field->published_variance_energy;
        noise = active_field->published_noise_energy;
        coherence = active_field->published_temporal_coherence;
        muddiness = active_field->published_muddiness;
        life = active_field->published_life_state;
        projection_mode = active_field->published_projection_mode;
        projection_generation = active_field->published_projection_generation;
        int state = GROOVE_STATE_BASE * 3;
        margin_pc = groove_u16(active_field->groove, state + 1);
        margin_vector = active_field->groove[state + 3];
        margin_address = (int)groove_u32(active_field->groove, state + 20);
        margin_opcode = active_field->published_opcode;
        if (margin_address >= 0 && margin_address < PERMA_CELLS) {
            int at = margin_address * 3;
            margin_rgb[0] = active_field->groove[at];
            margin_rgb[1] = active_field->groove[at + 1];
            margin_rgb[2] = active_field->groove[at + 2];
        }
        if (active && !strcmp(s->margin_probe_id, active->id) &&
            s->margin_probe_address >= 0 && s->margin_probe_address < PERMA_CELLS) {
            margin_is_probe = true;
            margin_address = s->margin_probe_address;
            int at = margin_address * 3;
            margin_rgb[0] = active_field->groove[at];
            margin_rgb[1] = active_field->groove[at + 1];
            margin_rgb[2] = active_field->groove[at + 2];
            int probed_opcode = groove_opcode_at(active_field, margin_address);
            if (probed_opcode) margin_opcode = probed_opcode;
        }
    }
    alive = alive_channels; dormant = dormant_channels;
    collapsed = collapsed_channels; flicker = flicker_channels;
    writes_per_second = live_writes_per_second;
    pthread_mutex_unlock(&live_lock);
    fill(canvas, 6, 5, 628, 28, (Color){4, 8, 13, 220});
    draw_text(canvas, "PIECEFARM", 10, 8, 4, INK);
    char clock_label[64];
    snprintf(clock_label, sizeof(clock_label), "%s %.0fBPM%s", s->clock_synced ? "AC UTC" : "UTC SYNC", s->musical_bpm,
             audio_online ? "" : " MUTE");
    draw_text(canvas, clock_label, 455, 12, 2, audio_online ? MINT : PINK);
    char policy_bonus = !strcmp(s->piece_vm_policy_bonus, "phenotype-lead") ? 'P' :
        !strcmp(s->piece_vm_policy_bonus, "champion-control") ? 'C' :
        !strcmp(s->piece_vm_policy_bonus, "branch-diversity") ? 'D' : '-';
    char operator_bonus = !strcmp(s->piece_vm_operator_bonus, "variation") ? 'V' :
        !strcmp(s->piece_vm_operator_bonus, "machinery") ? 'M' :
        !strcmp(s->piece_vm_operator_bonus, "exchange") ? 'X' : '-';
    char mission[192];
    snprintf(mission, sizeof(mission), "%s PIECEVM G%d %s / 64%s 128%s 256%s / UCB +%c OP +%c %.24s / CUR%s / GIT GROWN",
             inspect_mode ? "INSPECT 1:1 /" : "WALL /", s->piece_vm_generation,
             s->piece_vm_mutation[0] ? s->piece_vm_mutation : "foundation",
             s->piece_vm_half_verified ? "OK" : "--", s->piece_vm_standard_verified ? "OK" : "--",
             s->piece_vm_double_verified ? "OK" : "--", policy_bonus, operator_bonus,
             s->piece_vm_mutation_bonus[0] ? s->piece_vm_mutation_bonus : "explore",
             s->piece_vm_curriculum_lead ? "*" : "");
    draw_wrapped(canvas, mission, 10, 42, 620, 4, 3, GOLD);

    char line[512];
    const Program *inspected = s->program_count && selected_index >= 0 && selected_index < s->program_count
        ? &s->programs[selected_index] : active;
    snprintf(line, sizeof(line), "ITER %llu  A%llu R%llu  COV %d/%d  %s %.12s",
             (unsigned long long)s->iteration, (unsigned long long)s->piece_vm_accepted,
             (unsigned long long)s->piece_vm_rejected, s->coverage, s->capacity,
             inspect_mode ? "VIEW" : "PVM", inspect_mode && inspected ? inspected->id :
             (s->piece_vm_id[0] ? s->piece_vm_id : "AWAKENING"));
    fill(canvas, 7, 148, 626, 25, (Color){4, 8, 13, 235});
    draw_text(canvas, line, 11, 154, 2, INK);

    uint64_t elapsed = ticks > s->received_at ? ticks - s->received_at : 0;
    uint64_t remain_ms = s->checkpoint_ms > elapsed ? s->checkpoint_ms - elapsed : 0;
    unsigned seconds = (unsigned)((remain_ms + 999) / 1000);
    snprintf(line, sizeof(line), "GIT %02u:%02u:%02u", seconds / 3600,
             seconds / 60 % 60, seconds % 60);
    fill(canvas, 7, 179, 626, 55, (Color){9, 13, 21, 242});
    stroke(canvas, 7, 179, 626, 55, GOLD);
    draw_text(canvas, line, 14, 190, 5, GOLD);
    snprintf(line, sizeof(line), "ED%d %.8s @%llu", s->git_editions,
             s->git_head[0] ? s->git_head : "NOHEAD",
             (unsigned long long)s->git_iteration);
    draw_text(canvas, line, 428, 183, 1, MINT);
    if (s->piece_vm_selection_parent[0])
        snprintf(line, sizeof(line), "PHENO %.8s N%.2f B%+.3f R%d V%d %s CUR B%d/5 %.5s",
                 s->piece_vm_selection_parent, s->piece_vm_phenotype_score,
                 s->piece_vm_phenotype_bias, s->piece_vm_phenotype_reports,
                 s->piece_vm_phenotype_voices,
                 s->piece_vm_phenotype_ready ? "READY" : "GATHER",
                 s->piece_vm_development_breadth, s->piece_vm_development_signature);
    else snprintf(line, sizeof(line), "PVM %.12s Q%.3f R%d C%d M%d S%d F%d A%d L%d CUR B%d/5 %.5s",
                  s->piece_vm_id, s->piece_vm_score, s->piece_vm_registers,
                  s->piece_vm_calls, s->piece_vm_memory, s->piece_vm_senses,
                  s->piece_vm_functions, s->piece_vm_arguments, s->piece_vm_layouts,
                  s->piece_vm_development_breadth, s->piece_vm_development_signature);
    draw_text(canvas, line, 14, 218, 1, INK);
    if (s->piece_vm_environment_capability[0]) {
        snprintf(line, sizeof(line), "ECO %.8s<%.6s", s->piece_vm_environment_capability,
                 s->piece_vm_environment_donor);
        draw_text(canvas, line, 300, 218, 1, CYAN);
    }
    struct ProofLamp { int x; const char *label; bool valid; } lamps[3] = {
        {430, "64", s->piece_vm_half_verified}, {492, "128", s->piece_vm_standard_verified},
        {562, "256", s->piece_vm_double_verified},
    };
    for (int lamp = 0; lamp < 3; lamp++) {
        fill(canvas, lamps[lamp].x, 207, lamp ? 62 : 54, 22, lamps[lamp].valid ? (Color){18, 70, 50, 255} : (Color){55, 20, 28, 255});
        draw_text(canvas, lamps[lamp].label, lamps[lamp].x + 5, 211, 2, lamps[lamp].valid ? MINT : PINK);
    }
    if (s->piece_vm_policy_bonus[0]) {
        snprintf(line, sizeof(line), "U+%c P%d/%.2f C%d/%.2f D%d/%.2f | O+%c V%d/%.2f M%d/%.2f X%d/%.2f",
                 policy_bonus,
                 s->piece_vm_policy_trials[0], s->piece_vm_policy_reward[0],
                 s->piece_vm_policy_trials[1], s->piece_vm_policy_reward[1],
                 s->piece_vm_policy_trials[2], s->piece_vm_policy_reward[2], operator_bonus,
                 s->piece_vm_operator_trials[0], s->piece_vm_operator_reward[0],
                 s->piece_vm_operator_trials[1], s->piece_vm_operator_reward[1],
                 s->piece_vm_operator_trials[2], s->piece_vm_operator_reward[2]);
        draw_text(canvas, line, 10, 231, 1, CYAN);
    } else if (margin_address >= 0) {
        char rim[24]; groove_rim_position(margin_address, rim, sizeof(rim));
        snprintf(line, sizeof(line), "%s %s @%03d %s/%s %s RGB %02X.%02X.%02X PC%d V%d %s",
                 margin_is_probe ? "PROBE" : "MARGIN",
                 active && active->address[0] ? active->address : "--", margin_address,
                 rim, groove_track_name(margin_address),
                 groove_address_protected(margin_address) ? "LOCK" : "MUT",
                 margin_rgb[0], margin_rgb[1], margin_rgb[2], margin_pc, margin_vector,
                 opcode_name(margin_opcode));
        draw_text(canvas, line, 10, 231, 1, CYAN);
    }

    int piece_vm_frontier = 1;
    while (piece_vm_frontier <= s->piece_vm_generation && piece_vm_frontier < 128) piece_vm_frontier *= 2;
    char growth_bar_name[32], margin_bar_name[32], opcode_bar_name[32];
    int probe_voices = 0;
    char probe_source_id[16] = {0}, probe_address[4] = {0};
    pthread_mutex_lock(&sonic_field.lock);
    probe_voices = sonic_field.probe_voices;
    snprintf(probe_source_id, sizeof(probe_source_id), "%s", sonic_field.probe_source_id);
    snprintf(probe_address, sizeof(probe_address), "%s", sonic_field.probe_address);
    pthread_mutex_unlock(&sonic_field.lock);
    if (s->margin_probe_descendant_id[0])
        snprintf(growth_bar_name, sizeof(growth_bar_name), "GRAFT %.8s", s->margin_probe_descendant_id);
    else snprintf(growth_bar_name, sizeof(growth_bar_name), "PVM GROW");
    if (s->margin_probe_descendant_id[0]) {
        snprintf(margin_bar_name, sizeof(margin_bar_name), "PROP %.15s",
                 s->margin_probe_capability[0] ? s->margin_probe_capability : "CAP");
        snprintf(opcode_bar_name, sizeof(opcode_bar_name), "SONIC PATH");
    } else {
        snprintf(margin_bar_name, sizeof(margin_bar_name), "%s %s", margin_is_probe ? "PROBE" : "MARGIN",
                 active && active->address[0] ? active->address : "--");
        snprintf(opcode_bar_name, sizeof(opcode_bar_name), "%s %s",
                 margin_is_probe && s->margin_probe_capability[0] ? "CAP" : "OP",
                 margin_is_probe && s->margin_probe_capability[0]
                    ? s->margin_probe_capability : opcode_name(margin_opcode));
    }
    struct Bar { const char *name; float value; char display[96]; Color color; } bars[4] = {
        {growth_bar_name, s->margin_probe_status[0] ?
            (!strcmp(s->margin_probe_status, "admitted") ? 1 : s->margin_probe_attempts / 8.0f) :
            (piece_vm_frontier ? (float)s->piece_vm_generation / piece_vm_frontier : 0), "", PINK},
        {margin_bar_name, s->margin_probe_descendant_id[0]
            ? fminf(1, s->margin_probe_propagation_descendants / 8.0f)
            : margin_address >= 0 ? margin_address / (float)TILE_PERIMETER : 0, "", CYAN},
        {opcode_bar_name, s->margin_probe_descendant_id[0]
            ? probe_voices / (float)AUDIO_VOICES : margin_pc / 8.0f, "", GOLD},
        {"EVAL RATE", fminf(1, s->evaluations_per_second / 16), "", MINT},
    };
    if (s->margin_probe_descendant_id[0])
        snprintf(bars[0].display, sizeof(bars[0].display), "%s %s G%d", s->margin_probe_status,
                 s->margin_probe_descendant_state, s->margin_probe_generation);
    else snprintf(bars[0].display, sizeof(bars[0].display), "G%d L%d X%d", s->piece_vm_generation,
                  s->piece_vm_lineage, s->piece_vm_crossovers);
    if (s->margin_probe_descendant_id[0]) {
        snprintf(bars[1].display, sizeof(bars[1].display), "%d DESC %d LIVE F%d",
                 s->margin_probe_propagation_descendants, s->margin_probe_propagation_residents,
                 s->margin_probe_propagation_generation);
        if (probe_voices && probe_source_id[0])
            snprintf(bars[2].display, sizeof(bars[2].display), "%d/%d %s %.8s",
                     probe_voices, AUDIO_VOICES, probe_address, probe_source_id);
        else snprintf(bars[2].display, sizeof(bars[2].display), "%d/%d VOICES CHAMP %s",
                      probe_voices, AUDIO_VOICES, s->margin_probe_champion_carrier ? "YES" : "NO");
    } else {
        snprintf(bars[1].display, sizeof(bars[1].display), "@%03d %s %s", margin_address,
                 groove_track_name(margin_address), groove_address_protected(margin_address) ? "LOCK" : "MUT");
        snprintf(bars[2].display, sizeof(bars[2].display), "PC%d V%d %02X.%02X.%02X", margin_pc, margin_vector,
                 margin_rgb[0], margin_rgb[1], margin_rgb[2]);
    }
    snprintf(bars[3].display, sizeof(bars[3].display), "%.1f/S", s->evaluations_per_second);
    for (int i = 0; i < 4; i++) {
        int x = 7 + i * 158;
        fill(canvas, x, 240, 151, 50, (Color){5, 14, 19, 235});
        stroke(canvas, x, 240, 151, 50, DIM);
        draw_text(canvas, bars[i].name, x + 5, 246, 2, MINT);
        draw_text(canvas, bars[i].display, x + 6, 260, 1, INK);
        fill(canvas, x + 5, 270, 141, 8, (Color){8, 17, 20, 255});
        fill(canvas, x + 5, 270, 141 * fminf(1, bars[i].value), 8, bars[i].color);
    }

    float spectrum[32];
    pthread_mutex_lock(&sonic_field.lock); memcpy(spectrum, sonic_field.spectrum, sizeof(spectrum)); pthread_mutex_unlock(&sonic_field.lock);
    fill(canvas, 7, 294, 626, 37, (Color){12, 9, 24, 255});
    for (int band = 0; band < 32; band++) {
        float height = 2 + fminf(1, spectrum[band]) * 31;
        Color color = band < 11 ? CYAN : band < 22 ? PINK : GOLD;
        fill(canvas, 9 + band * 19.4f, 329 - height, 14, height, color);
    }
    const char *tag = life == 2 ? "COLLAPSE" : life == 3 ? "FLICKER" :
        muddiness > .45f ? "MUDDY" : noise > .24f && coherence < .42f ? "CHAOS" : spatial > .30f ? "EDGE" :
        variance > .18f && coherence > .52f ? "VOLUME" : actual > .07f ? "PULSE" : "FLOW";
    static const char *spaces[] = {"CURVED", "RAY", "CHAMBER", "HEIGHT"};
    snprintf(line, sizeof(line), "MUSIGRAPH > %s %s V%d / A%.2f P%.2f N%.2f C%.2f",
             spaces[projection_mode >= 0 && projection_mode < 4 ? projection_mode : 0], tag,
             projection_generation, actual, potential, noise, coherence);
    fill(canvas, 9, 298, 480, 26, (Color){4, 8, 13, 210});
    draw_text(canvas, line, 12, 303, 2, GOLD);
    uint64_t authority_ms = s->utc_ms + (ticks > s->received_at ? ticks - s->received_at : 0);
    double beat = authority_ms / 60000.0 * fmax(1.0, s->musical_bpm);
    int beat_byte = (int)lround((beat - floor(beat)) * 255);
    snprintf(line, sizeof(line), "BEAT%03d AI%d", beat_byte, s->visual_reviews);
    fill(canvas, 490, 298, 141, 26, (Color){4, 8, 13, 210});
    draw_text(canvas, line, 496, 303, 2, MINT);
    time_t seconds_utc = (time_t)(authority_ms / 1000); struct tm utc = {0}; gmtime_r(&seconds_utc, &utc);
    snprintf(line, sizeof(line), "%.0fFPS %02d:%02dZ VM240 M%llu A%d D%d C%d F%d W%.1fM FIELD%dK",
             display_fps, utc.tm_hour, utc.tm_min, (unsigned long long)vm_deadline_misses,
             alive, dormant, collapsed, flicker, writes_per_second / 1000000.0,
             s->resident_bytes / 1024);
    draw_text(canvas, line, 10, 335, 2, INK);
    if (!s->connected) draw_text(canvas, "WAITING FOR PIECEFARM AUTHORITY", 120, 175, 3, PINK);
}

static const char *opcode_name(int opcode) {
    static const char *names[] = {"?", "ADD", "XOR", "SHIFT", "MIX", "SOLAR", "BLUR", "EDGE",
        "ROTATE", "MIRROR", "CHANNEL", "AND", "OR", "LINE", "TRI", "FLOOD", "BOX", "COPY", "PASTE", "CELL"};
    if (opcode == 20) return "PIECEVM";
    return opcode >= 1 && opcode <= MAX_RASTER_OPCODE ? names[opcode] : "?";
}

static int tile_perimeter_index(int px, int py) {
    if (px < 0 || px >= TILE_W || py < 0 || py >= TILE_H) return -1;
    if (py == 0) return px;
    if (px == TILE_W - 1) return TILE_W + py - 1;
    if (py == TILE_H - 1) return TILE_W + TILE_H - 1 + (TILE_W - 2 - px);
    if (px == 0) return TILE_W + TILE_H - 1 + TILE_W - 1 + (TILE_H - 2 - py);
    return -1;
}

static bool tile_perimeter_self_test(void) {
    bool seen[TILE_PERIMETER] = {0};
    int count = 0;
    for (int py = 0; py < TILE_H; py++) for (int px = 0; px < TILE_W; px++) {
        int index = tile_perimeter_index(px, py);
        if (index < 0) continue;
        if (index >= TILE_PERIMETER || seen[index]) return false;
        seen[index] = true; count++;
    }
    return count == TILE_PERIMETER;
}

static bool inspector_geometry(int side, int *x, int *y) {
    if (side < MIN_RASTER_SIDE || side > MAX_RASTER_SIDE) return false;
    *x = (LOGICAL_W - side) / 2;
    *y = (LOGICAL_H - side) / 2;
    return *x >= 0 && *y >= 0 && *x + side <= LOGICAL_W && *y + side <= LOGICAL_H;
}

static bool inspector_geometry_self_test(void) {
    static const int sides[] = {32, 64, 128, 256};
    for (size_t i = 0; i < sizeof(sides) / sizeof(sides[0]); i++) {
        int x = -1, y = -1;
        if (!inspector_geometry(sides[i], &x, &y)) return false;
        if (LOGICAL_W - x * 2 != sides[i] || LOGICAL_H - y * 2 != sides[i]) return false;
    }
    return true;
}

static void render_tile(Canvas *canvas, const Program *p, int slot) {
    const int cols = 4, rows = 3, tw = LOGICAL_W / cols, th = LOGICAL_H / rows;
    int x = slot % cols * tw, y = slot / cols * th;
    int health = 0;
    int current_opcode = 0;
    Color core_colors[TILE_PERIMETER] = {0}, fringe_colors[TILE_PERIMETER] = {0};
    bool has_margin = false;
    bool has_image = false;
    int pixel_side = RASTER_W;
    uint8_t pixels[MAX_RASTER_BYTES] = {0};
    if (p->raster_count == p->raster_width * p->raster_height * 3 && p->raster_count > 0) {
        pthread_mutex_lock(&live_lock);
        LiveField *field = find_live_field(p);
        if (field) {
            pixel_side = field->resolution;
            memcpy(pixels, field->projection[field->projection_index], (size_t)field->byte_count);
            health = (int)lroundf(fminf(1, fmaxf(0, field->published_health)) * 100);
            current_opcode = field->published_opcode;
            for (int sample = 0; sample < TILE_PERIMETER; sample++) {
                /* One rim pixel is one literal, stable groove address. The 556
                   core pixels expose cells 0..555 without the old decimation,
                   so headers, sequence, functions, bodies and projection data
                   have a directly inspectable screen position. */
                int core_pixel = sample;
                int fringe_pixel = GROOVE_FRINGE_BASE + sample;
                int core_at = core_pixel * 3, fringe_at = fringe_pixel * 3;
                core_colors[sample] = (Color){field->groove[core_at], field->groove[core_at + 1], field->groove[core_at + 2], 255};
                fringe_colors[sample] = (Color){field->groove[fringe_at], field->groove[fringe_at + 1], field->groove[fringe_at + 2], 255};
            }
            has_margin = true;
        }
        else { pixel_side = p->raster_width; memcpy(pixels, p->raster, (size_t)p->raster_count); }
        pthread_mutex_unlock(&live_lock);
        has_image = true;
    }
    fill(canvas, x, y, tw, th, BLACK);
    if (has_image) {
        /* Integer-ratio, square-pixel previews: 32²→96² (3x), 64²→64²
           (1x), 128²→64² (1/2), 256²→64² (1/4). No crop or aspect warp. */
        int image_side = pixel_side == 32 ? 96 : 64;
        int image_x = x + (tw - image_side) / 2;
        int image_y = y + (th - 16 - image_side) / 2;
        for (int py = 0; py < image_side; py++) for (int px = 0; px < image_side; px++) {
            int sx = px * pixel_side / image_side, sy = py * pixel_side / image_side;
            int at = (sy * pixel_side + sx) * 3;
            canvas->pixels[(image_y + py) * LOGICAL_W + image_x + px] =
                packed((Color){pixels[at], pixels[at + 1], pixels[at + 2], 255});
        }
    }
    if (has_margin) {
        /* WYSIWYG machine rim: every perimeter pixel is one stable address.
           Outer two logical pixels read mutable fringe bytes; inner two read
           protected/core bytes. No grouping, tint, cursor, or spectral rail. */
        const int rim = 2;
        for (int py = 0; py < th; py++) for (int px = 0; px < tw; px++) {
            int address = tile_perimeter_index(px, py);
            if (address < 0) continue;
            if (py == 0) {
                fill(canvas, x + px, y, 1, rim, fringe_colors[address]);
                fill(canvas, x + px, y + rim, 1, rim, core_colors[address]);
            } else if (py == th - 1) {
                fill(canvas, x + px, y + th - rim, 1, rim, fringe_colors[address]);
                fill(canvas, x + px, y + th - rim * 2, 1, rim, core_colors[address]);
            } else if (px == 0) {
                fill(canvas, x, y + py, rim, 1, fringe_colors[address]);
                fill(canvas, x + rim, y + py, rim, 1, core_colors[address]);
            } else {
                fill(canvas, x + tw - rim, y + py, rim, 1, fringe_colors[address]);
                fill(canvas, x + tw - rim * 2, y + py, rim, 1, core_colors[address]);
            }
        }
    }
    const int health_x = x + 5, health_y = y + th - 6, health_w = tw - 10, health_h = 3;
    fill(canvas, health_x, health_y, health_w, health_h, HEALTH_BLACK);
    int filled = health_w * health / 100;
    for (int pixel = 0; pixel < filled; pixel++)
        fill(canvas, health_x + pixel, health_y, 1, health_h, health_scale_color(pixel / (float)(health_w - 1)));
    draw_text_shadow(canvas, opcode_name(current_opcode), x + 5, y + th - 15, 1, INK);
}

static void render_inspector(Canvas *canvas, const Program *program) {
    int side = 0, image_x = 0, image_y = 0;
    uint8_t pixels[MAX_RASTER_BYTES] = {0};
    pthread_mutex_lock(&live_lock);
    LiveField *field = find_live_field(program);
    if (field) {
        side = field->resolution;
        memcpy(pixels, field->projection[field->projection_index], (size_t)field->byte_count);
    } else if (program && program->raster_count == program->raster_width * program->raster_height * 3) {
        side = program->raster_width;
        memcpy(pixels, program->raster, (size_t)program->raster_count);
    }
    pthread_mutex_unlock(&live_lock);
    if (!inspector_geometry(side, &image_x, &image_y)) return;
    for (int py = 0; py < side; py++) for (int px = 0; px < side; px++) {
        int at = (py * side + px) * 3;
        canvas->pixels[(image_y + py) * LOGICAL_W + image_x + px] =
            packed((Color){pixels[at], pixels[at + 1], pixels[at + 2], 255});
    }
}

static void render_soup(Canvas *canvas, const FarmState *s, uint64_t ticks,
                        bool inspect_mode, int selected_index) {
    (void)ticks;
    fill(canvas, 0, 0, LOGICAL_W, LOGICAL_H, BLACK);
    if (inspect_mode && s->program_count) {
        if (selected_index < 0 || selected_index >= s->program_count) selected_index = 0;
        render_inspector(canvas, &s->programs[selected_index]);
        return;
    }
    for (int slot = 0; slot < s->program_count && slot < 12; slot++) render_tile(canvas, &s->programs[slot], slot);
}

static bool make_panel(Panel *panel, SDL_Rect bounds, SDL_DisplayID display, const char *title) {
    panel->bounds = bounds;
    panel->display = display;
    panel->window = SDL_CreateWindow(title, bounds.w, bounds.h,
        SDL_WINDOW_BORDERLESS | SDL_WINDOW_ALWAYS_ON_TOP | SDL_WINDOW_HIDDEN);
    if (!panel->window) return false;
    SDL_SetWindowPosition(panel->window, SDL_WINDOWPOS_CENTERED_DISPLAY(display),
                          SDL_WINDOWPOS_CENTERED_DISPLAY(display));
    panel->renderer = SDL_CreateRenderer(panel->window, NULL);
    if (!panel->renderer) return false;
    panel->texture = SDL_CreateTexture(panel->renderer, SDL_PIXELFORMAT_RGBA8888,
                                       SDL_TEXTUREACCESS_STREAMING, LOGICAL_W, LOGICAL_H);
    if (!panel->texture) return false;
    panel->pixels = calloc(LOGICAL_W * LOGICAL_H, sizeof(uint32_t));
    if (!panel->pixels) return false;
    panel->canvas.pixels = panel->pixels;
    SDL_SetTextureScaleMode(panel->texture, SDL_SCALEMODE_NEAREST);
    SDL_SetTextureBlendMode(panel->texture, SDL_BLENDMODE_NONE);
    SDL_ShowWindow(panel->window);
    SDL_SetWindowFullscreen(panel->window, true);
    SDL_RaiseWindow(panel->window);
    return true;
}

static void ensure_panel(Panel *panel) {
    int width, height;
    SDL_GetWindowSize(panel->window, &width, &height);
    if ((SDL_GetWindowFlags(panel->window) & SDL_WINDOW_FULLSCREEN) == 0)
        SDL_SetWindowFullscreen(panel->window, true);
    (void)width; (void)height;
    SDL_RaiseWindow(panel->window);
}

static void present_panel(Panel *panel) {
    SDL_UpdateTexture(panel->texture, NULL, panel->pixels, LOGICAL_W * 4);
    SDL_SetRenderDrawColor(panel->renderer, BLACK.r, BLACK.g, BLACK.b, 255);
    SDL_RenderClear(panel->renderer);
    int width, height; SDL_GetRenderOutputSize(panel->renderer, &width, &height);
    int scale_x = width / LOGICAL_W, scale_y = height / LOGICAL_H;
    int scale = scale_x < scale_y ? scale_x : scale_y;
    if (scale < 1) scale = 1;
    int output_w = LOGICAL_W * scale, output_h = LOGICAL_H * scale;
    SDL_FRect dst = {(float)((width - output_w) / 2), (float)((height - output_h) / 2),
                     (float)output_w, (float)output_h};
    SDL_RenderTexture(panel->renderer, panel->texture, NULL, &dst);
    SDL_RenderPresent(panel->renderer);
}

static void present(Stage *stage, const FarmState *state, uint64_t ticks,
                    bool inspect_mode, int selected_index) {
    render_board(&stage->board.canvas, state, ticks, inspect_mode, selected_index);
    render_soup(&stage->soup.canvas, state, ticks, inspect_mode, selected_index);
    present_panel(&stage->board);
    present_panel(&stage->soup);
}

static void save_ppm(const Canvas *canvas, const char *name) {
    if (!snapshot_dir) return;
    char path[512], temporary[520];
    snprintf(path, sizeof(path), "%s/%s.ppm", snapshot_dir, name);
    snprintf(temporary, sizeof(temporary), "%s.tmp", path);
    FILE *file = fopen(temporary, "wb");
    if (!file) return;
    fprintf(file, "P6\n%d %d\n255\n", LOGICAL_W, LOGICAL_H);
    for (int i = 0; i < LOGICAL_W * LOGICAL_H; i++) {
        uint32_t pixel = canvas->pixels[i];
        unsigned char rgb[3] = {pixel >> 24, pixel >> 16 & 255, pixel >> 8 & 255};
        fwrite(rgb, 1, 3, file);
    }
    if (fclose(file) == 0) rename(temporary, path);
}

static int compare_rects(const void *a, const void *b) {
    const SDL_Rect *ra = a, *rb = b;
    if (ra->y != rb->y) return ra->y - rb->y;
    return ra->x - rb->x;
}

int main(int argc, char **argv) {
    if (argc == 2 && !strcmp(argv[1], "--self-test")) {
        if (!sonic_self_test() || !tile_perimeter_self_test() || !raster_replay_self_test() ||
            !inspector_geometry_self_test() || !groove_microscope_self_test() || !shifted_probe_self_test() ||
            !mixed_resolution_self_test() || !viability_self_test()) {
            fprintf(stderr, "native sonic/raster mapping self-test failed\n"); return 4;
        }
        fprintf(stdout, "native sonic/raster mapping self-test passed\n"); return 0;
    }
    for (int i = 1; i + 1 < argc; i++) {
        if (!strcmp(argv[i], "--port")) server_port = atoi(argv[++i]);
        else if (!strcmp(argv[i], "--snapshot-dir")) snapshot_dir = argv[++i];
    }
    if (!SDL_Init(SDL_INIT_VIDEO | SDL_INIT_GAMEPAD | SDL_INIT_AUDIO)) {
        fprintf(stderr, "SDL init failed: %s\n", SDL_GetError()); return 1;
    }
    SDL_AudioSpec audio_spec = {SDL_AUDIO_F32, 2, AUDIO_RATE};
    SDL_AudioStream *audio_stream = SDL_OpenAudioDeviceStream(
        SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK, &audio_spec, synth_audio, NULL);
    if (audio_stream && SDL_ResumeAudioStreamDevice(audio_stream)) audio_online = true;
    else fprintf(stderr, "spatial sonic output unavailable: %s\n", SDL_GetError());
    int count = 0; SDL_DisplayID *ids = SDL_GetDisplays(&count);
    if (!ids || count < 2) {
        fprintf(stderr, "Piecefarm needs two displays; SDL found %d\n", count);
        return 2;
    }
    SDL_Rect *bounds = calloc((size_t)count, sizeof(*bounds));
    for (int i = 0; i < count; i++) SDL_GetDisplayBounds(ids[i], &bounds[i]);
    qsort(bounds, (size_t)count, sizeof(*bounds), compare_rects);
    SDL_Rect top = bounds[0], bottom = bounds[count - 1];
    SDL_DisplayID top_display = 0, bottom_display = 0;
    for (int i = 0; i < count; i++) {
        SDL_Rect candidate = {0}; SDL_GetDisplayBounds(ids[i], &candidate);
        if (candidate.x == top.x && candidate.y == top.y) top_display = ids[i];
        if (candidate.x == bottom.x && candidate.y == bottom.y) bottom_display = ids[i];
    }
    int left = top.x < bottom.x ? top.x : bottom.x;
    int upper = top.y < bottom.y ? top.y : bottom.y;
    int right = top.x + top.w > bottom.x + bottom.w ? top.x + top.w : bottom.x + bottom.w;
    int lower = top.y + top.h > bottom.y + bottom.h ? top.y + top.h : bottom.y + bottom.h;
    SDL_Rect union_bounds = {left, upper, right - left, lower - upper};
    Stage stage = {0};
    if (!make_panel(&stage.board, top, top_display, "Piecefarm Scoreboard [SDL3]") ||
        !make_panel(&stage.soup, bottom, bottom_display, "Piecefarm Lisp Wall [SDL3]")) {
        fprintf(stderr, "display creation failed: %s\n", SDL_GetError()); return 3;
    }
    float stage_refresh = 1000;
    for (int i = 0; i < count; i++) {
        const SDL_DisplayMode *mode = SDL_GetCurrentDisplayMode(ids[i]);
        SDL_Rect display_bounds = {0};
        SDL_GetDisplayBounds(ids[i], &display_bounds);
        fprintf(stdout, "display %s: %dx%d@%d,%d %.2f Hz\n",
                SDL_GetDisplayName(ids[i]), display_bounds.w, display_bounds.h,
                display_bounds.x, display_bounds.y, mode ? mode->refresh_rate : 0);
        if (mode && mode->refresh_rate > 0 && mode->refresh_rate < stage_refresh)
            stage_refresh = mode->refresh_rate;
    }
    if (stage_refresh == 1000) stage_refresh = 120;
    uint64_t frame_interval_ns = (uint64_t)(1000000000.0 / stage_refresh);
    SDL_free(ids); free(bounds); SDL_HideCursor();
    fprintf(stdout, "SDL3 Piecefarm stage: %dx%d@%d,%d; board %dx%d@%d,%d; soup %dx%d@%d,%d; video %s; renderers %s/%s\n",
        union_bounds.w, union_bounds.h, union_bounds.x, union_bounds.y,
        top.w, top.h, top.x, top.y, bottom.w, bottom.h, bottom.x, bottom.y,
        SDL_GetCurrentVideoDriver(), SDL_GetRendererName(stage.board.renderer), SDL_GetRendererName(stage.soup.renderer));
    fflush(stdout);

    pthread_t poller, live_worker;
    pthread_create(&poller, NULL, poll_state, NULL);
    pthread_create(&live_worker, NULL, run_live_runtime, NULL);
    int selected_index = 0;
    /* Supervisor seam for framebuffer QA; interactive users toggle this with
       keyboard/gamepad events below. */
    const char *inspect_start = getenv("PIECEFARM_INSPECT");
    bool inspect_mode = inspect_start && inspect_start[0] && strcmp(inspect_start, "0");
    SDL_Gamepad *gamepad = NULL;
    uint64_t last_frame_ns = 0;
    uint64_t fps_epoch = SDL_GetTicks(), frame_count = 0;
    uint64_t last_geometry_check = fps_epoch;
    uint64_t next_visual_refresh = 0;
    FarmState visual_state = {0};
    int snapshot_phase = 0;
    uint64_t snapshot_at = 0;
    while (running) {
        SDL_Event event;
        while (SDL_PollEvent(&event)) {
            if (event.type == SDL_EVENT_QUIT) running = false;
            if (event.type == SDL_EVENT_GAMEPAD_ADDED && !gamepad)
                gamepad = SDL_OpenGamepad(event.gdevice.which);
            if (event.type == SDL_EVENT_GAMEPAD_REMOVED && gamepad &&
                SDL_GetGamepadID(gamepad) == event.gdevice.which) {
                SDL_CloseGamepad(gamepad); gamepad = NULL;
            }
            if (event.type == SDL_EVENT_KEY_DOWN) {
                if (event.key.key == SDLK_ESCAPE) {
                    if (inspect_mode) inspect_mode = false;
                    else running = false;
                }
                if (event.key.key == SDLK_I || event.key.key == SDLK_RETURN) inspect_mode = !inspect_mode;
                int delta = 0;
                if (event.key.key == SDLK_LEFT) delta = -1;
                if (event.key.key == SDLK_RIGHT) delta = 1;
                if (event.key.key == SDLK_UP) delta = -10;
                if (event.key.key == SDLK_DOWN) delta = 10;
                if (delta) move_selection(&selected_index, delta);
                if (event.key.key == SDLK_LEFTBRACKET) move_margin_probe(selected_index, -1, "native-key");
                if (event.key.key == SDLK_RIGHTBRACKET) move_margin_probe(selected_index, 1, "native-key");
                if (event.key.key == SDLK_PAGEUP) move_margin_probe(selected_index, -8, "native-key");
                if (event.key.key == SDLK_PAGEDOWN) move_margin_probe(selected_index, 8, "native-key");
            }
            if (event.type == SDL_EVENT_GAMEPAD_BUTTON_DOWN) {
                int delta = 0;
                if (event.gbutton.button == SDL_GAMEPAD_BUTTON_SOUTH) inspect_mode = !inspect_mode;
                if (event.gbutton.button == SDL_GAMEPAD_BUTTON_EAST) inspect_mode = false;
                if (event.gbutton.button == SDL_GAMEPAD_BUTTON_DPAD_LEFT) delta = -1;
                if (event.gbutton.button == SDL_GAMEPAD_BUTTON_DPAD_RIGHT) delta = 1;
                if (event.gbutton.button == SDL_GAMEPAD_BUTTON_DPAD_UP) delta = -4;
                if (event.gbutton.button == SDL_GAMEPAD_BUTTON_DPAD_DOWN) delta = 4;
                if (delta) move_selection(&selected_index, delta);
                if (event.gbutton.button == SDL_GAMEPAD_BUTTON_LEFT_SHOULDER)
                    move_margin_probe(selected_index, -1, "xbox");
                if (event.gbutton.button == SDL_GAMEPAD_BUTTON_RIGHT_SHOULDER)
                    move_margin_probe(selected_index, 1, "xbox");
            }
        }
        uint64_t now = SDL_GetTicks();
        uint64_t now_ns = SDL_GetTicksNS();
        if (now - last_geometry_check >= 2000) {
            ensure_panel(&stage.board); ensure_panel(&stage.soup);
            last_geometry_check = now;
        }
        if (now_ns - last_frame_ns >= frame_interval_ns) {
            FarmState latest;
            pthread_mutex_lock(&state_lock); latest = shared_state; pthread_mutex_unlock(&state_lock);
            if (!visual_state.connected || now >= next_visual_refresh) {
                visual_state = latest;
                next_visual_refresh = now + 250;
            }
            present(&stage, &visual_state, now, inspect_mode, selected_index);
            if (visual_state.connected && snapshot_phase == 0 && display_fps > 0) {
                save_ppm(&stage.board.canvas, "board"); save_ppm(&stage.soup.canvas, "soup");
                snapshot_phase = 1; snapshot_at = now + 1000;
            } else if (snapshot_phase == 1 && now >= snapshot_at) {
                save_ppm(&stage.board.canvas, "board-live"); save_ppm(&stage.soup.canvas, "soup-live");
                snapshot_phase = 2; snapshot_at = now + 5000;
            } else if (snapshot_phase == 2 && now >= snapshot_at) {
                save_ppm(&stage.board.canvas, "board-live"); save_ppm(&stage.soup.canvas, "soup-live");
                snapshot_at = now + 5000;
            }
            last_frame_ns = now_ns;
            frame_count++;
            if (now - fps_epoch >= 2000) {
                display_fps = frame_count * 1000.0 / (now - fps_epoch);
                fprintf(stdout, "display %.1f fps (two pixel-perfect 640x360 uploads)\n", display_fps);
                fflush(stdout);
                fps_epoch = now; frame_count = 0;
            }
        } else {
            SDL_DelayPrecise(frame_interval_ns - (now_ns - last_frame_ns));
        }
    }
    pthread_join(poller, NULL);
    pthread_join(live_worker, NULL);
    if (gamepad) SDL_CloseGamepad(gamepad);
    if (audio_stream) SDL_DestroyAudioStream(audio_stream);
    free(stage.board.pixels); free(stage.soup.pixels);
    SDL_DestroyTexture(stage.board.texture); SDL_DestroyRenderer(stage.board.renderer); SDL_DestroyWindow(stage.board.window);
    SDL_DestroyTexture(stage.soup.texture); SDL_DestroyRenderer(stage.soup.renderer); SDL_DestroyWindow(stage.soup.window);
    SDL_Quit();
    return 0;
}
