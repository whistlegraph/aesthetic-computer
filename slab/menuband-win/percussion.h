// percussion.h — the Menu Band percussion engine for Windows.
//
// A C port of MenuBandPercussion.swift (which mirrors lib/percussion.mjs):
// the twelve-drum keyboard kit (kick, crash, snare, splash, clap, snap,
// cowbell, closed hat, block, open hat, tambourine, ride — one per pitch
// class, C upward) and the TrackDrum membrane: five concentric instruments
// (kick, tom, snare, hat, click) read from a strike's normalized position on
// the trackpad. Voices are layered one-shots (sine / triangle / square /
// noise through a biquad, with sweeps, exponential tails, drive and the
// TR-808 metal cluster) plus held voices for the hi-hat foot-pedal model.
//
// Threading: perc_hit_* / perc_strike_* / perc_release / perc_flush are
// called from the UI thread and stage work under an internal critical
// section; perc_render drains it on the audio thread. perc_render ADDS into
// an interleaved stereo float buffer. Everything is allocation-free after
// perc_init.
#ifndef MENUBAND_PERCUSSION_H
#define MENUBAND_PERCUSSION_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
    PERC_KICK = 0, PERC_CRASH, PERC_SNARE, PERC_SPLASH, PERC_CLAP, PERC_SNAP,
    PERC_COWBELL, PERC_HAT_CLOSED, PERC_BLOCK, PERC_HAT_OPEN, PERC_TAMBO, PERC_RIDE,
    PERC_DRUM_COUNT
} PercDrum;

// Call once before the audio thread starts.
void perc_init(double sample_rate);

// The keyboard kit: one drum, velocity 0…1, pan -1…1 (0 = centre).
// `group` identifies the press for perc_release (held voices: open hat rings
// until released; closed hat stages a lift click at release). 0 = no group.
void perc_hit_drum(PercDrum drum, double velocity, double pan, uint64_t group);

// The TrackDrum membrane: a strike at normalized pad position (x, y in 0…1,
// origin top-left of the pad, pad aspect ~0.82:0.50 as on a MacBook pad),
// velocity 0…1. `anchors` are the other resting contacts (fingers damping
// the head), `anchor_count` may be 0. Returns the pulse drum the strike read
// as (for lighting a pad: kick / block / snare / closed hat).
PercDrum perc_strike_pad(double x, double y, double velocity,
                         const double *anchors_xy, int anchor_count, uint64_t group);

// Finger-up articulation for a contact that struck the membrane.
void perc_lift_pad(double x, double y, uint64_t group);

// Release every held voice of a group (open-hat damping, closed-hat lift).
void perc_release(uint64_t group);

// Drop everything (focus loss / panic).
void perc_flush(void);

// Kit-wide pitch multiplier (the FX surface's bend), 1.0 = none.
void perc_set_pitch(double scale);

// Percussion trim 0…3 (the Mac's outputLevel is 0.90 at unity; above 1 is
// make-up gain, which the output soft limiter keeps from clipping).
void perc_set_level(double level);

// Render `frames` of interleaved stereo float32, ADDED into `out`.
void perc_render(float *out, int frames);

// Three-letter labels as the Mac prints them (BAS, SNR, CLP …).
const char *perc_label(PercDrum drum);

// Diagnostics: active voice count after the last render.
int perc_active_voices(void);

#ifdef __cplusplus
}
#endif
#endif
