// scratch_voice.h — the drum head, rubbed rather than struck.
//
// Striking the pad is a note: it fires, it decays, it is over. Dragging a
// finger across it is not. That sound exists exactly as long as the finger
// keeps moving, and every property of it — how bright, how rough, how high the
// head sings — tracks where the finger is and how fast it is going. So it is
// one continuous voice with no note-on and no duration, driven by the control
// side re-stating it every frame.
//
// Ported from the scratch block in Menu Band's MenuBandPercussion.swift so the
// Mac and the machine make the same noise under the same finger.
//
// Dependency-free on purpose, the same way gm_synth.c and fluoddity_voice.c
// are: standard C only, no ALSA, no engine headers. That is what lets the
// voice be built and auditioned on a laptop (tools/scratch-audition.c) instead
// of only on the hardware it ships to.

#ifndef SCRATCH_VOICE_H
#define SCRATCH_VOICE_H

#include <stdint.h>

// What the control side asks for. Everything here is re-stated per frame; the
// voice slews toward it rather than jumping, so a gap between updates sustains
// instead of stuttering.
typedef struct {
    double target;      // 0..~0.22 amplitude asked for; 0 releases
    double cutoff;      // friction band centre, Hz (skin dull, rim bright)
    double resonance;   // head carrier, Hz — this is what gesture speed moves
    double roughness;   // 0..1 grip nonlinearity
    double release;     // fall time once the finger stops, seconds
    double pan;         // -1..1
    int    synthetic;   // 1 = the broader ring-modulated electro surface
} ScratchParams;

typedef struct {
    ScratchParams p;
    double level;       // slewed toward p.target; the actual amplitude
    double noise;       // fast one-pole noise state
    double slow_noise;  // slow one-pole state; the pair makes the band
    double phase;       // carrier phase, 0..1
    uint32_t seed;      // xorshift state
} ScratchVoice;

// Zero the state and seed the noise. Safe to call repeatedly.
void scratch_voice_init(ScratchVoice *v);

// Replace the control parameters. Cheap; call once per control frame.
void scratch_voice_set(ScratchVoice *v, const ScratchParams *p);

// Ask the voice to fall silent. The release ramp still runs.
void scratch_voice_stop(ScratchVoice *v);

// 1 while the voice still has something to contribute — nothing asked for and
// nothing left ringing means the mixer can skip it entirely.
int scratch_voice_active(const ScratchVoice *v);

// Render one sample into a stereo pair (accumulating is the caller's job).
void scratch_voice_render(ScratchVoice *v, double sample_rate,
                          double *out_l, double *out_r);

#endif // SCRATCH_VOICE_H
