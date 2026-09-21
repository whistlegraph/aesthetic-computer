// singer.h — speech-to-singing core. Pure DSP, no audio device, no I/O.
//
// This is the shared core: /pop, Menu Band (AVAudioEngine) and fedac/native
// (ALSA) all link THIS and supply their own audio device. Follows the existing
// precedent in the repo — Menu Band's CGMSynth target is a symlink to
// fedac/native/src/gm_synth.c. Same idea: one C file, many consumers.
//
// Threading contract:
//   singer_analyze_chunk()  — call from a BACKGROUND thread. Slow (libworld).
//   singer_render_phrase()  — call from a RENDER thread. ~30ms. Allocates.
//   singer_audio_block()    — call from the AUDIO thread. Realtime-safe:
//                             no malloc, no locks, no file I/O. Ever.
//
// The chunked analysis is the whole point: we only need the FIRST phrase
// analyzed to make a sound. The rest streams in behind the playhead.

#ifndef SINGER_H
#define SINGER_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

#define SINGER_FP_MS      5.0     // WORLD frame period
#define SINGER_MAX_WORDS  4096
#define SINGER_MAX_PAT    32

typedef struct {
  int   a, b;        // word span, in WORLD frames
  int   vs, ve;      // vowel nucleus (onset consonant = a..vs, coda = ve..b)
  char  text[48];
  int   nsyl;        // syllables in this word (0 or 1 = one); SCORE mode
                     // splits the word into that many sung units
  int   fixed;       // 1 = vs/ve were MEASURED by the host (wordmeta):
                     // trust them instead of re-detecting the nucleus
} singer_word;

// SCORE mode: one note per syllable, in lyric order. Time in sixteenths from
// the score's downbeat. The singer sings exactly these pitches; how much of
// the spoken contour survives is the profile's `lock`.
#define SINGER_MAX_NOTES 4096
typedef struct { double midi; double at16; double dur16; } singer_note;

typedef enum { SINGER_SNAP = 0, SINGER_MELODY = 1, SINGER_SCORE = 2 } singer_mode;

typedef struct {
  double bpm;
  double morph;        // 0 = spoken, 1 = sung (drives pitch AND time together)
  double snap;         // 0 = her pitch, 1 = hard onto the scale
  double depth;        // 0 = in your face, 1 = buried in the track
  double level;        // vocal bus gain
  double f0_floor;     // per-speaker: frames below this never sing
  double consonant_gain;
  singer_mode mode;
  int    root_pc;      // 0=C .. 9=A
  int    scale[8];     // semitone offsets from root
  int    n_scale;
  int    pattern[SINGER_MAX_PAT];  // step durations in 16ths; negative = rest
  int    n_pattern;
  // ── the voice profile's constraints (SCORE mode) ──
  double lock;         // 0 = spoken contour rides on the note, 1 = hard on it
  double vib_hz;       // vibrato rate (profile: 4x battery cycles/day)
  double vib_cents;    // vibrato depth
  double sustain_db;   // only nucleus frames within this many dB of the
                       // run's peak are stretched; quieter voiced edges
                       // (b, n, l, r) keep speaking rate. 0 = whole run.
  double gap_ms;       // a held vowel stops this long before the next
                       // unit's onset consonant (a stop's closure). 0 = legato.
  double presence_db;  // peaking EQ at 3 kHz on the sung output, dB. 0 = off.
  double voiced_consonant_mix;   // 0…1: how much of the SPOKEN original is
                       // blended over voiced consonants (b d g m n l r w) in
                       // onset/coda regions; unvoiced ones are always original.
  double hold_ms;      // longest a single vowel may sound; the rest of a long
                       // note is air. 0 = hold it for the whole note.
  double consonant_balance;   // 0…1: blend toward matching the spliced
                       // consonant's level to its own vowel's, as spoken,
                       // instead of to the vocoder's near-silent estimate.
  double consonant_stretch;   // >1 slows onset/coda consonant regions, the
                       // way clear speech lengthens them. 1 = speaking rate.
  int    sustain_band; // 1 = judge the sustain zone by 400 Hz–4 kHz energy
                       // (vowel vs voiced consonant), 0 = total energy
  int    loop_sustain; // 1 = a held vowel wanders its nucleus back and forth
                       // at speaking rate (alive); 0 = eased frozen hold.
} singer_params;

typedef struct singer singer;

// ── lifecycle ──────────────────────────────────────────────────────────
singer *singer_create(const double *pcm, int n, int fs);
void    singer_destroy(singer *s);
void    singer_set_words(singer *s, const singer_word *w, int n);
singer_params *singer_params_ptr(singer *s);   // live-tweakable (audio thread reads)

// ── analysis (BACKGROUND thread) ───────────────────────────────────────
// Analyze source frames [from, to). Call repeatedly to stream the corpus in.
// Returns frames actually analyzed. singer_analyzed() reports the watermark.
int  singer_analyze_chunk(singer *s, int from_frame, int to_frame);
int  singer_analyzed(const singer *s);
int  singer_total_frames(const singer *s);
// Is phrase p fully covered by the analysis watermark yet?
int  singer_phrase_ready(const singer *s, int phrase);
int  singer_phrase_count(const singer *s);

// ── render (RENDER thread) ─────────────────────────────────────────────
// THE FAUCET: render exactly `bars` bars of singing starting at word
// `word_start`, consuming however many words actually fit. Returns the buffer
// (caller owns) and reports how many words it used, so the caller advances its
// cursor and the next call picks up seamlessly. Because every buffer is an
// exact whole number of bars, phrases chain back-to-back with NO dead air —
// which is what "the faucet is on" means for a live set.
double *singer_render_bars(singer *s, int word_start, int bars,
                           int *out_len, int *words_used);
int singer_word_count(const singer *s);
int singer_words_ready(const singer *s);   // how many words the analysis covers

// ── SCORE mode ─────────────────────────────────────────────────────────
// Notes pair with syllables: word w with nsyl k consumes notes in order.
void singer_set_score(singer *s, const singer_note *n, int count);
int  singer_note_count(const singer *s);
// Render the window [from16, to16) of the score — exactly that many
// sixteenths of audio — singing every syllable whose onset falls inside it
// and whose source speech is analyzed. Caller owns the buffer. `notes_used`
// is how many notes were sung (stops at the first unanalyzed one).
double *singer_render_score(singer *s, double from16, double to16,
                            int *out_len, int *notes_used);

// ── audio (AUDIO thread — realtime-safe) ───────────────────────────────
// Hand it a rendered phrase; it takes ownership and plays it on the next bar.
void singer_queue_phrase(singer *s, double *buf, int len);
void singer_transport(singer *s, int playing);
int  singer_pending(const singer *s);   // is a phrase already queued?
// Fills `out` (mono) with bed + vocal + reverb. NEVER allocates or locks.
void singer_audio_block(singer *s, float *out, int frames);
long long singer_pos(const singer *s);   // the sample clock

#ifdef __cplusplus
}
#endif
#endif
