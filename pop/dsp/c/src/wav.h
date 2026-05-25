#ifndef ACDSP_WAV_H
#define ACDSP_WAV_H

typedef struct {
  int    sample_rate;
  int    channels;
  int    n_frames;     // samples per channel
  float *data;         // interleaved float32, length n_frames * channels
} wav_t;

// Read any PCM (8/16/24/32 int) or 32-bit IEEE float WAV. Returns 0 on success.
int  wav_read(const char *path, wav_t *out);

// Write a WAV. bits ∈ {16, 24, 32}; if `is_float` is non-zero, writes IEEE float.
int  wav_write(const char *path, const wav_t *in, int bits, int is_float);

void wav_free(wav_t *w);

#endif
