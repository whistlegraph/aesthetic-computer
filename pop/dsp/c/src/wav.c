// Minimal WAV reader/writer — RIFF/WAVE/fmt /data, PCM int (8/16/24/32) +
// IEEE float (32). Anything fancier (RF64, BWF, multi-chunk past fmt+data)
// is out of scope; pop/ doesn't generate or consume those.

#include "wav.h"
#include "util.h"
#include "acdsp.h"
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define FOURCC(a,b,c,d) ((uint32_t)(a) | ((uint32_t)(b)<<8) | ((uint32_t)(c)<<16) | ((uint32_t)(d)<<24))

static uint32_t read_u32(FILE *f) { uint8_t b[4]; if (fread(b,1,4,f)!=4) return 0;
  return (uint32_t)b[0] | ((uint32_t)b[1]<<8) | ((uint32_t)b[2]<<16) | ((uint32_t)b[3]<<24); }
static uint16_t read_u16(FILE *f) { uint8_t b[2]; if (fread(b,1,2,f)!=2) return 0;
  return (uint16_t)b[0] | ((uint16_t)b[1]<<8); }
static void write_u32(FILE *f, uint32_t v) {
  uint8_t b[4]={v&0xff,(v>>8)&0xff,(v>>16)&0xff,(v>>24)&0xff}; fwrite(b,1,4,f); }
static void write_u16(FILE *f, uint16_t v) {
  uint8_t b[2]={v&0xff,(v>>8)&0xff}; fwrite(b,1,2,f); }

int wav_read(const char *path, wav_t *out) {
  memset(out, 0, sizeof(*out));
  FILE *f = fopen(path, "rb");
  if (!f) { acdsp_set_error("wav_read: cannot open %s", path); return -1; }

  if (read_u32(f) != FOURCC('R','I','F','F')) { fclose(f); acdsp_set_error("wav_read: not RIFF"); return -1; }
  (void)read_u32(f);  // file size
  if (read_u32(f) != FOURCC('W','A','V','E')) { fclose(f); acdsp_set_error("wav_read: not WAVE"); return -1; }

  uint16_t fmt = 0, ch = 0, bits = 0;
  uint32_t sr = 0;
  uint8_t *raw = NULL;
  uint32_t raw_size = 0;

  while (!feof(f)) {
    uint32_t cid = read_u32(f);
    uint32_t csz = read_u32(f);
    if (!cid) break;
    if (cid == FOURCC('f','m','t',' ')) {
      fmt  = read_u16(f);
      ch   = read_u16(f);
      sr   = read_u32(f);
      (void)read_u32(f);  // byte rate
      (void)read_u16(f);  // block align
      bits = read_u16(f);
      if (csz > 16) fseek(f, csz - 16, SEEK_CUR);
    } else if (cid == FOURCC('d','a','t','a')) {
      raw_size = csz;
      raw = (uint8_t *)malloc(csz);
      if (!raw) { fclose(f); acdsp_set_error("wav_read: oom"); return -1; }
      if (fread(raw, 1, csz, f) != csz) {
        free(raw); fclose(f); acdsp_set_error("wav_read: short read"); return -1;
      }
      break;
    } else {
      fseek(f, csz, SEEK_CUR);
    }
  }
  fclose(f);

  if (!raw || !sr || !ch || !bits) {
    free(raw); acdsp_set_error("wav_read: missing fmt/data");
    return -1;
  }

  int bytes_per_sample = bits / 8;
  int n_frames = raw_size / (bytes_per_sample * ch);
  float *data = (float *)malloc(sizeof(float) * (size_t)n_frames * ch);
  if (!data) { free(raw); acdsp_set_error("wav_read: oom"); return -1; }

  size_t n_samples = (size_t)n_frames * ch;
  if (fmt == 3 && bits == 32) {
    memcpy(data, raw, n_samples * sizeof(float));
  } else if (fmt == 1 && bits == 16) {
    const int16_t *src = (const int16_t *)raw;
    for (size_t i = 0; i < n_samples; i++) data[i] = (float)src[i] / 32768.0f;
  } else if (fmt == 1 && bits == 24) {
    for (size_t i = 0; i < n_samples; i++) {
      int32_t s = (int32_t)raw[i*3] | ((int32_t)raw[i*3+1]<<8) | ((int32_t)raw[i*3+2]<<16);
      if (s & 0x800000) s |= 0xff000000;
      data[i] = (float)s / 8388608.0f;
    }
  } else if (fmt == 1 && bits == 32) {
    const int32_t *src = (const int32_t *)raw;
    for (size_t i = 0; i < n_samples; i++) data[i] = (float)src[i] / 2147483648.0f;
  } else if (fmt == 1 && bits == 8) {
    for (size_t i = 0; i < n_samples; i++) data[i] = ((float)raw[i] - 128.0f) / 128.0f;
  } else {
    free(raw); free(data);
    acdsp_set_error("wav_read: unsupported format=%u bits=%u", fmt, bits);
    return -1;
  }
  free(raw);

  out->sample_rate = (int)sr;
  out->channels    = (int)ch;
  out->n_frames    = n_frames;
  out->data        = data;
  return 0;
}

int wav_write(const char *path, const wav_t *in, int bits, int is_float) {
  if (bits != 16 && bits != 24 && bits != 32) {
    acdsp_set_error("wav_write: bits must be 16/24/32"); return -1;
  }
  if (is_float && bits != 32) {
    acdsp_set_error("wav_write: float requires 32-bit"); return -1;
  }
  FILE *f = fopen(path, "wb");
  if (!f) { acdsp_set_error("wav_write: cannot open %s", path); return -1; }

  size_t n_samples  = (size_t)in->n_frames * in->channels;
  size_t bytes_per  = bits / 8;
  uint32_t data_sz  = (uint32_t)(n_samples * bytes_per);
  uint16_t fmt_tag  = is_float ? 3 : 1;
  uint32_t byte_rate = (uint32_t)in->sample_rate * in->channels * (uint32_t)bytes_per;
  uint16_t block_al  = (uint16_t)(in->channels * bytes_per);

  write_u32(f, FOURCC('R','I','F','F'));
  write_u32(f, 36 + data_sz);
  write_u32(f, FOURCC('W','A','V','E'));
  write_u32(f, FOURCC('f','m','t',' '));
  write_u32(f, 16);
  write_u16(f, fmt_tag);
  write_u16(f, (uint16_t)in->channels);
  write_u32(f, (uint32_t)in->sample_rate);
  write_u32(f, byte_rate);
  write_u16(f, block_al);
  write_u16(f, (uint16_t)bits);
  write_u32(f, FOURCC('d','a','t','a'));
  write_u32(f, data_sz);

  if (is_float) {
    fwrite(in->data, sizeof(float), n_samples, f);
  } else if (bits == 16) {
    for (size_t i = 0; i < n_samples; i++) {
      float x = in->data[i];
      if (x > 1.0f) x = 1.0f; else if (x < -1.0f) x = -1.0f;
      int16_t v = (int16_t)lrintf(x * 32767.0f);
      write_u16(f, (uint16_t)v);
    }
  } else if (bits == 24) {
    for (size_t i = 0; i < n_samples; i++) {
      float x = in->data[i];
      if (x > 1.0f) x = 1.0f; else if (x < -1.0f) x = -1.0f;
      int32_t v = (int32_t)lrintf(x * 8388607.0f);
      uint8_t b[3] = { v & 0xff, (v >> 8) & 0xff, (v >> 16) & 0xff };
      fwrite(b, 1, 3, f);
    }
  } else if (bits == 32) {
    for (size_t i = 0; i < n_samples; i++) {
      float x = in->data[i];
      if (x > 1.0f) x = 1.0f; else if (x < -1.0f) x = -1.0f;
      int32_t v = (int32_t)lrint((double)x * 2147483647.0);
      write_u32(f, (uint32_t)v);
    }
  }
  fclose(f);
  return 0;
}

void wav_free(wav_t *w) {
  if (!w) return;
  free(w->data);
  w->data = NULL;
  w->n_frames = 0;
}
