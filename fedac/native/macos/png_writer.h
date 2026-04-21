// png_writer.h — ARGB8888 framebuffer → PNG via CoreGraphics/ImageIO.
// Pure C (no ObjC). Writes `width * height * 4` bytes from `argb_pixels`
// (kARGB8888, premultiplied last — matches ACFramebuffer layout) to
// `out_path`. Returns 1 on success, 0 on failure (fprintf logs reason).
#ifndef AC_MACOS_PNG_WRITER_H
#define AC_MACOS_PNG_WRITER_H

#include <stdint.h>

int png_write_argb(const char *out_path,
                   const uint32_t *argb_pixels,
                   int width, int height, int stride_pixels);

#endif
