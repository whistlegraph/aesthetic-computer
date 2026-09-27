#ifndef AC_SCREEN_TRIANGLE_H
#define AC_SCREEN_TRIANGLE_H
#include <math.h>
#include <stdint.h>

// Already-projected, flat-color geometry. No matrix transform, clipping
// polygon allocations, perspective correction, or per-pixel color divides.
static inline void ac_screen_triangle(uint32_t *pixels, float *depth,
    int width, int height, int stride,
    float ax, float ay, float az, float bx, float by, float bz,
    float cx, float cy, float cz, uint32_t color) {
  if (!isfinite(ax + ay + az + bx + by + bz + cx + cy + cz)) return;
  float area = (bx-ax)*(cy-ay) - (by-ay)*(cx-ax);
  if (fabsf(area) < .0001f) return;
  float low_x = fmaxf(0, fminf(ax, fminf(bx, cx)));
  float high_x = fminf(width-1, fmaxf(ax, fmaxf(bx, cx)));
  float low_y = fmaxf(0, fminf(ay, fminf(by, cy)));
  float high_y = fminf(height-1, fmaxf(ay, fmaxf(by, cy)));
  if (low_x > high_x || low_y > high_y) return;
  int x0 = (int)floorf(low_x), x1 = (int)ceilf(high_x);
  int y0 = (int)floorf(low_y), y1 = (int)ceilf(high_y);
  const float inv = 1.f / area;
  const float dx0 = (by-cy)*inv, dy0 = (cx-bx)*inv;
  const float dx1 = (cy-ay)*inv, dy1 = (ax-cx)*inv;
  float row0 = ((cx-bx)*(y0+.5f-by) - (cy-by)*(x0+.5f-bx))*inv;
  float row1 = ((ax-cx)*(y0+.5f-cy) - (ay-cy)*(x0+.5f-cx))*inv;
  const float dz0 = az-cz, dz1 = bz-cz;
  for (int y=y0; y<=y1; y++, row0+=dy0, row1+=dy1) {
    float w0=row0, w1=row1;
    int at=y*stride+x0;
    for (int x=x0; x<=x1; x++, at++, w0+=dx0, w1+=dx1) {
      if (w0 < -.000001f || w1 < -.000001f || w0+w1 > 1.000001f) continue;
      float z=cz+w0*dz0+w1*dz1;
      // The Xbox host uses LESS_EQUAL: later coplanar faces paint over
      // earlier ones. A tiny tolerance absorbs floating-point roundoff.
      if (z <= depth[at]+.000001f) { pixels[at]=color; depth[at]=z; }
    }
  }
}
#endif
