#version 300 es
precision highp float;

in vec2 v_texc;
out vec4 endColor;

uniform sampler2D iTexture; // The original texture from aesthetic.computer.
uniform sampler2D iTexturePost; // The resulting post-processing effect.
uniform vec2 iMouse;
uniform vec2 iResolution;
uniform float iTime;

// See also: https://iquilezles.org/www/articles/distfunctions2d/distfunctions2d.htm
float sdSegment(in vec2 p, in vec2 a, in vec2 b) {
  vec2 pa = p - a, ba = b - a;
  float h = clamp(dot(pa, ba) / dot(ba, ba), 0.0, 1.0);
  return length(pa - ba * h);
}

// Sharp-bilinear sampling: keeps pixel interiors crisp but anti-aliases the
// texel boundaries, so the low-res glaze buffer upscales to a large/high-DPI
// canvas without the uneven "doubled pixel" artifacts that plain NEAREST
// produces at non-integer scale factors (worst on bright, high-contrast pixels).
vec2 sharpBilinearUV(vec2 uv, vec2 texSize) {
  vec2 pix = uv * texSize;
  vec2 fl = floor(pix + 0.5);
  vec2 dxy = fwidth(pix);
  // Avoid division blowups where derivatives are ~0 (e.g. degenerate edges).
  dxy = max(dxy, vec2(1e-5));
  pix = fl + clamp((pix - fl) / dxy, -0.5, 0.5);
  return pix / texSize;
}

void main() {
  vec2 uv = vec2(v_texc.x, 1. - v_texc.y);
  vec2 texSize = vec2(textureSize(iTexturePost, 0));
  vec3 color = texture(iTexturePost, sharpBilinearUV(uv, texSize)).xyz;

  // *** Learning SDFs through making cursors. 2022.02.12.01.14
  /*
  float w = 2. / resolution.x;
  float h = 2. / resolution.y;

  float dm = .024;

  vec2 ratio = vec2(1, resolution.x / resolution.y);

  float dist = sdSegment(v_texc, mouse, mouse + vec2(w, h));

  if (dist < dm) {
    color = vec3(1., 0., 0.);
  }

  float xlength = 30. / resolution.x;
  float ylength = 30. / resolution.y;

  if (abs(v_texc.x - mouse.x) < w/2. ||
      abs(v_texc.y - mouse.y) < h/2.) {

    if (v_texc.y < mouse.y + ylength &&
        v_texc.y > mouse.y - ylength
        &&
        v_texc.x < mouse.x + xlength &&
        v_texc.x > mouse.x - xlength) {
      color = vec3(1., 0., 0.);
    }
  }
  */

  endColor = vec4(color, 1.0);
}
