#version 300 es

precision highp float;

in vec2 pos;
in vec4 color;
out vec4 v_color;

uniform vec2 res; // Set this to the actual resolution: (width, height)

void main() {
  // Convert from screen coordinates to clip space
  vec2 alteredPos = pos + 0.5;
  vec2 zeroToOne = alteredPos / res; // Normalize position by resolution
  vec2 zeroToTwo = zeroToOne * 2.0; // Scale up to range [0, 2]
  vec2 clipSpace = zeroToTwo - 1.0; // Translate to range [-1, 1]

  gl_Position = vec4(clipSpace * vec2(1, -1), 0, 1);
  v_color = color;
}
