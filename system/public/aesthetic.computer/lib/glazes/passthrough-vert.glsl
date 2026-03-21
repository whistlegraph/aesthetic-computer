#version 300 es
precision highp float;

in vec2 a_position;
in vec2 a_texc;
out vec2 v_texc;

void main() {
  gl_Position = vec4(a_position, 0., 1.);
  v_texc = a_texc;
}
