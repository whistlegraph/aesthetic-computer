#version 300 es
precision highp float;

in vec2 v_texc;
out vec4 outColor;

uniform sampler2D iTexture; // The original texture from aesthetic.computer.
uniform sampler2D iTexturePost; // The processed texture so far.
uniform vec2 iMouse;
uniform vec2 iResolution;
uniform float iTime;

void main() {
  outColor = vec4(texture(iTexturePost, vec2(v_texc.x, 1. - v_texc.y)).xyz, 1.0);
}
