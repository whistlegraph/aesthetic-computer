#version 300 es
 
// fragment shaders don't have a default precision so we need
// to pick one. highp is a good default. It means "high precision"
precision highp float;

in vec4 v_color;
out vec4 endColor;

void main() {
  endColor = v_color; 
}