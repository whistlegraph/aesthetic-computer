#version 300 es
 
// an attribute is an input (in) to a vertex shader.
// It will receive data from a buffer
in vec2 pos;
in vec4 color;
out vec4 v_color;

uniform vec2 res;

 
// all shaders have a main function
void main() {
  vec2 adjustedPos = pos; // + vec2(0.5, -0.5);
  vec2 clipSpace = (adjustedPos / res) * 2.0 - 1.0;
  clipSpace.y *= -1.0;
  gl_Position = vec4(clipSpace, 0, 1);
  v_color = color;
}