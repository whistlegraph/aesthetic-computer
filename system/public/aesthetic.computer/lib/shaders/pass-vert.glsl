#version 300 es
 
// an attribute is an input (in) to a vertex shader.
// It will receive data from a buffer
in vec2 pos;
in vec4 color;
out vec4 v_color;

uniform vec2 res;
 
// all shaders have a main function
void main() {
  // Convert screen coordinates to clipspace.
  vec2 clipSpace = (pos / res) * 2.0 - 1.0; 
  clipSpace.y *= -1.0;
  gl_Position = vec4(clipSpace, 0, 1);

  v_color = color;
}