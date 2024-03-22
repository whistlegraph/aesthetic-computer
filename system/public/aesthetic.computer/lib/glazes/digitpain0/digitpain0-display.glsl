#version 300 es
precision highp float;

in vec2 v_texc;
out vec4 outColor;

uniform sampler2D iTexture;
uniform sampler2D iTexturePost;
uniform vec2 iMouse;
uniform vec2 iResolution;
uniform float iTime;

vec3 lightDirection = normalize(vec3(0., 2., 1.));
const float specularExponent = 2.;
vec3 specularColor = vec3(1.);
float specularBrightness = 40.;

float getSpecularity(vec2 pos)
{
  vec2 delta = 1./iResolution;

  vec4 selfColor = texture(iTexturePost, pos);
  vec4 upColor = texture(iTexturePost, pos + vec2(0., delta.y));
  vec4 rightColor = texture(iTexturePost, pos + vec2(delta.x, 0.));

  float selfBrightness = max(max(selfColor.x, selfColor.y), selfColor.z);
  float upBrightness = max(max(upColor.x, upColor.y), upColor.z);
  float rightBrightness = max(max(rightColor.x, rightColor.y), rightColor.z);

  vec3 up = vec3(0., delta.y, upBrightness) - vec3(0., 0., selfBrightness);
  vec3 right = vec3(delta.x, 0., rightBrightness) - vec3(0., 0., selfBrightness);

  up = normalize(up);
  right = normalize(right);

  vec3 normal = cross(up, right);

  float specular = pow(max(dot(normal, lightDirection), 0.), specularExponent);
  return specular;
}

void main() {
  vec3 imgColor = texture(iTexture, v_texc.xy).xyz;
  vec3 postColor = texture(iTexturePost, v_texc.xy).xyz;
  float postFactor = max(0., cos(iTime*.001));
  if (sin(iTime*.001) < 0.)
  {
    postFactor = 0.;
  }
  outColor = vec4(imgColor + vec3(getSpecularity(v_texc.xy)*postFactor), 1.0);
}
