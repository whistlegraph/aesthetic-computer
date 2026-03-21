#version 300 es
precision highp float;

in vec2 v_texc;
out vec4 outColor;

uniform sampler2D iTexture;
uniform vec2 iMouse;
uniform vec2 iResolution;
uniform float iTime;

vec3 lightDirection = normalize(vec3(0., 2., 1.));
const float specularExponent = 20.;
float decimateFactor = 200.;
vec3 specularColor = vec3(1.);
float specularBrightness = 40.;

float getSpecularity(vec2 pos)
{
  float delta = 1./iResolution.x;

  vec4 selfColor = texture(iTexture, pos);
  vec4 upColor = texture(iTexture, pos + vec2(0., delta));
  vec4 rightColor = texture(iTexture, pos + vec2(delta, 0.));

  float selfBrightness = max(max(selfColor.x, selfColor.y), selfColor.z);
  float upBrightness = max(max(upColor.x, upColor.y), upColor.z);
  float rightBrightness = max(max(rightColor.x, rightColor.y), rightColor.z);

  vec3 up = vec3(1., 0., upBrightness) - vec3(0., 0., selfBrightness);
  vec3 right = vec3(0., 1., rightBrightness) - vec3(0., 0., selfBrightness);

  up = normalize(up);
  right = normalize(right);

  vec3 normal = cross(up, right);

  float specular = pow(max(dot(normal, lightDirection), 0.), specularExponent);
  return specular;
}

vec2 getSpecularLocation(vec2 pos)
{
  return floor(pos*decimateFactor)/decimateFactor;
}

float random(vec2 st) {
  return fract(sin(dot(st.xy, vec2(12.9898, 78.233)))*43758.5453123);
}

void main() {
  lightDirection = normalize(vec3(0., 2., random(vec2(0., iTime))));
  float specular = 0.;
  for (int i = -5; i <= 5; ++i)
  {
    for (int j = 0; j <= 1; ++j)
    {
      specular += getSpecularity(getSpecularLocation(v_texc.xy + vec2(i*8, j*3)/iResolution.x))/(1. + abs(float(i)));
    }
  }
  specular /= 9.;
  outColor = vec4(1.*texture(iTexture, v_texc.xy).xyz + specularBrightness * specularColor * specular, 1.0);
}
