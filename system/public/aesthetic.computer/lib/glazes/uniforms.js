// 🌚 Glaze (Shader Uniforms) 2022.04.11.04.52
// Already available: iTexture, iTexturePost, iTime, iMouse, iResolution

// ⚠️ This file is for exposing remote customization of uniforms to pieces.
//    These values are set every frame, currently just for the `frag` stage of
//    the pipeline.

const uniforms = {}

uniforms.digitpain0 = {
  // "1i:testInteger": 0,
};

// Used for the `prompt` piece.
// TODO: - [] When 1f:radialBlurAmount is set to 0 there is an xy offset.
//       (Reproduce by visiting `line` then going back to prompt with `)
uniforms.prompt = {
  "1i:fogIterations": 20,
  "1i:shadowIterations": 5,
  "1i:freezeGrain": 1,
  "1f:focalLength": 1,
  "1f:screenScale": 1,
  "1f:shadowRange": 1,
  "1f:cameraDistance": 2.236,
  "1f:volumeRadius": 0.005,
  "1f:inputRadius": 0.005,
  "1f:innerDensity": 20,
  "1f:outerDensity": 10.1,
  "1f:anisotropy": -0.123,
  "1f:lightPower": 8,
  "1f:radialBlurAmount": 0.9, // from 0-1 (or more or less if insane)
  "3f:lightColor": [1, 1, 1], // r, g, b
  "3f:lightDirection": [-1, -1, -0.05], // x, y, z
  "3f:bgColor": [0.084, 0.0533, 0.078], // r, g, b,
  //"3f:bgColor": [0.1, 0.1, 0.1], // r, g, b,
};

export default uniforms;