// WebGL2 Support Functions, 23.07.11.16.23

import { pathEnd } from "./helpers.mjs";

function createShader(gl, type, source) {
  const shader = gl.createShader(type);
  gl.shaderSource(shader, source);
  gl.compileShader(shader);
  const success = gl.getShaderParameter(shader, gl.COMPILE_STATUS);
  if (success) return shader;

  console.error(gl.getShaderInfoLog(shader), source);
  gl.deleteShader(shader);
}

function createProgram(gl, vertShader, fragShader) {
  const program = gl.createProgram();
  gl.attachShader(program, vertShader);
  gl.attachShader(program, fragShader);
  gl.linkProgram(program);
  const success = gl.getProgramParameter(program, gl.LINK_STATUS);
  if (success) return program;
  console.error(gl.getProgramInfoLog(program));
  gl.deleteProgram(program);
}

// Loads shader sources from a list of filenames: [url1, url2...]
// Then adds them to lib[].
async function preloadShaders(pathArray) {
  const sources = await Promise.all(
    pathArray.map((path) =>
      fetch("/aesthetic.computer/lib/" + path + ".glsl").then((file) => {
        return file.text();
      })
    )
  );

  const lib = {};
  pathArray.forEach((path, i) => (lib[pathEnd(path)] = sources[i]));
  return lib;
}

export { createShader, createProgram, preloadShaders };
