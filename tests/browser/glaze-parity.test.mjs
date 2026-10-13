// Compare the complete prompt glaze pipeline with its pre-optimization shader.
// Real WebGL, fixed input/time: catches flips, changed sampling and lost uniforms.
// AC_TEST_URL must serve the source tree (or the deployed revision under test).
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { ACSession, CONFIG } from "./ac-harness.mjs";

const reference = readFileSync(new URL("./fixtures/prompt-frag-reference.glsl", import.meta.url), "utf8");
const ac = await ACSession.open();
try {
  // An empty same-origin page avoids running the application during this test.
  await ac.page.setRequestInterception(true);
  ac.page.on("request", (request) => request.isNavigationRequest()
    ? request.respond({ status: 200, contentType: "text/html", body: "<!doctype html><body></body>" })
    : request.continue());
  await ac.page.goto(`${CONFIG.baseURL}/__glaze-parity`);
  const results = await ac.page.evaluate(async (reference) => {
    const Glaze = await import("/aesthetic.computer/lib/glaze.mjs");
    const { default: defaults } = await import("/aesthetic.computer/lib/glazes/uniforms.js");
    const root = "/aesthetic.computer/lib/glazes/";
    const [vertex, compute, display] = await Promise.all([
      "passthrough-vert", "prompt/prompt-compute", "prompt/prompt-display",
    ].map((name) => fetch(`${root}${name}.glsl`).then((r) => r.text())));
    const input = document.createElement("canvas");
    input.width = 32; input.height = 24;
    const ctx = input.getContext("2d");
    const pixels = ctx.createImageData(32, 24);
    for (let y = 0; y < 24; y++) for (let x = 0; x < 32; x++) {
      const i = (y * 32 + x) * 4;
      pixels.data.set((x + y) % 5 < 2
        ? [(x * 23) % 256, (y * 31) % 256, ((x + y) * 17) % 256, 255]
        : [68, 54, 88, 255], i);
    }
    ctx.putImageData(pixels, 0, 0);
    const refCanvas = document.createElement("canvas");
    const gl = refCanvas.getContext("webgl2", { alpha: false, antialias: false, depth: false });
    if (!gl) throw new Error("WebGL2 is required");
    function program(fragment) {
      const p = gl.createProgram();
      for (const [type, source] of [[gl.VERTEX_SHADER, vertex], [gl.FRAGMENT_SHADER, fragment]]) {
        const shader = gl.createShader(type);
        gl.shaderSource(shader, source); gl.compileShader(shader);
        if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) throw new Error(gl.getShaderInfoLog(shader));
        gl.attachShader(p, shader);
      }
      gl.linkProgram(p);
      if (!gl.getProgramParameter(p, gl.LINK_STATUS)) throw new Error(gl.getProgramInfoLog(p));
      return p;
    }
    const programs = [reference, compute, display].map(program);
    function geometry(p) {
      for (const [name, data] of [
        ["a_position", [-1, 1, -1, -1, 1, -1, 1, 1]],
        ["a_texc", [0, 0, 0, 1, 1, 1, 1, 0]],
      ]) {
        gl.bindBuffer(gl.ARRAY_BUFFER, gl.createBuffer());
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(data), gl.STATIC_DRAW);
        const a = gl.getAttribLocation(p, name);
        gl.vertexAttribPointer(a, 2, gl.FLOAT, false, 0, 0); gl.enableVertexAttribArray(a);
      }
      gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, gl.createBuffer());
      gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, new Uint16Array([0, 1, 2, 0, 2, 3]), gl.STATIC_DRAW);
    }
    const fb = gl.createFramebuffer();
    function texture(w, h, data, filter) {
      const t = gl.createTexture(); gl.bindTexture(gl.TEXTURE_2D, t);
      gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, w, h, 0, gl.RGBA, gl.UNSIGNED_BYTE, data);
      for (const parameter of [gl.TEXTURE_MIN_FILTER, gl.TEXTURE_MAG_FILTER]) gl.texParameteri(gl.TEXTURE_2D, parameter, filter);
      for (const parameter of [gl.TEXTURE_WRAP_S, gl.TEXTURE_WRAP_T]) gl.texParameteri(gl.TEXTURE_2D, parameter, gl.CLAMP_TO_EDGE);
      return t;
    }
    function renderReference(w, h, dpr, time, uniforms) {
      refCanvas.width = w * dpr; refCanvas.height = h * dpr;
      gl.activeTexture(gl.TEXTURE0);
      const source = texture(32, 24, pixels.data, gl.NEAREST);
      const a = texture(w, h, null, gl.LINEAR), b = texture(w, h, null, gl.LINEAR);
      for (let stage = 0; stage < 3; stage++) {
        const p = programs[stage]; gl.useProgram(p); geometry(p);
        gl.bindFramebuffer(gl.FRAMEBUFFER, stage < 2 ? fb : null);
        if (stage < 2) gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, stage === 0 ? a : b, 0);
        const width = stage < 2 ? w : refCanvas.width, height = stage < 2 ? h : refCanvas.height;
        gl.viewport(0, 0, width, height);
        gl.activeTexture(gl.TEXTURE0); gl.bindTexture(gl.TEXTURE_2D, source);
        gl.activeTexture(gl.TEXTURE1); gl.bindTexture(gl.TEXTURE_2D, stage === 1 ? a : b);
        gl.uniform1i(gl.getUniformLocation(p, "iTexture"), 0);
        gl.uniform1i(gl.getUniformLocation(p, "iTexturePost"), 1);
        gl.uniform2f(gl.getUniformLocation(p, "iResolution"), width, height);
        gl.uniform1f(gl.getUniformLocation(p, "iTime"), time);
        if (stage === 0) for (const [key, value] of Object.entries(uniforms)) {
          const [type, name] = key.split(":");
          gl[`uniform${type}`](gl.getUniformLocation(p, name), ...(Array.isArray(value) ? value : [value]));
        }
        gl.drawElements(gl.TRIANGLES, 6, gl.UNSIGNED_SHORT, 0);
      }
      const bytes = new Uint8Array(refCanvas.width * refCanvas.height * 4);
      gl.readPixels(0, 0, refCanvas.width, refCanvas.height, gl.RGBA, gl.UNSIGNED_BYTE, bytes);
      for (const t of [source, a, b]) gl.deleteTexture(t);
      return bytes;
    }
    const results = [];
    const scenes = [
      { name: "desktop", w: 192, h: 144, time: 0 },
      { name: "portrait", w: 144, h: 192, time: 1520 },
      { name: "retina", w: 160, h: 120, dpr: 2, time: 3100 },
      { name: "change-dpr-only", w: 160, h: 120, time: 3100 },
      { name: "custom-steps", w: 192, h: 144, time: 9876, overrides: { "1i:fogIterations": 31, "1i:shadowIterations": 7, "1i:freezeGrain": 0 } },
      { name: "thin-input", w: 192, h: 144, time: 1200, overrides: { "1f:inputRadius": 0.003 } },
      { name: "zero-radial", w: 192, h: 144, time: 3100, overrides: { "1f:radialBlurAmount": 0 } },
      { name: "reverse-ray", w: 192, h: 144, time: 700, overrides: { "1f:radialBlurAmount": 8, "3f:lightDirection": [0.2, 1, 0.05] } },
      { name: "empty", w: 192, h: 144, time: 0, overrides: { "1f:innerDensity": 0, "1f:outerDensity": 0 } },
      { name: "restore-defaults", w: 192, h: 144, time: 0 },
    ];
    for (const scene of scenes) {
      const { w, h, time, dpr = 1 } = scene;
      Object.defineProperty(window, "devicePixelRatio", { configurable: true, value: dpr });
      const settings = { ...defaults.prompt, ...scene.overrides };
      await Glaze.on(32, 24, { width: w, height: h }, w, h, document.body, "prompt", () => {});
      // Exercise live uniform switching, including returning to the specialized shader.
      for (const [key, value] of Object.entries(settings)) Glaze.setDynamicUniform(key, value);
      Glaze.update(input); Glaze.render(time, { x: 0, y: 0 });
      const actualGL = Glaze.getCan().getContext("webgl2");
      const actual = new Uint8Array(w * h * dpr * dpr * 4);
      actualGL.readPixels(0, 0, w * dpr, h * dpr, actualGL.RGBA, actualGL.UNSIGNED_BYTE, actual);
      const expected = renderReference(w, h, dpr, time, settings);
      let max = 0, sum = 0, nonblack = 0;
      for (let i = 0; i < actual.length; i++) {
        const difference = Math.abs(actual[i] - expected[i]);
        max = Math.max(max, difference); sum += difference;
        if (i % 4 !== 3 && expected[i] > 0) nonblack++;
      }
      results.push({ name: scene.name, max, mean: sum / actual.length, nonblack,
        error: actualGL.getError(), referenceError: gl.getError() });
      Glaze.off();
    }
    return results;
  }, reference);
  for (const result of results) {
    console.log(JSON.stringify(result));
    assert.equal(result.error, 0); assert.equal(result.referenceError, 0);
    if (result.name !== "empty") assert.ok(result.nonblack > 0, "reference must contain visible pixels");
    // Equivalent integration differs slightly after float rounding and RGBA8 quantization.
    assert.ok(result.max <= 2 && result.mean < 0.01, `${result.name}: visible shader regression`);
  }
} finally {
  await ac.close();
}
