import { rayControls } from "./raytrace.mjs";
export function createRayWasm(bytes) {
  const module = new WebAssembly.Module(bytes);
  if (WebAssembly.Module.imports(module).length) throw new TypeError("Ray frame module must have no imports");
  const { exports } = new WebAssembly.Instance(module);
  return {
    bytes: bytes.length,
    render(controls = {}) {
      const { width, height, frame, bounces, greenX } = rayControls(controls);
      const offset = exports.render(width, height, bounces, greenX);
      if (!offset) throw new RangeError("Wasm rejected ray controls");
      // This is a borrowed view; copy it to retain a frame after the next call.
      return { width, height, frame, rgba: new Uint8ClampedArray(exports.memory.buffer, offset, width * height * 4), rays: exports.rayCount(), intersections: exports.intersectionCount() };
    },
  };
}
