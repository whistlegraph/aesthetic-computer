import * as T from '/vendor/three.module.js';

// The visual layer has no authority over movement or hits.
export function createPaint() {
  const ramp = new T.DataTexture(new Uint8Array([85, 165, 235]), 3, 1, T.RedFormat);
  ramp.minFilter = ramp.magFilter = T.NearestFilter; ramp.needsUpdate = true;
  const materials = new Map(), ink = new T.MeshBasicMaterial({ color: 0x302638, side: T.BackSide });
  function material(color) {
    if (!materials.has(color)) materials.set(color, new T.MeshToonMaterial({ color, gradientMap: ramp }));
    return materials.get(color);
  }
  function shape(geometry, color, parent, x = 0, y = 0, z = 0, outline = true) {
    const mesh = new T.Mesh(geometry, material(color));
    mesh.position.set(x, y, z); parent.add(mesh);
    if (outline) {
      const edge = new T.Mesh(geometry, ink);
      edge.scale.setScalar(1.065); mesh.add(edge);
    }
    return mesh;
  }
  const paper = document.createElement('canvas'); paper.width = paper.height = 512;
  const c = paper.getContext('2d');
  c.fillStyle = '#c4a07f'; c.fillRect(0, 0, 512, 512);
  let seed = 17;
  const random = () => ((seed = (Math.imul(seed, 1664525) + 1013904223) >>> 0) / 4294967296);
  for (let i = 0; i < 5000; i++) {
    c.fillStyle = i % 2 ? '#efd6b51d' : '#694e5520';
    c.fillRect(random() * 512, random() * 512, 1 + random() * 3, 1 + random() * 2);
  }
  for (let y = 0; y < 512; y += 128) {
    c.strokeStyle = '#826b6245'; c.lineWidth = 2; c.beginPath(); c.moveTo(0, y);
    c.bezierCurveTo(128, y + 3, 384, y - 3, 512, y); c.stroke();
  }
  const floorTexture = new T.CanvasTexture(paper); floorTexture.colorSpace = T.SRGBColorSpace;
  floorTexture.wrapS = floorTexture.wrapT = T.RepeatWrapping; floorTexture.repeat.set(6, 6);

  // A bounded RGBA-frame inlet for a future KidLisp worker. No evaluator lives here.
  const image = document.createElement('canvas'); image.width = image.height = 256;
  const context = image.getContext('2d'), fieldTexture = new T.CanvasTexture(image);
  fieldTexture.colorSpace = T.SRGBColorSpace;
  let external = false, lastFrame = -1;
  function setSurfaceFrame(frame) {
    if (frame?.width !== 256 || frame?.height !== 256 ||
        !(frame.data instanceof Uint8ClampedArray) || frame.data.length !== 256 * 256 * 4) {
      throw new TypeError('Arena surface requires a 256 × 256 RGBA frame');
    }
    context.putImageData(new ImageData(frame.data, 256, 256), 0, 0);
    external = true; fieldTexture.needsUpdate = true;
  }
  function tick(time) {
    const frame = Math.floor(time * 12);
    if (external || frame === lastFrame) return;
    lastFrame = frame;
    context.fillStyle = '#53445f'; context.fillRect(0, 0, 256, 256);
    for (let i = 12; i >= 0; i--) {
      const r = i * 16 + Math.sin(time * 0.8 + i * 0.7) * 5;
      context.strokeStyle = ['#e6bd92', '#cc755f', '#9c8ab5'][i % 3];
      context.lineWidth = 5; context.beginPath();
      context.ellipse(128, 128, Math.max(1, r), Math.max(1, r * 0.8), time * 0.1, 0, Math.PI * 2);
      context.stroke();
    }
    fieldTexture.needsUpdate = true;
  }
  return { material, shape, floorTexture, fieldTexture, tick, setSurfaceFrame };
}
