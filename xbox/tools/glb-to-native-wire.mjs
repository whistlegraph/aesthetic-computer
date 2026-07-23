#!/usr/bin/env node
// Extract a bounded, deterministic wireframe from a simple triangle GLB.
// The compact JSON is intended for native Xbox live pieces, not as a
// replacement for the original textured asset.

import { readFileSync } from "node:fs";

const [source, maxEdgesArg = "1400"] = process.argv.slice(2);
if (!source) throw new Error("usage: glb-to-native-wire.mjs <model.glb> [max-edges]");
const maxEdges = Math.max(12, Math.min(5000, Number.parseInt(maxEdgesArg, 10) || 1400));
const bytes = readFileSync(source);
if (bytes.toString("ascii", 0, 4) !== "glTF" || bytes.readUInt32LE(4) !== 2)
  throw new Error("expected a glTF 2.0 binary file");

let json = null;
let binary = null;
for (let offset = 12; offset + 8 <= bytes.length;) {
  const length = bytes.readUInt32LE(offset);
  const type = bytes.readUInt32LE(offset + 4);
  const chunk = bytes.subarray(offset + 8, offset + 8 + length);
  if (type === 0x4e4f534a) json = JSON.parse(chunk.toString("utf8").replace(/\0+$/, ""));
  else if (type === 0x004e4942) binary = chunk;
  offset += 8 + length;
}
if (!json || !binary) throw new Error("GLB is missing JSON or BIN data");
if (json.extensionsRequired?.length) throw new Error("compressed/extended GLBs are not supported");

const component = {
  5121: { bytes: 1, read: "readUInt8" },
  5123: { bytes: 2, read: "readUInt16LE" },
  5125: { bytes: 4, read: "readUInt32LE" },
  5126: { bytes: 4, read: "readFloatLE" },
};
const widths = { SCALAR: 1, VEC2: 2, VEC3: 3, VEC4: 4 };

function accessor(index) {
  const spec = json.accessors[index];
  const view = json.bufferViews[spec.bufferView];
  const kind = component[spec.componentType];
  const width = widths[spec.type];
  if (!spec || !view || !kind || !width || spec.sparse) throw new Error(`unsupported accessor ${index}`);
  const stride = view.byteStride || kind.bytes * width;
  const start = (view.byteOffset || 0) + (spec.byteOffset || 0);
  return Array.from({ length: spec.count }, (_, row) => {
    const values = Array.from({ length: width }, (_, column) =>
      binary[kind.read](start + row * stride + column * kind.bytes));
    return width === 1 ? values[0] : values;
  });
}

const primitive = json.meshes?.[0]?.primitives?.find((item) => (item.mode ?? 4) === 4);
if (!primitive || primitive.attributes?.POSITION === undefined)
  throw new Error("GLB has no triangle position primitive");
const positions = accessor(primitive.attributes.POSITION);
const indices = primitive.indices === undefined
  ? positions.map((_, index) => index)
  : accessor(primitive.indices);
if (indices.length % 3) throw new Error("triangle index count is not divisible by three");

const unique = new Map();
for (let index = 0; index < indices.length; index += 3) {
  const triangle = [indices[index], indices[index + 1], indices[index + 2]];
  for (const [left, right] of [[0, 1], [1, 2], [2, 0]]) {
    const a = Math.min(triangle[left], triangle[right]);
    const b = Math.max(triangle[left], triangle[right]);
    if (a !== b) unique.set(`${a}:${b}`, [a, b]);
  }
}
const allEdges = [...unique.values()];
const selected = allEdges.length <= maxEdges
  ? allEdges
  : Array.from({ length: maxEdges }, (_, index) =>
      allEdges[Math.floor(index * allEdges.length / maxEdges)]);

const used = [...new Set(selected.flat())].sort((a, b) => a - b);
const remap = new Map(used.map((value, index) => [value, index]));
const raw = used.map((index) => positions[index]);
const min = [0, 1, 2].map((axis) => Math.min(...raw.map((point) => point[axis])));
const max = [0, 1, 2].map((axis) => Math.max(...raw.map((point) => point[axis])));
const center = min.map((value, axis) => (value + max[axis]) / 2);
const scale = 2 / Math.max(...max.map((value, axis) => value - min[axis]), 1e-9);
const round = (value) => Math.round(value * 10000) / 10000;
const vertices = raw.map((point) => point.map((value, axis) => round((value - center[axis]) * scale)));
const edges = selected.map(([a, b]) => [remap.get(a), remap.get(b)]);

console.log(JSON.stringify({
  sourceBytes: bytes.length,
  sourceVertices: positions.length,
  sourceTriangles: indices.length / 3,
  sourceEdges: allEdges.length,
  vertices,
  edges,
}));
