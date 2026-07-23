#!/usr/bin/env node
// Extract a textured head/neck LOD from a static Meshy GLB. Geometry is kept
// as real triangles while the PBR atlas is sampled into compact per-face color
// for the sandboxed Xbox scene (which already has one UV texture bound to the
// animated body).

import { readFileSync, writeFileSync } from "node:fs";
import { MeshoptSimplifier } from "meshoptimizer";
import sharp from "sharp";

const [source, destination, requestedCut = "0.5", requestedTriangles = "5200"] = process.argv.slice(2);
if (!source || !destination)
  throw new Error("usage: glb-head-bake.mjs <meshy.glb> <head-bake.json> [vertical-cut=0.5]");
const verticalCut = Math.max(0.3, Math.min(0.75, Number(requestedCut) || 0.5));
const targetTriangles = Math.max(500, Math.min(6500,
  Number.parseInt(requestedTriangles, 10) || 5200));
const bytes = readFileSync(source);
if (bytes.toString("ascii", 0, 4) !== "glTF" || bytes.readUInt32LE(4) !== 2)
  throw new Error("expected a glTF 2.0 binary file");

let json, binary;
for (let offset = 12; offset + 8 <= bytes.length;) {
  const length = bytes.readUInt32LE(offset), type = bytes.readUInt32LE(offset + 4);
  const chunk = bytes.subarray(offset + 8, offset + 8 + length);
  if (type === 0x4e4f534a) json = JSON.parse(chunk.toString("utf8").replace(/\0+$/, ""));
  if (type === 0x004e4942) binary = chunk;
  offset += 8 + length;
}
if (!json || !binary || json.extensionsRequired?.length)
  throw new Error("GLB must contain uncompressed JSON and BIN chunks");

const components = {
  5121: { bytes: 1, read: "readUInt8" }, 5123: { bytes: 2, read: "readUInt16LE" },
  5125: { bytes: 4, read: "readUInt32LE" }, 5126: { bytes: 4, read: "readFloatLE" },
};
const widths = { SCALAR: 1, VEC2: 2, VEC3: 3, VEC4: 4 };
function accessor(index) {
  const spec = json.accessors[index], view = json.bufferViews[spec?.bufferView];
  const kind = components[spec?.componentType], width = widths[spec?.type];
  if (!spec || !view || !kind || !width || spec.sparse)
    throw new Error(`unsupported accessor ${index}`);
  const stride = view.byteStride || kind.bytes * width;
  const start = (view.byteOffset || 0) + (spec.byteOffset || 0);
  return Array.from({ length: spec.count }, (_, row) => {
    const value = Array.from({ length: width }, (_, column) =>
      binary[kind.read](start + row * stride + column * kind.bytes));
    return width === 1 ? value[0] : value;
  });
}
async function decodedImage(textureIndex) {
  const texture = json.textures?.[textureIndex], image = json.images?.[texture?.source];
  const view = json.bufferViews?.[image?.bufferView];
  if (!view) return null;
  const encoded = binary.subarray(view.byteOffset || 0, (view.byteOffset || 0) + view.byteLength);
  const { data, info } = await sharp(encoded).removeAlpha().raw()
    .toBuffer({ resolveWithObject: true });
  return { data, width: info.width, height: info.height, channels: info.channels };
}
function texel(image, uv, fallback) {
  if (!image) return fallback;
  const wrap = (value) => ((value % 1) + 1) % 1;
  const x = Math.min(image.width - 1, Math.floor(wrap(uv[0]) * image.width));
  const y = Math.min(image.height - 1, Math.floor(wrap(uv[1]) * image.height));
  const offset = (y * image.width + x) * image.channels;
  return [image.data[offset], image.data[offset + 1], image.data[offset + 2]];
}

const primitive = json.meshes?.flatMap((mesh) => mesh.primitives || [])
  .find((item) => (item.mode ?? 4) === 4 && item.attributes?.POSITION !== undefined &&
    item.attributes?.TEXCOORD_0 !== undefined && item.indices !== undefined);
if (!primitive) throw new Error("GLB needs an indexed textured triangle primitive");
const positions = accessor(primitive.attributes.POSITION);
const uvs = accessor(primitive.attributes.TEXCOORD_0);
const indices = accessor(primitive.indices);
const minY = Math.min(...positions.map((point) => point[1]));
const maxY = Math.max(...positions.map((point) => point[1]));
const cutoff = minY + (maxY - minY) * verticalCut;
let selected = [];
for (let offset = 0; offset < indices.length; offset += 3) {
  const ids = [indices[offset], indices[offset + 1], indices[offset + 2]];
  if (ids.reduce((sum, id) => sum + positions[id][1], 0) / 3 >= cutoff) selected.push(ids);
}
if (selected.length < 100) throw new Error(`head selection produced only ${selected.length} triangles`);
let simplificationError = 0;
if (selected.length > targetTriangles) {
  await MeshoptSimplifier.ready;
  const sourceIndices = Uint32Array.from(selected.flat());
  const sourcePositions = Float32Array.from(positions.flat());
  const [indices, error] = MeshoptSimplifier.simplifySloppy(sourceIndices,
    sourcePositions, 3, null, targetTriangles * 3, .035);
  simplificationError = error;
  selected = Array.from({ length: indices.length / 3 }, (_, index) =>
    [indices[index * 3], indices[index * 3 + 1], indices[index * 3 + 2]]);
}
if (selected.length > 6500)
  throw new Error(`head simplification stopped at ${selected.length} triangles`);

const used = [...new Set(selected.flat())];
const remap = new Map(used.map((id, index) => [id, index]));
const points = used.map((id) => positions[id]);
const minimum = [0, 1, 2].map((axis) => Math.min(...points.map((point) => point[axis])));
const maximum = [0, 1, 2].map((axis) => Math.max(...points.map((point) => point[axis])));
const center = minimum.map((value, axis) => (value + maximum[axis]) / 2);
const normalization = 1 / Math.max(maximum[1] - minimum[1], 1e-9);
const round = (value, places = 4) => {
  const factor = 10 ** places;
  return Math.round(value * factor) / factor;
};
const vertices = points.map((point) => point.map((value, axis) =>
  round((value - center[axis]) * normalization)));

const material = json.materials?.[primitive.material || 0] || {};
const pbr = material.pbrMetallicRoughness || {};
const [baseImage, metalImage, emissiveImage] = await Promise.all([
  decodedImage(pbr.baseColorTexture?.index),
  decodedImage(pbr.metallicRoughnessTexture?.index),
  decodedImage(material.emissiveTexture?.index),
]);
const average = (image, ids, fallback) => [0, 1, 2].map((channel) => Math.round(
  ids.map((id) => texel(image, uvs[id], fallback)[channel])
    .reduce((sum, value) => sum + value, 0) / ids.length));
const faces = selected.map((ids) => {
  const base = average(baseImage, ids, [184, 150, 136]);
  const metalRough = average(metalImage, ids, [0, 180, 0]);
  const emissive = average(emissiveImage, ids, [0, 0, 0]);
  return [...ids.map((id) => remap.get(id)), ...base,
    round(metalRough[2] / 255, 3), round(metalRough[1] / 255, 3), ...emissive];
});

const output = {
  source: "Meshy v6 multi-image Jeffrey platter LOD",
  verticalCut, targetTriangles, simplificationError: round(simplificationError, 6),
  triangles: faces.length, vertices: vertices.length,
  bounds: { minimum: minimum.map(round), maximum: maximum.map(round) },
  points: vertices, faces,
};
writeFileSync(destination, JSON.stringify(output));
console.log(JSON.stringify({ destination, triangles: faces.length, vertices: vertices.length,
  bytes: Buffer.byteLength(JSON.stringify(output)) }));
