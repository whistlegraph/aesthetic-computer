#!/usr/bin/env node
// Bake a bounded glTF skin animation into interpolable vertex frames for the
// sandboxed Xbox JavaScript scene. The source GLB keeps its real joints,
// weights, inverse bind matrices, and animation channels; the bake avoids doing
// thousands of matrix operations in QuickJS every display frame.

import { readFileSync, writeFileSync } from "node:fs";
import sharp from "sharp";

const [source, destination, requestedFrames = "24", textureDestination,
  requestedTextureSize = "1024"] = process.argv.slice(2);
const frameCount = Math.max(8, Math.min(60, Number.parseInt(requestedFrames, 10) || 24));
const textureSize = Math.max(256, Math.min(2048,
  Number.parseInt(requestedTextureSize, 10) || 1024));
if (!source || !destination) {
  throw new Error("usage: glb-skin-bake.mjs <animated.glb> <bake.json> [frames] [texture.rgba]");
}

function readGlb(path) {
  const bytes = readFileSync(path);
  if (bytes.toString("ascii", 0, 4) !== "glTF" || bytes.readUInt32LE(4) !== 2)
    throw new Error("expected a glTF 2.0 binary file");
  let json, binary;
  for (let offset = 12; offset + 8 <= bytes.length;) {
    const length = bytes.readUInt32LE(offset);
    const type = bytes.readUInt32LE(offset + 4);
    const chunk = bytes.subarray(offset + 8, offset + 8 + length);
    if (type === 0x4e4f534a) json = JSON.parse(chunk.toString("utf8").replace(/\0+$/, ""));
    if (type === 0x004e4942) binary = chunk;
    offset += 8 + length;
  }
  if (!json || !binary || json.extensionsRequired?.length)
    throw new Error("GLB must contain uncompressed JSON and BIN chunks");
  return { json, binary };
}

const document = readGlb(source);
const { json, binary } = document;
const components = {
  5120: { bytes: 1, read: "readInt8" },
  5121: { bytes: 1, read: "readUInt8" },
  5122: { bytes: 2, read: "readInt16LE" },
  5123: { bytes: 2, read: "readUInt16LE" },
  5125: { bytes: 4, read: "readUInt32LE" },
  5126: { bytes: 4, read: "readFloatLE" },
};
const widths = { SCALAR: 1, VEC2: 2, VEC3: 3, VEC4: 4, MAT4: 16 };

function accessor(index) {
  const spec = json.accessors[index];
  const view = json.bufferViews[spec?.bufferView];
  const kind = components[spec?.componentType];
  const width = widths[spec?.type];
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

const identity = () => [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1];
function multiply(a, b) {
  const out = Array(16).fill(0);
  for (let column = 0; column < 4; column++)
    for (let row = 0; row < 4; row++)
      for (let k = 0; k < 4; k++) out[column * 4 + row] += a[k * 4 + row] * b[column * 4 + k];
  return out;
}
function trs(translation, rotation, scale) {
  const [x, y, z, w] = rotation, [sx, sy, sz] = scale;
  const x2 = x + x, y2 = y + y, z2 = z + z;
  const xx = x * x2, xy = x * y2, xz = x * z2;
  const yy = y * y2, yz = y * z2, zz = z * z2;
  const wx = w * x2, wy = w * y2, wz = w * z2;
  return [
    (1 - (yy + zz)) * sx, (xy + wz) * sx, (xz - wy) * sx, 0,
    (xy - wz) * sy, (1 - (xx + zz)) * sy, (yz + wx) * sy, 0,
    (xz + wy) * sz, (yz - wx) * sz, (1 - (xx + yy)) * sz, 0,
    translation[0], translation[1], translation[2], 1,
  ];
}
function transform(matrix, point) {
  const [x, y, z] = point;
  return [
    matrix[0] * x + matrix[4] * y + matrix[8] * z + matrix[12],
    matrix[1] * x + matrix[5] * y + matrix[9] * z + matrix[13],
    matrix[2] * x + matrix[6] * y + matrix[10] * z + matrix[14],
  ];
}
function mix(a, b, amount) { return a.map((value, index) => value + (b[index] - value) * amount); }
function slerp(a, b, amount) {
  let cosine = a.reduce((sum, value, index) => sum + value * b[index], 0);
  if (cosine < 0) { b = b.map((value) => -value); cosine = -cosine; }
  if (cosine > .9995) {
    const result = mix(a, b, amount), length = Math.hypot(...result) || 1;
    return result.map((value) => value / length);
  }
  const theta = Math.acos(Math.max(-1, Math.min(1, cosine)));
  const sine = Math.sin(theta) || 1;
  const left = Math.sin((1 - amount) * theta) / sine, right = Math.sin(amount * theta) / sine;
  return a.map((value, index) => value * left + b[index] * right);
}

const meshNodeIndex = json.nodes.findIndex((node) => node.mesh !== undefined && node.skin !== undefined);
if (meshNodeIndex < 0) throw new Error("animated GLB needs a skinned mesh node");
const meshNode = json.nodes[meshNodeIndex];
const primitive = json.meshes[meshNode.mesh]?.primitives?.find((item) => (item.mode ?? 4) === 4);
for (const name of ["POSITION", "TEXCOORD_0", "JOINTS_0", "WEIGHTS_0"])
  if (primitive?.attributes?.[name] === undefined) throw new Error(`missing ${name}`);
if (primitive.indices === undefined) throw new Error("skinned mesh must be indexed");

const positions = accessor(primitive.attributes.POSITION);
const uvs = accessor(primitive.attributes.TEXCOORD_0);
const joints = accessor(primitive.attributes.JOINTS_0);
const weights = accessor(primitive.attributes.WEIGHTS_0);
const indices = accessor(primitive.indices);
if (indices.length % 3 || indices.length / 3 > 2400)
  throw new Error("skin bake requires 1-2400 triangles; simplify the source first");

const skin = json.skins[meshNode.skin];
const inverseBinds = accessor(skin.inverseBindMatrices);
if (inverseBinds.length !== skin.joints.length) throw new Error("joint/inverse bind mismatch");

const parents = Array(json.nodes.length).fill(-1);
json.nodes.forEach((node, index) => node.children?.forEach((child) => { parents[child] = index; }));
const base = json.nodes.map((node) => ({
  translation: node.translation || [0, 0, 0],
  rotation: node.rotation || [0, 0, 0, 1],
  scale: node.scale || [1, 1, 1],
  matrix: node.matrix || null,
}));

const animation = json.animations?.[0];
if (!animation?.channels?.length) throw new Error("animated GLB has no animation clip");
const channels = animation.channels.map((channel) => {
  const sampler = animation.samplers[channel.sampler];
  return {
    node: channel.target.node,
    path: channel.target.path,
    times: accessor(sampler.input),
    values: accessor(sampler.output),
    interpolation: sampler.interpolation || "LINEAR",
  };
});
const duration = Math.max(...channels.flatMap((channel) => channel.times));

function sample(channel, time) {
  const times = channel.times;
  let right = times.findIndex((value) => value > time);
  if (right < 0) right = times.length - 1;
  const left = Math.max(0, right - 1);
  if (left === right || channel.interpolation === "STEP") return channel.values[left];
  const amount = (time - times[left]) / Math.max(1e-9, times[right] - times[left]);
  return channel.path === "rotation"
    ? slerp(channel.values[left], channel.values[right], amount)
    : mix(channel.values[left], channel.values[right], amount);
}

function pose(time) {
  const state = base.map((node) => ({
    translation: [...node.translation], rotation: [...node.rotation], scale: [...node.scale], matrix: node.matrix,
  }));
  for (const channel of channels) state[channel.node][channel.path] = sample(channel, time);
  const world = Array(json.nodes.length);
  const solve = (index) => {
    if (world[index]) return world[index];
    const node = state[index];
    const local = node.matrix || trs(node.translation, node.rotation, node.scale);
    return world[index] = parents[index] < 0 ? local : multiply(solve(parents[index]), local);
  };
  json.nodes.forEach((_, index) => solve(index));
  const matrices = skin.joints.map((joint, index) => multiply(world[joint], inverseBinds[index]));
  const vertices = positions.map((position, vertex) => {
    const result = [0, 0, 0];
    let total = 0;
    for (let influence = 0; influence < 4; influence++) {
      const weight = weights[vertex][influence];
      if (weight <= 0) continue;
      const point = transform(matrices[joints[vertex][influence]], position);
      result[0] += point[0] * weight; result[1] += point[1] * weight; result[2] += point[2] * weight;
      total += weight;
    }
    if (total > 0 && Math.abs(total - 1) > .0001) return result.map((value) => value / total);
    return result;
  });
  const jointPoints = skin.joints.map((joint) => transform(world[joint], [0, 0, 0]));
  return { vertices, jointPoints };
}

async function decodedBaseImage() {
  const material = json.materials?.[primitive.material || 0] || {};
  const textureIndex = material.pbrMetallicRoughness?.baseColorTexture?.index;
  const texture = json.textures?.[textureIndex];
  const image = json.images?.[texture?.source];
  const view = json.bufferViews?.[image?.bufferView];
  if (!view) return null;
  const encoded = binary.subarray(view.byteOffset || 0, (view.byteOffset || 0) + view.byteLength);
  if (textureDestination) {
    const texture = await sharp(encoded).resize(textureSize, textureSize, { fit: "fill" })
      .ensureAlpha().raw().toBuffer();
    writeFileSync(textureDestination, texture);
  }
  const { data, info } = await sharp(encoded).removeAlpha().raw().toBuffer({ resolveWithObject: true });
  return { data, width: info.width, height: info.height, channels: info.channels };
}
function texel(image, uv) {
  if (!image) return [170, 155, 190];
  const wrap = (value) => ((value % 1) + 1) % 1;
  const x = Math.min(image.width - 1, Math.floor(wrap(uv[0]) * image.width));
  // glTF and the D3D texture path both address the decoded image from its top
  // row. Flipping V here (and again in the emitted UVs) mapped Jeffrey's face
  // onto unrelated shirt and trouser islands in the atlas.
  const y = Math.min(image.height - 1, Math.floor(wrap(uv[1]) * image.height));
  const offset = (y * image.width + x) * image.channels;
  return [image.data[offset], image.data[offset + 1], image.data[offset + 2]];
}

const sampled = Array.from({ length: frameCount }, (_, frame) => pose(duration * frame / frameCount));
const initial = sampled[0].vertices;
const min = [0, 1, 2].map((axis) => Math.min(...initial.map((point) => point[axis])));
const max = [0, 1, 2].map((axis) => Math.max(...initial.map((point) => point[axis])));
const center = min.map((value, axis) => (value + max[axis]) / 2);
const normalization = 2.25 / Math.max(1e-9, max[1] - min[1]);
const round = (value, places = 4) => {
  const factor = 10 ** places;
  return Math.round(value * factor) / factor;
};
const normalize = (point) => point.map((value, axis) => round((value - center[axis]) * normalization));
const frames = sampled.map((frame) => frame.vertices.map(normalize));
const jointFrames = sampled.map((frame) => frame.jointPoints.map(normalize));

const image = await decodedBaseImage();
const faces = [];
for (let offset = 0; offset < indices.length; offset += 3) {
  const ids = [indices[offset], indices[offset + 1], indices[offset + 2]];
  const samples = ids.map((id) => texel(image, uvs[id]));
  const color = [0, 1, 2].map((channel) => Math.round(
    samples.reduce((sum, sample) => sum + sample[channel], 0) / samples.length));
  faces.push([...ids, ...color]);
}
const jointLookup = new Map(skin.joints.map((node, index) => [node, index]));
const bones = skin.joints.flatMap((node, index) => {
  const parent = jointLookup.get(parents[node]);
  return parent === undefined ? [] : [[parent, index]];
});

const output = {
  source: "thespianjas/v001/idle.glb",
  clip: animation.name || "idle",
  duration: round(duration, 5), frameCount, vertices: positions.length,
  triangles: indices.length / 3, joints: skin.joints.length,
  frames, jointFrames, faces, bones,
  uvs: uvs.map((uv) => [round(uv[0], 5), round(uv[1], 5)]),
  texture: textureDestination ? { width: textureSize, height: textureSize,
    filter: "linear" } : null,
};
writeFileSync(destination, JSON.stringify(output));
console.log(JSON.stringify({ destination, bytes: Buffer.byteLength(JSON.stringify(output)),
  frames: frameCount, vertices: positions.length, triangles: indices.length / 3,
  joints: skin.joints.length, bones: bones.length, duration: output.duration,
  texture: textureDestination || null }));
