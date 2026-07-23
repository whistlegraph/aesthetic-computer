#!/usr/bin/env node
// Derive a compact run cycle from an existing baked humanoid skin. This keeps
// the original triangle/UV topology and uses its real 24-joint hierarchy;
// nearest-joint blending turns the low-LOD idle body into a distinct run clip.

import { readFileSync, writeFileSync } from "node:fs";

const [source, destination = source, requestedFrames = "20"] = process.argv.slice(2);
if (!source) throw new Error("usage: procedural-run-bake.mjs <idle-bake.json> [output.json] [frames]");
const sourceText = readFileSync(source, "utf8");
const embedded = sourceText.match(/^const jeffrey=(.*);$/m);
const bake = JSON.parse(embedded ? embedded[1] : sourceText);
if (!bake.frames?.[0] || !bake.jointFrames?.[0] || !bake.bones?.length)
  throw new Error("bake needs frames, jointFrames, and bones");
const frameCount = Math.max(12, Math.min(40, Number.parseInt(requestedFrames, 10) || 20));
const vertices = bake.frames[0], joints = bake.jointFrames[0];
const parents = Array(joints.length).fill(-1);
for (const [parent, child] of bake.bones) parents[child] = parent;

const influence = vertices.map((vertex) => joints.map((joint, index) => ({
  index,
  distance: Math.hypot(vertex[0] - joint[0], vertex[1] - joint[1], vertex[2] - joint[2]),
})).sort((left, right) => left.distance - right.distance).slice(0, 3).map((item) => ({
  index: item.index,
  weight: 1 / Math.pow(item.distance + .045, 3),
})));
for (const weights of influence) {
  const total = weights.reduce((sum, item) => sum + item.weight, 0);
  for (const item of weights) item.weight /= total;
}
const rotate = (point, angle) => {
  const cosine = Math.cos(angle), sine = Math.sin(angle);
  return [point[0] * cosine - point[1] * sine,
    point[0] * sine + point[1] * cosine, point[2]];
};
const round = (value) => Math.round(value * 10000) / 10000;

function pose(phase) {
  const wave = Math.sin(phase), other = Math.sin(phase + Math.PI);
  const angles = Array(joints.length).fill(0);
  // Mixamo-like hierarchy in the source bake: 0 pelvis; 1/5 hips; 2/6 knees;
  // 12/16 shoulders; 13/17 elbows; 9-11 spine; 20-22 neck/head.
  angles[1] = wave * .5; angles[5] = other * .5;
  angles[2] = .15 + Math.max(0, -wave) * .72;
  angles[6] = .15 + Math.max(0, -other) * .72;
  angles[3] = -.1 - Math.max(0, -wave) * .28;
  angles[7] = -.1 - Math.max(0, -other) * .28;
  angles[12] = other * .62; angles[16] = wave * .62;
  angles[13] = -.22 - Math.max(0, wave) * .34;
  angles[17] = .22 + Math.max(0, other) * .34;
  angles[9] = -wave * .035; angles[10] = wave * .025;
  angles[11] = -wave * .02; angles[20] = wave * .018;

  const posed = Array(joints.length), cumulative = Array(joints.length).fill(0);
  posed[0] = [joints[0][0] + Math.sin(phase * 2) * .012,
    joints[0][1] + Math.abs(wave) * .035, joints[0][2]];
  cumulative[0] = -.055;
  for (let index = 1; index < joints.length; index++) {
    const parent = parents[index];
    if (parent < 0 || !posed[parent]) {
      posed[index] = [...joints[index]];
      continue;
    }
    const local = joints[index].map((value, axis) => value - joints[parent][axis]);
    const offset = rotate(local, cumulative[parent]);
    posed[index] = posed[parent].map((value, axis) => value + offset[axis]);
    cumulative[index] = cumulative[parent] + angles[index];
  }
  const transformed = vertices.map((vertex, vertexIndex) => {
    const result = [0, 0, 0];
    for (const item of influence[vertexIndex]) {
      const joint = item.index;
      const local = vertex.map((value, axis) => value - joints[joint][axis]);
      const offset = rotate(local, cumulative[joint]);
      for (let axis = 0; axis < 3; axis++)
        result[axis] += (posed[joint][axis] + offset[axis]) * item.weight;
    }
    return result.map(round);
  });
  return { vertices: transformed, joints: posed.map((point) => point.map(round)) };
}

const poses = Array.from({ length: frameCount }, (_, frame) =>
  pose(frame / frameCount * Math.PI * 2));
bake.runClip = "procedural-run";
bake.runDuration = .72;
bake.runFrameCount = frameCount;
bake.runFrames = poses.map((item) => item.vertices);
bake.runJointFrames = poses.map((item) => item.joints);
writeFileSync(destination, JSON.stringify(bake));
console.log(JSON.stringify({ destination, runFrames: frameCount,
  vertices: vertices.length, joints: joints.length,
  bytes: Buffer.byteLength(JSON.stringify(bake)) }));
