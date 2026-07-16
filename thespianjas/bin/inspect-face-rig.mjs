#!/usr/bin/env node
// Structural gate: a talking-head GLB must expose facial morphs and separate eyes.

import { readFileSync } from "node:fs";
import { resolve } from "node:path";

const path = resolve(process.argv[2] || "thespianjas/assets/versions/v002/head.glb");
const bytes = readFileSync(path);
if (bytes.toString("ascii", 0, 4) !== "glTF") throw new Error("not a binary glTF");
const jsonLength = bytes.readUInt32LE(12);
const gltf = JSON.parse(bytes.subarray(20, 20 + jsonLength).toString().replace(/\0+$/, ""));
const names = (gltf.nodes || []).map((node) => node.name || "");
const morphs = new Set((gltf.meshes || []).flatMap((mesh) => Object.keys(mesh.extras?.targetNames || {}).length
  ? Object.values(mesh.extras.targetNames)
  : (mesh.extras?.targetNames || [])));
for (const mesh of gltf.meshes || []) for (const name of mesh.extras?.targetNames || []) morphs.add(name);
const required = ["jawOpen", "eyeBlinkLeft", "eyeBlinkRight", "eyeLookInLeft", "eyeLookInRight", "eyeLookOutLeft", "eyeLookOutRight"];
const missingMorphs = required.filter((name) => !morphs.has(name));
const leftEye = names.some((name) => /left.*eye|eye.*left|eyeball.*l$/i.test(name));
const rightEye = names.some((name) => /right.*eye|eye.*right|eyeball.*r$/i.test(name));
const report = { pass: !missingMorphs.length && leftEye && rightEye, path, morphTargetCount: morphs.size, missingMorphs, independentEyes: { left: leftEye, right: rightEye } };
console.log(JSON.stringify(report, null, 2));
process.exit(report.pass ? 0 : 1);

