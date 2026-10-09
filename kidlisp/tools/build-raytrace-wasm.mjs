#!/usr/bin/env node
// Reproducible benchmark host build. Requires clang + wasm-ld (LLVM 22 tested).
import { readFile, writeFile, mkdir, mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { resolve, join, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import { rayKernel } from "../benchmarks/raytrace.mjs";
import { emitRayPlan } from "../benchmarks/ray-plan.mjs";
const base = fileURLToPath(new URL("../benchmarks/", import.meta.url));
const source = await readFile(join(base, "ray-sphere.lisp"), "utf8");
const host = await readFile(join(base, "raytrace-frame.c"));
const header = emitRayPlan(rayKernel(source).plan, "c");
const out = resolve(process.argv[2] || join(base, "raytrace-frame.wasm"));
const clang = process.env.KIDLISP_CLANG || "/opt/homebrew/opt/llvm@22/bin/clang";
const linker = process.env.KIDLISP_WASM_LD || "/opt/homebrew/opt/lld@22/bin/wasm-ld";
const scratch = await mkdtemp(join(tmpdir(), "kidlisp-ray-build-"));
try {
  await mkdir(dirname(out), { recursive: true });
  await writeFile(join(scratch, "ray-kernel.h"), header);
  const args = ["--target=wasm32", "-O3", "-fno-fast-math", "-ffp-contract=off", "-fno-builtin", "-nostdlib", "-I", scratch, "-c", join(base, "raytrace-frame.c"), "-o", join(scratch, "ray.o")];
  execFileSync(clang, args, { stdio: "pipe" });
  execFileSync(linker, ["--no-entry", "--export-memory", "--initial-memory=1179648", "--max-memory=1179648", "--strip-all", join(scratch, "ray.o"), "-o", out], { stdio: "pipe" });
  const bytes = await readFile(out);
  const module = new WebAssembly.Module(bytes);
  if (WebAssembly.Module.imports(module).length) throw new Error("Frame renderer must not import any host functions");
  const hash = value => createHash("sha256").update(value).digest("hex");
  await writeFile(`${out}.json`, JSON.stringify({ version: 1, profile: "ray-frame-f64-v1", sha256: hash(bytes), bytes: bytes.length, sourceSha256: hash(source), hostSha256: hash(host), kernelSha256: hash(header), compiler: execFileSync(clang, ["--version"], { encoding: "utf8" }).split("\n")[0], flags: args.slice(0, 6), memoryBytes: 1179648, imports: [] }, null, 2) + "\n");
  console.log(`${out}: ${bytes.length} bytes; no host imports`);
} finally { await rm(scratch, { recursive: true, force: true }); }
