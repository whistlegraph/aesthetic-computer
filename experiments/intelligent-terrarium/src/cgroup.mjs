import { readFile } from "node:fs/promises";
import { join } from "node:path";

async function readValue(path) {
  try {
    const value = (await readFile(path, "utf8")).trim();
    return value === "max" ? "max" : Number(value);
  } catch {
    return null;
  }
}

export async function memoryCgroup() {
  if (process.platform !== "linux") return null;
  const cgroup = await readFile("/proc/self/cgroup", "utf8");
  const unified = cgroup.split("\n").find((line) => line.startsWith("0::"));
  if (!unified) return null;
  const relative = unified.slice(3);
  const root = join("/sys/fs/cgroup", relative);
  return {
    path: relative,
    current: await readValue(join(root, "memory.current")),
    peak: await readValue(join(root, "memory.peak")),
    high: await readValue(join(root, "memory.high")),
    max: await readValue(join(root, "memory.max")),
    swapMax: await readValue(join(root, "memory.swap.max")),
  };
}

export async function requireMemoryMax(limit) {
  const memory = await memoryCgroup();
  if (!memory || memory.max === "max" || !Number.isFinite(memory.max) || memory.max > limit) {
    throw new Error(`required cgroup memory.max <= ${limit}; observed ${memory?.max ?? "unavailable"}`);
  }
  return memory;
}
