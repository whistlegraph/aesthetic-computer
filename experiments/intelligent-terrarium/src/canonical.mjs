import { createHash } from "node:crypto";
import { mkdir, rename, writeFile } from "node:fs/promises";
import { dirname } from "node:path";

function normalize(value) {
  if (value === null || typeof value === "string" || typeof value === "boolean") {
    return value;
  }
  if (typeof value === "number") {
    if (!Number.isFinite(value)) throw new TypeError("canonical JSON rejects non-finite numbers");
    return Object.is(value, -0) ? 0 : value;
  }
  if (Array.isArray(value)) return value.map(normalize);
  if (typeof value === "object") {
    return Object.fromEntries(
      Object.keys(value).sort().map((key) => [key, normalize(value[key])]),
    );
  }
  throw new TypeError(`canonical JSON rejects ${typeof value}`);
}

export function canonical(value) {
  return JSON.stringify(normalize(value));
}

export function hash(value) {
  const bytes = typeof value === "string" ? value : canonical(value);
  return createHash("sha256").update(bytes).digest("hex");
}

export function clone(value) {
  return JSON.parse(canonical(value));
}

export async function atomicWrite(path, contents) {
  await mkdir(dirname(path), { recursive: true });
  const temp = `${path}.tmp-${process.pid}`;
  await writeFile(temp, contents, { mode: 0o600 });
  await rename(temp, path);
}
