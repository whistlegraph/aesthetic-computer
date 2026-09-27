// fetch-polyhaven.mjs — pull CC0 Poly Haven assets by id through their public
// API, cached on disk, so a scene render never needs a browser or a GUI.
//
//   import { fetchHdri, fetchModel } from "./fetch-polyhaven.mjs";
//   const hdr = await fetchHdri("lythwood_room", "2k", cacheDir);
//   const gltf = await fetchModel("WoodenTable_01", "2k", cacheDir);
//
// Everything on Poly Haven is CC0 (https://polyhaven.com/license).

import { existsSync, mkdirSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";

const API = "https://api.polyhaven.com/files";

async function json(url) {
  const res = await fetch(url);
  if (!res.ok) throw new Error(`${res.status} ${url}`);
  return res.json();
}

async function download(url, out) {
  if (existsSync(out)) return out;
  mkdirSync(dirname(out), { recursive: true });
  const res = await fetch(url);
  if (!res.ok) throw new Error(`${res.status} ${url}`);
  writeFileSync(out, Buffer.from(await res.arrayBuffer()));
  return out;
}

export async function fetchHdri(id, res, cacheDir) {
  const files = await json(`${API}/${id}`);
  const hdr = files.hdri?.[res]?.hdr;
  if (!hdr) throw new Error(`no ${res} hdr for ${id}`);
  return download(hdr.url, join(cacheDir, "hdri", `${id}_${res}.hdr`));
}

// glTF plus every texture it references, kept at their relative paths.
export async function fetchModel(id, res, cacheDir) {
  const files = await json(`${API}/${id}`);
  // Not every model ships every resolution: take the asked one, else the
  // nearest that exists.
  const order = [res, "1k", "2k", "4k", ...Object.keys(files.gltf || {})];
  const got = order.find((r) => files.gltf?.[r]?.gltf);
  const gltf = got && files.gltf[got].gltf;
  if (!gltf) throw new Error(`no gltf for ${id}`);
  const dir = join(cacheDir, "models", id);
  const main = await download(gltf.url, join(dir, gltf.url.split("/").pop()));
  for (const [rel, file] of Object.entries(gltf.include || {})) await download(file.url, join(dir, rel));
  return main;
}
