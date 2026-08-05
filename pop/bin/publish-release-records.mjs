#!/usr/bin/env node

import { mkdir, readFile, readdir, writeFile } from "node:fs/promises";
import { dirname, extname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const ROOT = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const MANIFESTS = join(ROOT, "system/public/aesthetic.computer/disks/pop");
const RELEASES = join(ROOT, "pop/releases");
const OUTPUT = join(ROOT, "system/public/pop.aesthetic.computer/releases");

function assetUrl(value) {
  if (!value) return null;
  return value.startsWith("/assets/")
    ? `https://assets.aesthetic.computer${value.slice("/assets".length)}`
    : value;
}

function publicAsset(asset = {}) {
  const { localPath: _localPath, packetUri: _packetUri, md5: _md5, ...safe } = asset;
  return { ...safe, publicUrl: assetUrl(safe.publicUrl) };
}

export function publicProjection(record) {
  return {
    schemaVersion: record.schemaVersion,
    slug: record.slug,
    ddex: record.ddex,
    release: record.release,
    recording: record.recording,
    artwork: record.artwork,
    assets: {
      audio: publicAsset(record.assets?.audio),
      cover: publicAsset(record.assets?.cover),
    },
    links: record.links ?? {},
    delivery: {
      state: "evaluation",
      messageControlType: "TestMessage",
      transmitted: false,
    },
  };
}

async function json(path) {
  return JSON.parse(await readFile(path, "utf8"));
}

async function releaseRecords() {
  const records = new Map();
  for (const entry of await readdir(RELEASES, { withFileTypes: true })) {
    if (!entry.isDirectory()) continue;
    const path = join(RELEASES, entry.name, "release.json");
    try {
      const record = publicProjection(await json(path));
      records.set(record.slug, record);
    } catch (error) {
      if (error?.code !== "ENOENT") throw error;
    }
  }
  return records;
}

export async function buildPopCatalog({ output = OUTPUT } = {}) {
  const records = await releaseRecords();
  const tracks = [];
  const files = (await readdir(MANIFESTS)).filter((file) => extname(file) === ".json").sort();
  for (const file of files) {
    const manifest = await json(join(MANIFESTS, file));
    tracks.push({
      slug: manifest.slug,
      title: manifest.title,
      artist: manifest.artist,
      album: manifest.album,
      bpm: manifest.bpm,
      key: manifest.key,
      meter: manifest.meter,
      duration: manifest.duration,
      cover: assetUrl(manifest.cover),
      audio: assetUrl(manifest.audio),
      piece: `https://aesthetic.computer/${manifest.slug}`,
      links: manifest.links ?? {},
      credits: manifest.credits,
      releaseData: records.has(manifest.slug) ? `/releases/${manifest.slug}` : null,
    });
  }

  await mkdir(output, { recursive: true });
  for (const record of records.values()) {
    await writeFile(join(output, `${record.slug}.json`), `${JSON.stringify(record, null, 2)}\n`);
  }
  const catalog = {
    schemaVersion: 1,
    artist: "Aesthetic Dot Computer",
    tracks,
  };
  await writeFile(join(output, "catalog.json"), `${JSON.stringify(catalog, null, 2)}\n`);
  return catalog;
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const catalog = await buildPopCatalog();
  console.log(`Published ${catalog.tracks.length} tracks to ${OUTPUT}`);
}
