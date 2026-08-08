import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { existsSync, readFileSync } from "node:fs";
import { describe, it } from "node:test";
import { resolve } from "node:path";
import { CASSETTE_SPECS, CD_SPECS } from "../marketing/podcast/lib/kunaki.mjs";

// The kits are large build products, so these run only once they exist.
const cd = resolve(import.meta.dirname, "../pop/physical/out/pixsies-so-far-kunaki-cd");
const cassette = resolve(import.meta.dirname, "../pop/physical/out/pixsies-so-far-kunaki-cassette");
const built = (dir) => existsSync(resolve(dir, "manifest.json"));
const read = (dir) => JSON.parse(readFileSync(resolve(dir, "manifest.json"), "utf8"));

const probe = (path, entries) => execFileSync("ffprobe", [
  "-v", "error", "-show_entries", entries, "-of", "default=nw=1:nk=1", path,
], { encoding: "utf8" }).trim().split("\n");

const image = (path) => {
  const [width, height, x, y, units] = execFileSync("magick", [
    "identify", "-format", "%w %h %x %y %U", path,
  ], { encoding: "utf8" }).trim().split(" ");
  return { width: Number(width), height: Number(height), x: Number(x), y: Number(y), units };
};

describe("physical CD kit", { skip: built(cd) ? false : "kit not built" }, () => {
  const manifest = read(cd);

  it("stays inside Kunaki's program limits", () => {
    assert.ok(manifest.program.trackCount <= CD_SPECS.audio.maxTracks);
    assert.ok(manifest.program.totalSeconds <= CD_SPECS.audio.maxMinutes * 60);
  });

  it("presses Red Book audio", () => {
    for (const track of manifest.program.tracks) {
      const [codec, rate, channels] = probe(resolve(cd, track.file), "stream=codec_name,sample_rate,channels");
      assert.equal(codec, "pcm_s16le", `${track.slug} is ${codec}`);
      assert.equal(rate, "44100", `${track.slug} runs at ${rate}`);
      assert.equal(channels, "2", `${track.slug} has ${channels} channel(s)`);
    }
  });

  it("declares which tracks came from a lossy source", () => {
    const lossy = manifest.program.tracks.filter((track) => track.lossy).map((track) => track.slug);
    assert.deepEqual(lossy, ["trancenwaltz", "trancepenta"]);
  });

  it("cuts artwork to the exact part sizes at 300 DPI", () => {
    const parts = {
      [manifest.artwork.front]: CD_SPECS.artwork.frontCover,
      [manifest.artwork.insert]: CD_SPECS.artwork.insert,
      [manifest.artwork.tray]: CD_SPECS.artwork.trayCard,
      [manifest.artwork.disc]: CD_SPECS.artwork.disc,
    };
    for (const [file, spec] of Object.entries(parts)) {
      const art = image(resolve(cd, file));
      assert.equal(art.width, spec.width, `${file} width`);
      assert.equal(art.height, spec.height, `${file} height`);
      assert.equal(art.x, CD_SPECS.artwork.dpi, `${file} density`);
      assert.equal(art.units, "PixelsPerInch", `${file} units`);
    }
  });
});

describe("physical cassette kit", { skip: built(cassette) ? false : "kit not built" }, () => {
  const manifest = read(cassette);

  it("keeps both sides under the tape's limit", () => {
    assert.equal(manifest.program.sides.length, 2);
    for (const side of manifest.program.sides) {
      assert.ok(side.durationSeconds <= CASSETTE_SPECS.audio.maxMinutesPerSide * 60,
        `side ${side.letter} runs ${side.displayDuration}`);
    }
  });

  it("carries every track across the two sides exactly once", () => {
    const positions = manifest.program.sides.flatMap((side) => side.tracks.map((track) => track.position));
    assert.deepEqual([...positions].sort((a, b) => a - b), positions);
    assert.equal(new Set(positions).size, manifest.program.trackCount);
  });

  it("cuts artwork to the exact part sizes", () => {
    const parts = {
      [manifest.artwork.jCard]: CASSETTE_SPECS.artwork.jCard,
      [manifest.artwork.labelA]: CASSETTE_SPECS.artwork.labelA,
      [manifest.artwork.labelB]: CASSETTE_SPECS.artwork.labelB,
    };
    for (const [file, spec] of Object.entries(parts)) {
      const art = image(resolve(cassette, file));
      assert.equal(art.width, spec.width, `${file} width`);
      assert.equal(art.height, spec.height, `${file} height`);
    }
  });
});
