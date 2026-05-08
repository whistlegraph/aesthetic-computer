// jeffrey-refs.mjs — shared SHOOT + SELFIE reference photos for jeffrey
// identity grounding. Used by recap/, marketing/, and any other pipeline
// that calls OpenAI gpt-image-2 /v1/images/edits with jeffrey in the scene.
//
// Mirrors what recap/bin/jeffrey-photos.mjs and recap/bin/gen-photos.mjs
// hardcode today. Those scripts can be migrated to import from here in a
// follow-up cleanup pass.

import { existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "..", "..");

const SHOOT_DIR = `${REPO}/portraits/jeffrey/corpus/shoot`;
const ARCHIVE_DIR = `${REPO}/portraits/jeffrey/ig-archive/whistlegraph`;

export const SHOOT_REFS = [
  `${SHOOT_DIR}/jeffery-av--07.jpg`,
  `${SHOOT_DIR}/jeffery-av--01.jpg`,
  `${SHOOT_DIR}/jeffery-av--04.jpg`,
];

export const SELFIE_REFS = [
  `${ARCHIVE_DIR}/2018-12-02_Bq4ckGFFNtW.jpg`,
  `${ARCHIVE_DIR}/2020-09-02_CEpxlO2FOvD.jpg`,
  `${ARCHIVE_DIR}/2021-07-10_CRI095Vl7AO_1.jpg`,
  `${ARCHIVE_DIR}/2025-01-25_DFQ2lHPzN_W.jpg`,
  `${ARCHIVE_DIR}/2017-04-10_BStid5yjTHq.jpg`,
];

export function jeffreyRefs() {
  return [...SHOOT_REFS, ...SELFIE_REFS].filter((p) => {
    if (existsSync(p)) return true;
    console.warn(`  ⚠ ref missing, dropping: ${p}`);
    return false;
  });
}
