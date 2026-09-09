import test from "node:test";
import assert from "node:assert/strict";

import { countEvidenceFigures, validateManifest } from "../aesthetic-eye.mjs";

const baseManifest = {
  schema: 1,
  pdf: "paper.pdf",
  pdfSha256: "current-hash",
  visualInference: true,
  reviewedAt: "2026-09-09T00:00:00Z",
  reviewer: { kind: "visual-inference", agent: "test" },
  brand: {
    canonicalName: "Aesthetic.Computer",
    dotColor: "#B44887",
    design: "pass",
    checks: { period: "pass", dotColor: "pass" },
  },
  expectedDiagrams: 0,
  diagrams: [],
};

const passingFigure = {
  id: "result",
  page: 1,
  crop: [0.1, 0.2, 0.8, 0.4],
  design: "pass",
  checks: {
    scale: "pass",
    legibility: "pass",
    evidenceDominance: "pass",
    crop: "pass",
    captionFit: "pass",
  },
};

test("counts image-backed evidence figures but not a TikZ diagram", () => {
  const source = String.raw`
    \begin{figure}\includegraphics{evidence}\end{figure}
    \begin{figure*}\begin{tikzpicture}\end{tikzpicture}\end{figure*}`;
  assert.equal(countEvidenceFigures(source), 1);
});

test("fails when source evidence is absent from the Aesthetic Eye manifest", () => {
  const verdict = validateManifest(baseManifest, "current-hash", 1);
  assert.equal(verdict.pass, false);
  assert.match(verdict.errors.join("\n"), /expectedFigures/);
});

test("passes a fully reviewed evidence figure", () => {
  const manifest = { ...baseManifest, expectedFigures: 1, figures: [passingFigure] };
  assert.equal(validateManifest(manifest, "current-hash", 1).pass, true);
});

test("fails a too-small evidence verdict", () => {
  const figure = {
    ...passingFigure,
    checks: { ...passingFigure.checks, scale: "fail" },
  };
  const manifest = { ...baseManifest, expectedFigures: 1, figures: [figure] };
  const verdict = validateManifest(manifest, "current-hash", 1);
  assert.equal(verdict.pass, false);
  assert.match(verdict.errors.join("\n"), /scale/);
});
