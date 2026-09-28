import assert from "node:assert/strict";
import { mkdtemp, readFile, rm, symlink, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { create, run, validateTex } from "../src/media/paper.mjs";
import { sourceZip } from "../media/paper/zip.mjs";
async function paper(t) {
  const root = await mkdtemp(join(tmpdir(), "paper-test-"));
  t.after(() => rm(root, { recursive: true, force: true }));
  await create({ root, name: "Test paper" });
  return root;
}
const manuscript = { title: "A test", author: "Explicit Author", abstract: "A minimal test.", sections: [{ title: "Method", text: "Test text." }] };
test("paper scaffold has no inherited author, and builds require an explicit identity", async (t) => {
  const root = await paper(t);
  const source = await readFile(join(root, "manuscript.tex"), "utf8");
  assert.match(source, /Author not set/); assert.doesNotMatch(source, /@jeffrey/);
  await assert.rejects(run({ root, action: "build" }), /explicit author/);
  const result = await run({ root, action: "write", input: manuscript });
  assert.equal(result.status, "draft");
  assert.ok(result.files.includes("sources.md"));
});
test("paper rejects filesystem escapes, symlinks and executable TeX", async (t) => {
  const root = await paper(t);
  await rm(join(root, "sources.md"));
  await symlink(join(tmpdir(), "outside-paper-source"), join(root, "sources.md"));
  await assert.rejects(run({ root, action: "write", input: { ...manuscript, sources: "private" } }), /symlinks/);
  for (const text of ['\\input{/etc/passwd}', '\\write18{touch /tmp/x}', '\\csname input\\endcsname', '\\usepackage{shellesc}', '^^5cinput', '\\begin{filecontents}{/tmp/leak}x\\end{filecontents}']) assert.throws(() => validateTex(text));
  await assert.rejects(run({ root, action: "figure", input: { name: "../bad.png", base64: "", caption: "x" } }), /filename/);
});
test("source ZIP is deterministic and contains only explicitly selected entries", () => {
  const first = sourceZip({ "manuscript.tex": "source", "references.bib": "bib" });
  assert.deepEqual(first, sourceZip({ "references.bib": "bib", "manuscript.tex": "source" }));
  assert.equal(first.readUInt32LE(0), 0x04034b50);
  assert.equal(first.subarray(-22).readUInt16LE(10), 2);
  assert.ok(!first.includes(Buffer.from("sources.md")));
});
test("QA cannot mark an unbuilt draft ready", async (t) => {
  const root = await paper(t);
  await assert.rejects(run({ root, action: "qa", input: { aestheticEye: {}, figureTable: {} } }), /Build the current/);
});
test("real PDF builds await QA, reject stale reviews, and invalidate after source edits", async (t) => {
  if (process.env.EASEL_PAPER_SMOKE !== "1") return t.skip("Set EASEL_PAPER_SMOKE=1 to exercise an installed TeX compiler.");
  const root = await paper(t);
  await run({ root, action: "write", input: { ...manuscript, sections: [{ title: "Method", text: "Verified citation \\cite{test}." }], bibliography: "@book{test,author={Test Author},title={Test Book},year={2026}}" } });
  const built = await run({ root, action: "build" });
  assert.equal(built.status, "built-awaiting-qa");
  assert.equal((await readFile(join(root, "manuscript.pdf"))).subarray(0,5).toString(), "%PDF-");
  const eye = { schema: 1, visualInference: true, reviewer: { kind: "visual-inference" }, reviewedAt: new Date().toISOString(), pdfSha256: "old", expectedDiagrams: 0, diagrams: [], expectedFigures: 0, figures: [], brand: { canonicalName: "Aesthetic.Computer", dotColor: "#B44887", design: "pass", checks: { period: "pass", dotColor: "pass" } } };
  const figureTable = { pdfSha256: built.pdfSha256, status: "pass", reviewedBy: "test-fixture", reviewedAt: new Date().toISOString(), inspectedPages: [1] };
  await assert.rejects(run({ root, action: "qa", input: { aestheticEye: eye, figureTable } }), /stale/);
  eye.pdfSha256 = built.pdfSha256;
  assert.equal((await run({ root, action: "qa", input: { aestheticEye: eye, figureTable } })).status, "ready");
  await rm(join(root, "aesthetic-eye.json"));
  assert.equal((await run({ root, action: "status" })).status, "built-awaiting-qa");
  await writeFile(join(root, "references.bib"), "% changed\n");
  assert.equal((await run({ root, action: "status" })).status, "stale-build");
});
