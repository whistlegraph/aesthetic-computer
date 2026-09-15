import test from "node:test";
import assert from "node:assert/strict";
import { createPaintingWipService, paintingWipExpired } from "../backend/painting-wips.mjs";
import { encodePaintingState, decodePaintingState } from "../public/aesthetic.computer/lib/painting-state.mjs";
import { createNoPaintPiece, createNoPaintProposalLayer, appendNoPaintLayer } from "../public/aesthetic.computer/lib/nopaint-pieces.mjs";
import { memoryWipRepository } from "./painting-wip-fixture.mjs";

const identity = { id: "a".repeat(32), key: "b".repeat(48), width: 4, height: 3, initialLayers: 1 };
const base = () => createNoPaintPiece({ seed: "test", width: 4, height: 3, pixels: new Uint8ClampedArray(48).fill(255) });
function add(piece, color) {
  const pixels = new Uint8ClampedArray(48).fill(color);
  return appendNoPaintLayer(piece, createNoPaintProposalLayer({ piece, proposal: { kind: "line" },
    proposalNumber: piece.layers.length, proposalFrame: 10, pixels, pixelMode: "composite" }), pixels);
}
const rejected = (status) => (error) => error.status === status;

test("public WIP → accepted steps → Done preserves the code and freezes every edit", async () => {
  const repo = memoryWipRepository(), service = createPaintingWipService(repo);
  const created = await service.create(identity);
  assert.equal(created.status, "wip");
  assert.equal(created.steps, 0);
  assert.equal((await service.create(identity)).code, created.code, "creation retries reuse the same record");
  let piece = base();
  const first = await service.save({ code: created.code, key: identity.key, revision: 0, state: await encodePaintingState(piece) });
  const visitor = await service.read(created.code, true);
  assert.equal(visitor.canEdit, false);
  assert.equal(visitor.key, undefined);
  assert.equal(visitor.editorHash, undefined);
  assert.deepEqual(await decodePaintingState(visitor.state), piece);
  assert.equal((await service.read(created.code, false, null, identity.key)).canEdit, true);
  await assert.rejects(service.save({ code: created.code, key: "c".repeat(48), revision: 1, state: visitor.state }), rejected(403));
  piece = add(piece, 100);
  const encoded = await encodePaintingState(piece);
  const second = await service.save({ code: created.code, key: identity.key, revision: first.revision, state: encoded });
  assert.equal(second.steps, 1);
  assert.equal(repo.paintings.get(created.code).wip.expiresAt, undefined);
  assert.equal((await service.save({ code: created.code, key: identity.key, revision: first.revision, state: encoded })).revision, second.revision,
    "lost-response retry is idempotent");
  await assert.rejects(service.save({ code: created.code, key: identity.key, revision: first.revision, state: visitor.state }), rejected(409));
  const reference = { code: created.code, key: identity.key, revision: second.revision };
  const done = await service.seal(reference, null, "final:recording");
  assert.equal(done.code, created.code);
  assert.equal((await service.read(created.code)).status, "done");
  assert.equal((await service.read(created.code, false, null, identity.key)).canEdit, false);
  await assert.rejects(service.save({ ...reference, state: encoded }), rejected(409));
  await service.seal(reference, null, "a-different-upload");
  assert.equal(repo.paintings.get(created.code).slug, "final:recording", "Done retries never replace the sealed image");
  const fork = await service.create({ ...identity, id: "c".repeat(32), parent: done.code });
  assert.notEqual(fork.code, done.code);
  assert.equal(fork.parent, done.code);
  assert.equal(fork.steps, 0);
});

test("only WIPs with zero accepted steps expire after one hour", async () => {
  let now = 1000;
  const repo = memoryWipRepository(), service = createPaintingWipService(repo, () => now);
  const empty = await service.create(identity);
  await service.save({ code: empty.code, key: identity.key, revision: 0, state: await encodePaintingState(base()) });
  const worked = await service.create({ ...identity, id: "c".repeat(32) });
  await service.save({ code: worked.code, key: identity.key, revision: 0, state: await encodePaintingState(add(base(), 90)) });
  now += 3600001;
  await assert.rejects(service.read(empty.code), rejected(404));
  assert.equal(paintingWipExpired(repo.paintings.get(empty.code), now), true);
  assert.equal((await service.read(worked.code)).steps, 1);
});

test("account ownership and racing saves do not allow an older writer to overwrite", async () => {
  const repo = memoryWipRepository(), service = createPaintingWipService(repo);
  const user = { sub: "owner" };
  const made = await service.create(identity, user);
  const snapshot = await encodePaintingState(base());
  await assert.rejects(service.save({ code: made.code, key: identity.key, revision: 0, state: snapshot }), rejected(403));
  assert.equal((await service.read(made.code, false, user)).canEdit, true);
  const results = await Promise.allSettled([snapshot, await encodePaintingState(add(base(), 70))].map((state) =>
    service.save({ code: made.code, revision: 0, state }, user)));
  assert.equal(results.filter((r) => r.status === "fulfilled").length, 1);
  assert.equal(results.find((r) => r.status === "rejected").reason.status, 409);
  assert.equal(repo.states.size, 1, "the losing writer reclaims its unused snapshot");
});
