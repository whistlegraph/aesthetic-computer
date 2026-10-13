import test from "node:test";
import assert from "node:assert/strict";
import { paintingStorageUrl } from "./painting-storage.mjs";

test("resizing preserves fully qualified painting and chat upload paths", () => {
  const base = "https://user-aesthetic-computer.sfo3.digitaloceanspaces.com/auth0%7Cfixture/";
  assert.equal(paintingStorageUrl({ user: "auth0|fixture", slug: "auth0|fixture/chat/1782187678513" }), base + "chat/1782187678513.png");
  for (const slug of ["auth0|fixture/painting/2026.1.2", "painting/2026.1.2", "2026.1.2"]) {
    assert.equal(paintingStorageUrl({ user: "auth0|fixture", slug }), base + "painting/2026.1.2.png");
  }
  assert.equal(paintingStorageUrl({ slug: "abc:recording" }), "https://art-aesthetic-computer.sfo3.digitaloceanspaces.com/abc.png");
  assert.throws(() => paintingStorageUrl({ user: "fixture", slug: "../other" }));
});
