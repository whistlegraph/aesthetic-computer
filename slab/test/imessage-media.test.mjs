import assert from "node:assert/strict";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";

import {
  chooseMessagesMediaTransport,
  inspectMessagesMedia,
} from "../lib/imessage-media.mjs";

function withTempFile(name, bytes, callback) {
  const dir = mkdtempSync(join(tmpdir(), "slab-imessage-media-"));
  try {
    const path = join(dir, name);
    writeFileSync(path, bytes);
    return callback(path);
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
}

test("accepts a PDF by extension and header", () => {
  withTempFile("review.pdf", Buffer.from("%PDF-1.7\n1 0 obj\n<<>>\nendobj\n%%EOF\n"), (path) => {
    const info = inspectMessagesMedia(path);
    assert.equal(info.kind, "document");
    assert.equal(info.mimeType, "application/pdf");
    assert.equal(info.extension, ".pdf");
    assert.match(info.sha256, /^[0-9a-f]{64}$/);
  });
});

test("rejects a renamed non-PDF", () => {
  withTempFile("not-really.pdf", Buffer.from("plain text"), (path) => {
    assert.throws(
      () => inspectMessagesMedia(path),
      /does not contain a valid PDF header/,
    );
  });
});

test("preserves supported image behavior", () => {
  withTempFile("image.png", Buffer.from("image bytes"), (path) => {
    const info = inspectMessagesMedia(path);
    assert.equal(info.kind, "image");
    assert.equal(info.mimeType, "image/png");
  });
});

test("rejects unsupported attachment types", () => {
  withTempFile("notes.txt", Buffer.from("hello"), (path) => {
    assert.throws(() => inspectMessagesMedia(path), /unsupported Messages attachment type/);
  });
});

test("keeps PDFs on the backend with no UI fallback", () => {
  assert.deepEqual(
    chooseMessagesMediaTransport("document", "auto", "iMessage"),
    { primary: "backend", allowUiFallback: false },
  );
  assert.deepEqual(
    chooseMessagesMediaTransport("document", "backend", "RCS"),
    { primary: "backend", allowUiFallback: false },
  );
  assert.throws(
    () => chooseMessagesMediaTransport("document", "ui", "iMessage"),
    /require the verified backend/,
  );
  assert.throws(
    () => chooseMessagesMediaTransport("document", "auto", "SMS"),
    /require explicit mediaTransport=backend/,
  );
});

test("preserves guarded image fallback routing", () => {
  assert.deepEqual(
    chooseMessagesMediaTransport("image", "auto", "iMessage"),
    { primary: "backend", allowUiFallback: true },
  );
  assert.deepEqual(
    chooseMessagesMediaTransport("image", "auto", "RCS"),
    { primary: "ui", allowUiFallback: false },
  );
});
