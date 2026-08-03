import test from "node:test";
import assert from "node:assert/strict";
import {
  POSTHOG_OPTIONS,
  pieceProperties,
  safeRoutePath,
  userIdentity,
} from "../public/aesthetic.computer/lib/product-analytics.mjs";

test("analytics defaults disable broad or content-bearing capture", () => {
  assert.equal(POSTHOG_OPTIONS.autocapture, false);
  assert.equal(POSTHOG_OPTIONS.disable_session_recording, true);
  assert.equal(POSTHOG_OPTIONS.capture_exceptions, false);
  assert.equal(POSTHOG_OPTIONS.capture_performance, false);
  assert.equal(POSTHOG_OPTIONS.advanced_disable_flags, true);
  assert.equal(POSTHOG_OPTIONS.disable_external_dependency_loading, true);
  assert.equal(POSTHOG_OPTIONS.respect_dnt, true);
});

test("routes omit user-published names and source", () => {
  assert.equal(safeRoutePath("/@bash/hub"), "/@published");
  assert.equal(safeRoutePath("/$nece"), "/$code");
  assert.equal(safeRoutePath("/prompt~(wipe red)"), "/prompt");
  assert.equal(safeRoutePath("/notepat?ignored=true"), "/_other");
});

test("piece events name built-ins but minimize published pieces", () => {
  assert.deepEqual(
    pieceProperties("aesthetic.computer/disks/notepat", "/notepat"),
    { piece: "notepat", piece_kind: "built-in", route: "/notepat" },
  );
  assert.deepEqual(pieceProperties("@bash/hub", "/@bash/hub"), {
    piece: null,
    piece_kind: "published-or-code",
    route: "/@published",
  });
});

test("identity exports a stable id and public handle, never email", () => {
  assert.deepEqual(
    userIdentity({
      sub: "auth0|123",
      handle: "@jeffrey",
      email: "private@example.com",
    }),
    { distinctId: "auth0|123", properties: { handle: "@jeffrey" } },
  );
  assert.equal(userIdentity({ email: "private@example.com" }), null);
});
