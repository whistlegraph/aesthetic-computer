import test from "node:test";
import assert from "node:assert/strict";
import {
  POSTHOG_OPTIONS,
  mediaCreatedProperties,
  pieceProperties,
  routePieceProperties,
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

test("route milestones omit prompt and retain only safe piece dimensions", () => {
  assert.equal(routePieceProperties("/"), null);
  assert.equal(routePieceProperties("/prompt"), null);
  assert.deepEqual(routePieceProperties("/notepat"), {
    piece: "notepat",
    piece_kind: "built-in",
    route: "/notepat",
  });
  assert.deepEqual(routePieceProperties("/@published"), {
    piece: null,
    piece_kind: "published-or-code",
    route: "/@published",
  });
});

test("media milestones expose only kind and anonymous account state", () => {
  assert.deepEqual(mediaCreatedProperties("png", true), {
    media_kind: "painting",
    account_state: "identified",
  });
  assert.deepEqual(mediaCreatedProperties("lisp", false), {
    media_kind: "kidlisp",
    account_state: "anonymous",
  });
  assert.equal(mediaCreatedProperties("json", true), null);
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
