import test from "node:test";
import assert from "node:assert/strict";
import { authorizeLoopboyWait } from "../lib/loopboy-request-auth.mjs";

const loops = {
  fia: { sessionId: "session-fia", delivery: "inbox" },
  loretta: { sessionId: "session-loretta", delivery: "inbox" },
};

function context(contact, sessionId) {
  return {
    headers: {
      "x-slab-loopboy-contact": contact,
      "x-slab-prompt-session-id": sessionId,
    },
  };
}

test("Loopboy wait authorizes the matching per-client HTTP identity", () => {
  const result = authorizeLoopboyWait({
    context: context("FIA", "session-fia"),
    env: {},
    loops,
    requestedContact: "fia",
  });
  assert.equal(result.contact, "fia");
  assert.equal(result.sessionId, "session-fia");
  assert.equal(result.loop, loops.fia);
});

test("Loopboy wait rejects a different contact or session", () => {
  assert.throws(
    () => authorizeLoopboyWait({
      context: context("fia", "session-fia"), env: {}, loops, requestedContact: "loretta",
    }),
    /bound to fia, not loretta/,
  );
  assert.throws(
    () => authorizeLoopboyWait({
      context: context("fia", "session-loretta"), env: {}, loops, requestedContact: "fia",
    }),
    /not the bound fia listener/,
  );
});

test("Loopboy wait retains stdio environment fallback", () => {
  const result = authorizeLoopboyWait({
    env: {
      SLAB_LOOPBOY_CONTACT: "fia",
      SLAB_PROMPT_SESSION_ID: "session-fia",
    },
    loops,
  });
  assert.equal(result.contact, "fia");
  assert.equal(result.sessionId, "session-fia");
});
