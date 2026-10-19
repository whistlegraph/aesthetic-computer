import { test } from "node:test";
import assert from "node:assert/strict";
import { userMediaTarget } from "./media-path.mjs";

const userId = "auth0|abc";

test("piece sources come from the storage origin, so a republish shows at once", () => {
  for (const extension of ["mjs", "lisp", "p5", "l5", "lua"]) {
    const url = userMediaTarget({ userId, subPath: `piece/stars.${extension}`, extension });
    assert.match(url, /^https:\/\/user-aesthetic-computer\.sfo3\.digitaloceanspaces\.com\/auth0%7Cabc\/piece\/stars\./, extension);
  }
});

test("pictures and other media stay on the CDN", () => {
  for (const extension of ["png", "mp4", "zip"]) {
    const url = userMediaTarget({ userId, subPath: `painting/a.${extension}`, extension });
    assert.match(url, /^https:\/\/user\.aesthetic\.computer\//, extension);
  }
});
