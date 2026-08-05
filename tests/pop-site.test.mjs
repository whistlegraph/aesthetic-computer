import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";

import { publicProjection } from "../pop/bin/publish-release-records.mjs";

test("public release projection removes private and local fields", async () => {
  const source = JSON.parse(await readFile("pop/releases/marimbaba/release.json", "utf8"));
  const serialized = JSON.stringify(publicProjection(source));
  for (const forbidden of ["localPath", "privateOverlay", "defaultPath", "~/", "source"]) {
    assert.equal(serialized.includes(forbidden), false, `must not publish ${forbidden}`);
  }
  assert.match(serialized, /SimpleAudioSingle/);
  assert.match(serialized, /TestMessage/);
});

test("Pop page states the DDEX boundary", async () => {
  const html = await readFile("system/public/pop.aesthetic.computer/index.html", "utf8");
  assert.match(html, /ERN 4\.3\.2/);
  assert.match(html, /private overlay/i);
  assert.match(html, /not been transmitted/);
});
