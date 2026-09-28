import assert from "node:assert/strict";
import { mkdtemp, readFile, rm, writeFile, mkdir, utimes, stat } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { freshSession } from "../shared/ac-token.mjs";

async function scratch(t, record) {
  const dir = await mkdtemp(join(tmpdir(), "ac-token-"));
  t.after(() => rm(dir, { recursive: true, force: true }));
  const file = join(dir, ".ac-token");
  await writeFile(file, JSON.stringify(record));
  return file;
}

// A fake Auth0 that rotates: each refresh token works once.
function rotatingAuth0(first) {
  let live = first, calls = 0;
  const fetch = async (_url, { body }) => {
    calls += 1;
    await new Promise((r) => setTimeout(r, 30));
    const { refresh_token } = JSON.parse(body);
    if (refresh_token !== live) return { ok: false, status: 403, json: async () => ({}) };
    live = `r${calls}`;
    return { ok: true, json: async () => ({ access_token: `a${calls}`, refresh_token: live, expires_in: 3600 }) };
  };
  return { fetch, calls: () => calls };
}

test("concurrent refreshes spend the refresh token once", async (t) => {
  const file = await scratch(t, { access_token: "old", refresh_token: "r0", expires_at: Date.now() - 1, user: { handle: "jeffrey" } });
  const auth0 = rotatingAuth0("r0");
  const results = await Promise.all([1, 2, 3].map(() => freshSession({ file, fetch: auth0.fetch })));
  assert.equal(auth0.calls(), 1);
  assert.deepEqual(results.map((r) => r.access_token), ["a1", "a1", "a1"]);
  const saved = JSON.parse(await readFile(file, "utf8"));
  assert.equal(saved.refresh_token, "r1");
  assert.equal(saved.user.handle, "jeffrey");
  await assert.rejects(stat(file + ".lock"));
});

test("a fresh token is left alone unless forced", async (t) => {
  const file = await scratch(t, { access_token: "live", refresh_token: "r0", expires_at: Date.now() + 3600e3 });
  const auth0 = rotatingAuth0("r0");
  assert.equal((await freshSession({ file, fetch: auth0.fetch })).access_token, "live");
  assert.equal(auth0.calls(), 0);
  assert.equal((await freshSession({ file, fetch: auth0.fetch, force: true })).access_token, "a1");
});

test("a dead holder's lock is taken over", async (t) => {
  const file = await scratch(t, { access_token: "old", refresh_token: "r0", expires_at: Date.now() - 1 });
  await mkdir(file + ".lock");
  const old = new Date(Date.now() - 60e3);
  await utimes(file + ".lock", old, old);
  const auth0 = rotatingAuth0("r0");
  assert.equal((await freshSession({ file, fetch: auth0.fetch })).access_token, "a1");
});
