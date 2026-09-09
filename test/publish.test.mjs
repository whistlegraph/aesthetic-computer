import assert from "node:assert/strict";
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { looksLikePiece, planPublish, publishPiece, slugFor } from "../src/publish.mjs";

const SOURCE = "export function paint({ wipe }) { wipe(70, 50, 100); }\n";

async function piece(context, name = "smiley.mjs", source = SOURCE) {
  const root = await mkdtemp(join(tmpdir(), "aesthetic-code-publish-"));
  context.after(() => rm(root, { recursive: true, force: true }));
  const file = join(root, name);
  await writeFile(file, source);
  return { root, file };
}

test("plans the handle route from the file name", () => {
  const plan = planPublish({ file: "/work/smiley.mjs", handle: "tester" });
  assert.equal(plan.slug, "smiley");
  assert.equal(plan.name, "piece-smiley.mjs");
  assert.equal(plan.grantUrl, "https://aesthetic.computer/presigned-upload-url/mjs/piece-smiley.mjs/user");
  assert.equal(plan.route, "https://aesthetic.computer/@tester/smiley");
  assert.equal(plan.mediaUrl, "https://aesthetic.computer/media/@tester/piece/smiley.mjs");
  assert.equal(planPublish({ file: "/work/x.lisp", slug: "face", handle: "t" }).route, "https://aesthetic.computer/@t/face");
  assert.throws(() => slugFor("/work/bad name.mjs"), /letters, digits/);
  assert.throws(() => planPublish({ file: "/work/notes.txt", handle: "t" }), /\.mjs and \.lisp/);
  assert.equal(looksLikePiece(SOURCE, ".mjs"), true);
  assert.equal(looksLikePiece("const x = 1;", ".mjs"), false);
});

test("publishes through the presigned user-bucket flow and verifies", async (context) => {
  const { file } = await piece(context);
  const calls = [];
  const session = { handle: "tester", signedIn: true, token: async () => "tok" };
  const result = await publishPiece({
    file,
    session,
    fetch: async (url, options = {}) => {
      calls.push({ url, options });
      if (url.includes("/presigned-upload-url/")) {
        return new Response(JSON.stringify({ uploadURL: "https://bucket.test/sub/piece/smiley.mjs?sig=1" }), {
          status: 200,
          headers: { "content-type": "application/json" },
        });
      }
      if (options.method === "PUT") return new Response("", { status: 200 });
      return new Response(SOURCE, { status: 200 });
    },
  });
  assert.equal(result.route, "https://aesthetic.computer/@tester/smiley");
  assert.equal(result.verified, true);
  assert.equal(calls.length, 3);
  assert.equal(calls[0].url, "https://aesthetic.computer/presigned-upload-url/mjs/piece-smiley.mjs/user");
  assert.equal(calls[0].options.headers.Authorization, "Bearer tok");
  assert.match(calls[0].options.headers["User-Agent"], /Mozilla/);
  assert.equal(calls[1].url, "https://bucket.test/sub/piece/smiley.mjs?sig=1");
  assert.equal(calls[1].options.method, "PUT");
  assert.equal(calls[1].options.headers["Content-Type"], "application/javascript; charset=utf-8");
  assert.equal(calls[1].options.headers["x-amz-acl"], "public-read");
  assert.equal(calls[1].options.body, SOURCE);
  assert.equal(calls[2].url, "https://aesthetic.computer/media/@tester/piece/smiley.mjs");
});

test("refuses to publish without a signed-in handle or a real piece", async (context) => {
  const { file } = await piece(context);
  await assert.rejects(
    publishPiece({ file, session: { handle: "", signedIn: false }, fetch: async () => assert.fail("no network") }),
    /not signed in/,
  );
  await assert.rejects(
    publishPiece({ file, session: { handle: "", signedIn: true }, fetch: async () => assert.fail("no network") }),
    /no @handle/,
  );
  const { file: plain } = await piece(context, "plain.mjs", "const x = 1;\n");
  await assert.rejects(
    publishPiece({ file: plain, session: { handle: "t", signedIn: true, token: async () => "tok" } }),
    /does not export a piece/,
  );
});
