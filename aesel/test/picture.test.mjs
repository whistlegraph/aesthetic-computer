import test from "node:test";
import assert from "node:assert/strict";
import {
  mkdtemp,
  readFile,
  writeFile,
  symlink,
  rm,
  mkdir,
  copyFile,
} from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { create, run } from "../src/media/picture.mjs";
import { encode, decode } from "../media/picture/png.mjs";
import { plan, generate } from "../media/picture/illy.mjs";
async function project(t) {
  const root = await mkdtemp(join(tmpdir(), "easel-picture-"));
  t.after(() => rm(root, { recursive: true, force: true }));
  await create({ root, name: "Test" });
  return root;
}
test("proposal previews without changing accepted output; accept, hide, discard, restart", async (t) => {
  const root = await project(t),
    before = await readFile(join(root, "composite.png"));
  await run({
    root,
    action: "propose",
    input: {
      seed: "dots",
      color: [255, 0, 0, 255],
      thickness: 20,
      points: [
        { x: 10, y: 10 },
        { x: 490, y: 490 },
      ],
    },
  });
  assert.deepEqual(await readFile(join(root, "composite.png")), before);
  assert.notDeepEqual(await readFile(join(root, "preview.png")), before);
  const proposal = await readFile(join(root, "preview.png"));
  await run({ root, action: "accept" });
  assert.deepEqual(await readFile(join(root, "composite.png")), proposal);
  const state = JSON.parse(await readFile(join(root, "picture.json")));
  await run({
    root,
    action: "layer",
    input: { id: state.layers[0].id, visible: false },
  });
  assert.deepEqual(await readFile(join(root, "preview.png")), before);
  await run({ root, action: "propose", input: { seed: "second" } });
  await run({ root, action: "discard" });
  assert.deepEqual(await readFile(join(root, "preview.png")), before);
});
test("seeded brush replay is deterministic and imported PNG becomes a layer", async (t) => {
  const root = await project(t);
  await run({ root, action: "propose", input: { seed: "same" } });
  const bytes = await readFile(join(root, "preview.png"));
  await run({ root, action: "discard" });
  await run({ root, action: "propose", input: { seed: "same" } });
  assert.deepEqual(await readFile(join(root, "preview.png")), bytes);
  await writeFile(join(root, "import.png"), bytes);
  await run({ root, action: "import_png", input: { path: "import.png" } });
  await run({ root, action: "accept" });
  assert.deepEqual(await readFile(join(root, "composite.png")), bytes);
});
test("rejects path escapes, symlinks, invalid parameters without changing accepted image", async (t) => {
  const root = await project(t),
    before = await readFile(join(root, "composite.png"));
  await assert.rejects(
    run({ root, action: "import_png", input: { path: "../outside.png" } }),
    /escapes/,
  );
  await symlink(tmpdir(), join(root, "link"));
  await assert.rejects(
    run({ root, action: "import_png", input: { path: "link/file.png" } }),
    /Symlinks/,
  );
  await assert.rejects(
    run({
      root,
      action: "propose",
      input: {
        seed: "x",
        points: [
          { x: -1, y: 0 },
          { x: 2, y: 3 },
        ],
      },
    }),
    /within/,
  );
  await assert.rejects(
    run({
      root,
      action: "generate",
      input: { provider: "openai", model: "gpt-image-2", prompt: "test" },
    }),
    /authorization/,
  );
  assert.deepEqual(await readFile(join(root, "composite.png")), before);
});
test("PNG codec preserves alpha and rejects corrupt/oversized files", () => {
  const original = {
    width: 2,
    height: 1,
    data: Buffer.from([255, 0, 0, 128, 0, 255, 0, 0]),
  };
  const bytes = encode(original);
  assert.deepEqual(decode(bytes), original);
  bytes[29] ^= 1;
  assert.throws(() => decode(bytes), /checksum/);
  assert.throws(
    () => encode({ width: 9000, height: 1, data: Buffer.alloc(0) }),
    /dimensions/,
  );
});
test("Illy OpenAI adapter uses explicit key/model and preserves provenance; no retry on failure", async () => {
  const p = plan({
      provider: "openai",
      model: "gpt-image-2",
      prompt: "a small blue square",
    }),
    png = encode({ width: 1, height: 1, data: Buffer.from([0, 0, 255, 255]) });
  let calls = 0;
  const result = await generate(p, {
    env: { OPENAI_API_KEY: "test-only" },
    fetchImpl: async (url, options) => {
      calls++;
      assert.equal(url, "https://api.openai.com/v1/images/generations");
      assert.equal(JSON.parse(options.body).model, "gpt-image-2");
      assert.equal(options.headers.Authorization, "Bearer test-only");
      return new Response(
        JSON.stringify({ data: [{ b64_json: png.toString("base64") }] }),
        { status: 200 },
      );
    },
  });
  assert.deepEqual(result.bytes, png);
  assert.equal(result.provenance.promptHash.length, 64);
  assert.equal(calls, 1);
  await assert.rejects(
    generate(p, { env: {}, fetchImpl: () => assert.fail() }),
    /OPENAI_API_KEY/,
  );
  calls = 0;
  await assert.rejects(
    generate(p, {
      env: { OPENAI_API_KEY: "test-only" },
      fetchImpl: async () => {
        calls++;
        return new Response("", { status: 503 });
      },
    }),
    /not retried/,
  );
  assert.equal(calls, 1);
});
test("Illy fal adapter validates queue routes and supports generation output", async () => {
  const p = plan({ provider: "fal", model: "fal-ai/flux/dev", prompt: "test" }),
    png = encode({ width: 1, height: 1, data: Buffer.from([0, 0, 255, 255]) });
  let receipt;
  const result = await generate(p, {
    env: { FAL_KEY: "test-only" },
    onSubmitted: async (r) => (receipt = r),
    fetchImpl: async (url) => {
      if (String(url).endsWith("/flux/dev"))
        return new Response(
          JSON.stringify({
            request_id: "r1",
            status_url: "https://queue.fal.run/status",
            response_url: "https://queue.fal.run/result",
          }),
        );
      if (String(url).endsWith("/status"))
        return new Response(JSON.stringify({ status: "COMPLETED" }));
      if (String(url).endsWith("/result"))
        return new Response(
          JSON.stringify({
            images: [{ url: "https://v3.fal.media/image.png" }],
          }),
        );
      return new Response(png);
    },
  });
  assert.equal(receipt.requestId, "r1");
  assert.deepEqual(result.bytes, png);
});
test("paid image job saves output and receipt and refuses duplicate submission", async (t) => {
  const root = await project(t),
    oldFetch = globalThis.fetch,
    oldKey = process.env.OPENAI_API_KEY;
  let calls = 0;
  process.env.OPENAI_API_KEY = "test-only";
  const png = encode({
    width: 1,
    height: 1,
    data: Buffer.from([255, 0, 255, 255]),
  });
  globalThis.fetch = async () => {
    calls++;
    return new Response(
      JSON.stringify({ data: [{ b64_json: png.toString("base64") }] }),
    );
  };
  t.after(() => {
    globalThis.fetch = oldFetch;
    if (oldKey === undefined) delete process.env.OPENAI_API_KEY;
    else process.env.OPENAI_API_KEY = oldKey;
  });
  const input = {
    provider: "openai",
    model: "gpt-image-2",
    prompt: "pink",
    authorized: true,
    jobId: "test-job",
  };
  const result = await run({ root, action: "generate", input });
  assert.equal(calls, 1);
  assert(result.files.some((p) => p.endsWith(".illy.json")));
  assert.deepEqual(await readFile(join(root, "jobs/test-job.png")), png);
  await assert.rejects(
    run({ root, action: "generate", input }),
    /already exists/,
  );
  assert.equal(calls, 1);
  assert(
    !String(await readFile(join(root, "jobs/test-job.json"))).includes(
      "test-only",
    ),
  );
});
test("bundled AC proposal matches canonical source, and picture works outside repository", async (t) => {
  const canonical = await readFile(
      new URL(
        "../../system/public/aesthetic.computer/disks/line.mjs",
        import.meta.url,
      ),
      "utf8",
    ),
    vendored = await readFile(
      new URL("../media/picture/ac-line.mjs", import.meta.url),
      "utf8",
    );
  const contract = canonical.slice(
    canonical.indexOf("const nopaintProposal ="),
    canonical.indexOf(
      "\nfunction meta()",
      canonical.indexOf("const nopaintProposal ="),
    ),
  );
  assert(
    vendored.includes(
      contract.replace("const nopaintProposal", "export const nopaintProposal"),
    ),
  );
  const root = await project(t),
    dist = join(root, "standalone");
  await mkdir(join(dist, "src/media"), { recursive: true });
  await mkdir(join(dist, "media/picture"), { recursive: true });
  for (const file of [
    "src/media/picture.mjs",
    "media/picture/ac-line.mjs",
    "media/picture/ac-tools.mjs",
    "media/picture/draw.mjs",
    "media/picture/png.mjs",
    "media/picture/illy.mjs",
  ])
    await copyFile(new URL("../" + file, import.meta.url), join(dist, file));
  const adapter = await import("file://" + join(dist, "src/media/picture.mjs"));
  await adapter.create({ root: join(root, "isolated"), name: "Portable" });
  await adapter.run({
    root: join(root, "isolated"),
    action: "propose",
    input: { seed: "portable" },
  });
  assert.equal(
    decode(await readFile(join(root, "isolated/preview.png"))).width,
    512,
  );
});

test('AC Fill respects a closed boundary, records a preview, and survives acceptance', async t => {
  const root = await project(t);
  await run({root, action:'draw', input:{tool:'box',x:100,y:100,width:200,height:200,filled:false,color:[0,0,0,255]}});
  await run({root, action:'accept'});
  const before = await readFile(join(root,'composite.png'));
  await run({root,action:'draw',input:{tool:'fill',x:150,y:150,color:[255,255,0,255]}});
  assert.deepEqual(await readFile(join(root,'composite.png')),before);
  const preview=decode(await readFile(join(root,'preview.png')));
  const pixel=(x,y)=>Array.from(preview.data.subarray((y*512+x)*4,(y*512+x)*4+4));
  assert.deepEqual(pixel(150,150),[255,255,0,255]);
  assert.deepEqual(pixel(50,50),[255,255,255,255]);
  assert.deepEqual(pixel(100,100),[0,0,0,255]);
  await run({root,action:'accept'});
  assert.deepEqual(decode(await readFile(join(root,'composite.png'))),preview);
  await run({root,action:'draw',input:{tool:'circle',x:200,y:200,radius:30,color:[255,0,0,255]}});
  await run({root,action:'discard'});
  assert.deepEqual(decode(await readFile(join(root,'composite.png'))),preview);
});
