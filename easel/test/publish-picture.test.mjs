import {createPaintingWipService} from '../../system/backend/painting-wips.mjs';
import {memoryWipRepository} from '../../system/tests/painting-wip-fixture.mjs';
import {syncPictureWip,pictureWipRecord} from '../src/picture-wip.mjs';
import {JSZip} from '../media/picture/ac-tools.mjs';
import test from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, rm, readFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { Artifacts } from "../src/artifacts.mjs";
import { publishPicture, publishedPicture } from "../src/publish-picture.mjs";
const session = {
  handle: "tester",
  signedIn: true,
  token: async () => "test-bearer-secret",
};
async function setup(t) {
  const cwd = await mkdtemp(join(tmpdir(), "easel-publish-picture-"));
  t.after(() => rm(cwd, { recursive: true, force: true }));
  const artifacts = new Artifacts(cwd);
  await artifacts.create("picture");
  return artifacts;
}
function server({
  loseTrack = false,
  loseUpload = false,
  badMedia = false,
} = {}) {
  const repository=memoryWipRepository();
  repository.code=async()=>"vaf";
  const service=createPaintingWipService(repository);
  const calls = [],
    slug = "auth0|test/painting-easel-test",
    code = "vaf";
  let recording;
  let uploaded,
    tracked = false;
  const json = (value) => new Response(JSON.stringify(value));
  const fetch = async (url, options = {}) => {
    url = String(url);
    calls.push({ url, options });
    if(url.endsWith('/api/painting-wip')) {
      const input=JSON.parse(options.body);
      return json(await service[input.action](input));
    }
    if (url.includes("/presigned-upload-url/")) {
      assert.equal(options.headers.Authorization, "Bearer test-bearer-secret");
      if(url.includes("/zip/"))return json({uploadURL:"https://objects.example/painting.zip",slug:slug+".zip"});
      return json({
        uploadURL:
          "https://objects.example/painting.png?signature=never-persist",
        slug: slug + ".png",
        code: "NOT-THE-PAINTING-CODE",
      });
    }
    if (options.method === "PUT") {
      assert(!options.headers.Authorization);
      if(url.endsWith(".zip")){recording=Buffer.from(options.body);return new Response("");}
      uploaded = Buffer.from(options.body);
      if (loseUpload) {
        loseUpload = false;
        throw new Error("lost upload response");
      }
      return new Response("");
    }
    if (url.endsWith("/api/track-media")) {
      assert.equal(options.headers.Authorization, "Bearer test-bearer-secret");
      const input=JSON.parse(options.body);
      assert.equal(input.wip.code,code);
      await service.seal(input.wip,null,input.slug);
      tracked = true;
      if (loseTrack) {
        loseTrack = false;
        throw new Error("lost track response");
      }
      return json({ slug, code, paintingId: "painting-record" });
    }
    if (url.includes("/api/painting-metadata?"))
      return tracked
        ? json({ slug, code, nuked: false })
        : new Response("", { status: 404 });
    if (url.includes("/api/painting-code?"))
      return json({ slug, code, handle: "tester" });
    if (
      url.includes("/media/paintings/") ||
      url === "https://objects.example/painting.png"
    ) {
      assert(!options.headers?.Authorization);
      return new Response(badMedia ? Buffer.from("wrong") : uploaded);
    }
    throw new Error("Unexpected request " + url);
  };
  return {
    fetch,
    calls,
    repository,
    get recording(){return recording;},
    get uploaded() {
      return uploaded;
    },
    set badMedia(v) {
      badMedia = v;
    },
  };
}
test("explicit publish uploads only accepted PNG and returns server-issued painting #code", async (t) => {
  const artifacts = await setup(t);
  await artifacts.run("propose", { seed: "pending", color: [255, 0, 0, 255] });
  const current = await artifacts.selected(),
    accepted = await readFile(join(current.root, "composite.png")),
    preview = await readFile(join(current.root, "preview.png"));
  assert.notDeepEqual(accepted, preview);
  const api = server();
  const result = await publishPicture({ artifacts, session, fetch: api.fetch });
  assert.deepEqual(api.uploaded, accepted);
  assert.equal(result.code, "vaf");
  assert.equal(result.tag, "#vaf");
  assert.equal(result.route, "https://aesthetic.computer/#vaf");
  assert.equal(result.verified, true);
  const receipt = String(
    await readFile(
      join(
        artifacts.root,
        "publications",
        current.id,
        `v${current.version}.json`,
      ),
    ),
  );
  assert(!receipt.includes("test-bearer-secret"));
  assert(!receipt.includes("never-persist"));
  assert.equal(
    (await publishedPicture({ artifacts, handle: "tester" })).tag,
    "#vaf",
  );
  await publishPicture({ artifacts, session, fetch: api.fetch });
  assert.equal(api.calls.filter((c) => c.options.method === "PUT" && c.url.endsWith(".png?signature=never-persist")).length, 1);
  assert.equal(api.calls.filter((c) => c.url.endsWith("/api/track-media")).length, 1);
});
test("lost tracking response safely retries sealing the same WIP", async (t) => {
  const artifacts = await setup(t),
    api = server({ loseTrack: true });
  await assert.rejects(
    publishPicture({ artifacts, session, fetch: api.fetch }),
    /lost track/,
  );
  const result = await publishPicture({ artifacts, session, fetch: api.fetch });
  assert.equal(result.verified, true);
  assert.equal(api.calls.filter((c) => c.url.endsWith("/api/track-media")).length, 2);
  assert.equal((await api.repository.find({code:"vaf"})).status,"done");
});
test("lost upload response recovers public bytes without another PUT", async (t) => {
  const artifacts = await setup(t),
    api = server({ loseUpload: true });
  await assert.rejects(
    publishPicture({ artifacts, session, fetch: api.fetch }),
    /lost upload/,
  );
  await publishPicture({ artifacts, session, fetch: api.fetch });
  assert.equal(api.calls.filter((c) => c.options.method === "PUT" && c.url.endsWith(".png?signature=never-persist")).length, 1);
});
test("no public QR before PNG verification; a new local version clears published state", async (t) => {
  const artifacts = await setup(t),
    api = server({ badMedia: true });
  await assert.rejects(
    publishPicture({ artifacts, session, fetch: api.fetch }),
    /not yet verified/,
  );
  assert.equal(await publishedPicture({ artifacts, handle: "tester" }), null);
  api.badMedia = false;
  await publishPicture({ artifacts, session, fetch: api.fetch });
  assert.equal(api.calls.filter((c) => c.options.method === "PUT" && c.url.endsWith(".png?signature=never-persist")).length, 1);
  await artifacts.run("propose", { seed: "new" });
  assert.equal(await publishedPicture({ artifacts, handle: "tester" }), null);
});
test("signed-out publishing performs no network request", async (t) => {
  const artifacts = await setup(t);
  await assert.rejects(
    publishPicture({
      artifacts,
      session: { handle: "", signedIn: false },
      fetch: () => assert.fail("network"),
    }),
    /login/,
  );
});

test('Easel starts a short-code WIP, saves accepted steps, and forks after Done',async t=>{
 const artifacts=await setup(t),api=server();
 const first=await syncPictureWip({artifacts,session,fetch:api.fetch});
 assert.equal(first.record.code,'vaf');assert.equal(first.record.steps,0);
 await artifacts.run('draw',{tool:'fill',x:10,y:10,color:[255,255,0,255]});await artifacts.run('accept');
 const next=await syncPictureWip({artifacts,session,fetch:api.fetch});assert.equal(next.record.code,'vaf');assert.equal(next.record.steps,1);
 const done=await publishPicture({artifacts,session,fetch:api.fetch});assert.equal(done.code,'vaf');
 const zip=await JSZip.loadAsync(api.recording);const steps=JSON.parse(await zip.file('painting.json').async('string'));assert.equal(steps.length,2);
 const original=await artifacts.selected();assert.equal((await pictureWipRecord(artifacts,original.id)).status,'done');
 await artifacts.run('draw',{tool:'circle',x:100,y:100,radius:30,color:[255,0,0,255]});
 const fork=await artifacts.selected();assert.notEqual(fork.id,original.id);assert.equal(fork.parent,'vaf');
 assert.equal((await pictureWipRecord(artifacts,original.id)).status,'done');
});
