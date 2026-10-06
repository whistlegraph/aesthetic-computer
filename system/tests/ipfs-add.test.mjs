import test from "node:test";
import assert from "node:assert/strict";
import { createHandler } from "../netlify/functions/ipfs-add.mjs";

const fileCid = "Qm" + "a".repeat(44), rootCid = "Qm" + "b".repeat(44);
const file = { name: "index.html", mimeType: "text/html", base64: Buffer.from("<html></html>").toString("base64") };
const event = body => ({ httpMethod: "POST", headers: {}, body: JSON.stringify(body) });
function fixture({ response, user = {}, admin = true } = {}) {
  const requests = [];
  const handler = createHandler({
    authorize: async () => user, hasAdmin: async () => admin,
    fetch: async (url, options) => {
      requests.push({ url, options });
      return new Response(response || JSON.stringify({ Name: "index.html", Hash: fileCid }) + "\n" + JSON.stringify({ Name: "", Hash: rootCid }) + "\n");
    },
  });
  return { handler, requests };
}

test("directory pin returns the wrapping CID and preserves ZIP filenames and bytes", async () => {
  const { handler, requests } = fixture();
  const files = [file, { ...file, name: "cover.gif", mimeType: "image/gif" }, { ...file, name: "thumbnail.png", mimeType: "image/png" }];
  const result = await handler(event({ files }));
  assert.equal(result.statusCode, 200);
  assert.deepEqual(JSON.parse(result.body), { cid: rootCid, uri: `ipfs://${rootCid}` });
  assert.match(requests[0].url, /wrap-with-directory=true/);
  const uploaded = requests[0].options.body.getAll("file");
  assert.deepEqual(uploaded.map(f => f.name), files.map(f => f.name));
  assert.deepEqual(uploaded.map(f => f.type), files.map(f => f.mimeType));
  for (const f of uploaded) assert.equal(await f.text(), "<html></html>");
  assert.ok(requests.some(r => r.url.includes(`/pin/add?arg=${rootCid}`)));
});

test("single files and JSON metadata still use file CIDs", async () => {
  for (const body of [file, { name: "metadata.json", json: { name: "Daily" } }]) {
    const { handler, requests } = fixture({ response: JSON.stringify({ Hash: fileCid }) });
    const result = await handler(event(body));
    assert.equal(JSON.parse(result.body).uri, `ipfs://${fileCid}`);
    assert.doesNotMatch(requests[0].url, /wrap-with-directory/);
  }
});

test("a missing wrapping CID fails instead of minting the index.html file CID", async () => {
  const { handler, requests } = fixture({ response: JSON.stringify({ Name: "index.html", Hash: fileCid }) });
  assert.equal((await handler(event({ files: [file] }))).statusCode, 502);
  assert.equal(requests.length, 1);
});

test("directory uploads retain admin authorization and reject ambiguous filenames", async () => {
  for (const [options, status] of [[{ user: null }, 401], [{ admin: false }, 403]]) {
    const { handler, requests } = fixture(options);
    assert.equal((await handler(event({ files: [file] }))).statusCode, status);
    assert.equal(requests.length, 0);
  }
  for (const files of [[], [file, file], [{ ...file, name: "../index.html" }], [null]]) {
    const { handler, requests } = fixture();
    assert.equal((await handler(event({ files }))).statusCode, 400);
    assert.equal(requests.length, 0);
  }
});
