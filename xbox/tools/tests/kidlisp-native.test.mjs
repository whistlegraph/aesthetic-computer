import assert from "node:assert/strict";
import { compileKidLispSource, compilePublishedKidLisp } from "../kidlisp-native.mjs";

const obk = compileKidLispSource("lightblue\nstamp #j8t \nblur 4", { code: "obk" });
assert.equal(obk.formCount, 3);
assert.deepEqual(obk.paintings, ["j8t"]);
assert.match(obk.generated, /wipe\(173,216,230\)/);
assert.match(obk.generated, /stampPainting\("#j8t",Math\.random\(\)\*runtime\(\)\.width,Math\.random\(\)\*runtime\(\)\.height,1\)/);
assert.match(obk.generated, /blur\(1\)/);

assert.throws(() => compileKidLispSource("stamp https://example.com/a.png"), /only a short #painting code/);
assert.throws(() => compileKidLispSource("fetch https://example.com"), /unsupported native KidLisp form/);
assert.throws(() => compileKidLispSource("blur 100"), /between 0 and 48/);
assert.throws(() => compileKidLispSource("$other"), /unsupported native KidLisp form/);

let requested;
const remote = await compilePublishedKidLisp("$obk", {
  fetchImpl: async (url, options) => {
    requested = { url: String(url), options };
    return new Response(JSON.stringify({ source: "lightblue\nstamp #j8t\nblur 4", handle: "@fifi" }), {
      status: 200,
      headers: { "content-type": "application/json" },
    });
  },
});
assert.equal(requested.url, "https://aesthetic.computer/api/store-kidlisp?code=obk");
assert.equal(requested.options.redirect, "error");
assert.equal(remote.handle, "@fifi");
assert.deepEqual(remote.paintings, ["j8t"]);

console.log("xbox native KidLisp bridge: tests passed");
