import assert from "node:assert/strict";
import { resolveFunctionName } from "../lith/route-resolution.mjs";

function run() {
  assert.equal(resolveFunctionName("news", undefined), "news");
  assert.equal(resolveFunctionName("news", "posts"), "news-api");
  assert.equal(resolveFunctionName("news", "submit"), "news-api");
  assert.equal(resolveFunctionName("news", "new"), "news");
  assert.equal(resolveFunctionName("news", "report"), "news");
  assert.equal(resolveFunctionName("news", "nwhh"), "news");
  assert.equal(resolveFunctionName("news", ["nwhh"]), "news");
  assert.equal(resolveFunctionName("news", "item/abcd"), "news");
  assert.equal(resolveFunctionName("news", "toll"), "news-toll");
  assert.equal(resolveFunctionName("chat", "messages"), "chat-messages");
  assert.equal(resolveFunctionName("verify-password"), "verify-builds-password");
  assert.equal(resolveFunctionName("ff1", "proxy/status", { "ff1-proxy": true }), "ff1-proxy");

  console.log("✅ lith route resolution tests passed");
}

run();
