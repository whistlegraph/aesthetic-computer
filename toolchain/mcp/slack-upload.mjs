#!/usr/bin/env node
// slack-upload.mjs — attach a file to a Slack channel with the same credentials
// the Slack MCP uses (aesthetic-computer-vault/mcp/slack.env), because
// slack-mcp-server exposes text messages but no file upload.
//
//   node toolchain/mcp/slack-upload.mjs --check
//   node toolchain/mcp/slack-upload.mjs --channel C09F3RLRNHL --file a.pdf \
//        [--title "Title"] [--comment-file note.md]
//
// Uses Slack's current flow: files.getUploadURLExternal → POST bytes →
// files.completeUploadExternal (files.upload is retired). Tokens are read into
// this process only and never printed.

import { readFileSync, statSync } from "node:fs";
import { basename } from "node:path";
import { requireCredentials, redactCredentials } from "../vault/credentials.mjs";

const { values: env } = requireCredentials("slack");
const token = env.SLACK_MCP_XOXP_TOKEN || env.SLACK_MCP_XOXB_TOKEN || env.SLACK_MCP_XOXC_TOKEN;
const cookie = env.SLACK_MCP_XOXD_TOKEN;
if (!token) {
  console.error("slack-upload: no Slack token resolved");
  process.exit(1);
}
const headers = { Authorization: `Bearer ${token}` };
if (token.startsWith("xoxc-")) {
  if (!cookie) { console.error("slack-upload: xoxc token needs SLACK_MCP_XOXD_TOKEN"); process.exit(1); }
  headers.Cookie = `d=${cookie}`;
}

async function api(method, params) {
  const body = new URLSearchParams(params);
  const res = await fetch(`https://slack.com/api/${method}`, { method: "POST", headers, body });
  const json = await res.json();
  if (!json.ok) throw new Error(redactCredentials(`${method}: ${json.error}${json.response_metadata ? " " + JSON.stringify(json.response_metadata) : ""}`, env));
  return json;
}

const args = process.argv.slice(2);
const arg = (k) => { const i = args.indexOf(k); return i >= 0 ? args[i + 1] : undefined; };

const who = await api("auth.test", {});
console.log(`workspace: ${who.team} (${who.team_id}) as ${who.user}`);
if (args.includes("--check")) process.exit(0);

const channel = arg("--channel");
const file = arg("--file");
if (!channel || !file) { console.error("need --channel and --file"); process.exit(1); }
const bytes = readFileSync(file);
const filename = arg("--name") || basename(file);
const title = arg("--title") || filename;
const comment = arg("--comment-file") ? readFileSync(arg("--comment-file"), "utf8").trim() : undefined;

const { upload_url, file_id } = await api("files.getUploadURLExternal", {
  filename, length: String(statSync(file).size),
});
const put = await fetch(upload_url, { method: "POST", headers: headers.Cookie ? { Cookie: headers.Cookie } : {}, body: bytes });
if (!put.ok) throw new Error(`upload POST: HTTP ${put.status}`);

const done = await api("files.completeUploadExternal", {
  files: JSON.stringify([{ id: file_id, title }]),
  channel_id: channel,
  ...(comment ? { initial_comment: comment } : {}),
});
const f = done.files?.[0] || {};
console.log(`uploaded ${f.name || filename} (${file_id}) to ${channel}${f.permalink ? " " + f.permalink : ""}`);
