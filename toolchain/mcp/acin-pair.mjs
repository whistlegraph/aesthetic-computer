#!/usr/bin/env node
// acin-pair.mjs — sign a screen in to rax as the shared AC session.
//
// A rax screen with nobody signed in (the desktop app with only a controller,
// a TV, a kiosk) shows a QR bottom left. A phone would scan it; this does the
// phone's part from here: the code is read out of a picture of that screen
// (a Frame capture), the same two posts the phone's page makes are made with
// ~/.ac-token, and the rax server seats that one screen as the handle
// (rax/pairing.mjs). The token goes to the rax server alone, and the code is
// never printed: it works once and lives two minutes.
//
//   node toolchain/mcp/acin-pair.mjs <picture | pair address | code>

import { execFile } from "node:child_process";
import { promisify } from "node:util";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { loadTokens, UA } from "./ac-token.mjs";
import { CODE } from "../../rax/pair-number.mjs";

const pexec = promisify(execFile);
const HOME = "https://aesthetic.computer/rax/";
const PAIR = /^(https?:\/\/[^\s#]+\/)pair#([A-Za-z0-9_-]+)$/;
const WHY = {
  gone: "That code was used or ran out: capture the screen again and pair from the new picture.",
  slow: "Too many tries from here: wait a minute, then capture the screen again.",
  account: "~/.ac-token was not accepted: run `ac-login`.",
  handle: "This account has no @handle.",
  bad: "The rax server refused the request.",
};

// The pairing address in a picture of the screen. A capture shows the QR a
// few pixels to the module, so it is read as it is and then enlarged.
async function readQr(image) {
  const dir = await mkdtemp(join(tmpdir(), "acin-pair-"));
  try {
    const tries = [image];
    for (const size of [1024, 2048]) {
      const out = join(dir, `${size}.png`);
      try {
        await pexec("sips", ["-Z", String(size), "-s", "format", "png", image, "--out", out]);
        tries.push(out);
      } catch { /* not a picture sips can read: zbarimg says so below */ }
    }
    for (const file of tries) {
      let stdout = "";
      try { ({ stdout } = await pexec("zbarimg", ["-q", "--raw", file])); }
      catch (e) {
        if (e.code === "ENOENT") throw new Error("zbarimg is not installed (`brew install zbar`).");
        continue; // (exit 4: no code in this one)
      }
      const found = stdout.split("\n").map((l) => l.trim()).find((l) => PAIR.test(l));
      if (found) return found;
    }
    return null;
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
}

async function post(base, path, body) {
  const res = await fetch(new URL(path, base), {
    method: "POST",
    headers: { "content-type": "application/json", "user-agent": UA },
    body: JSON.stringify(body),
  });
  try { return await res.json(); } catch { return { error: `HTTP ${res.status}` }; }
}

// → { ok, handle } or { ok: false, error, why }.
export async function pairScreen({ image, url, code, base } = {}) {
  let address = url;
  if (image) {
    address = await readQr(image);
    if (!address) return { ok: false, error: "no-qr", why: "No rax pairing QR in that picture. A signed-in screen shows none; crop the capture closer to the QR (bottom left) and try again." };
  }
  const match = address && PAIR.exec(address);
  if (match) [, base, code] = match;
  base ||= HOME;
  if (!CODE.test(code || "")) return { ok: false, error: "no-code", why: "Give a picture of the screen, its pairing address, or the code." };
  if (!/^https:\/\/([a-z0-9-]+\.)*aesthetic\.computer\//.test(base) && !/^http:\/\/(127\.0\.0\.1|localhost)(:\d+)?\//.test(base))
    return { ok: false, error: "foreign", why: `The token is not sent to ${new URL(base).host}.` };
  const { access_token: accessToken } = await loadTokens();
  const answer = await post(base, "./pair", { code, accessToken });
  return answer?.ok
    ? { ok: true, handle: answer.handle, server: new URL(base).host }
    : { ok: false, error: answer?.error || "unknown", why: WHY[answer?.error] || "The rax server did not answer." };
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const arg = process.argv[2];
  if (!arg) {
    console.error("usage: acin-pair.mjs <picture | pair address | code>");
    process.exit(2);
  }
  const said = await pairScreen(PAIR.test(arg) ? { url: arg } : CODE.test(arg) ? { code: arg } : { image: arg });
  console.log(said.ok ? `signed in as @${said.handle} (${said.server})` : said.why);
  process.exit(said.ok ? 0 : 1);
}
