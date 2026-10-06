#!/usr/bin/env node
// cli.mjs — account, profile and publish subcommands behind `ac`.
import "./env.mjs";
import process from "node:process";
import { ACSession } from "./ac-session.mjs";
import { planPublish, publishPiece } from "./publish.mjs";
import { handleColorPlan } from "./handle-colors.mjs";
import { SITE } from "./ac-session.mjs";
import { planMime, postMime } from "./mime.mjs";

const [command = "", ...rest] = process.argv.slice(2);
const session = new ACSession();
const out = (text) => process.stdout.write(`${text}\n`);
const note = (text) => process.stderr.write(`${text}\n`);
const fail = (message) => {
  process.stderr.write(`aesthetic: ${message}\n`);
  process.exit(1);
};

// POST to the site as the signed-in person; the server's own message on failure.
async function authorizedPost(path, payload) {
  const token = await session.token();
  const response = await fetch(`${SITE}${path}`, {
    method: "POST",
    headers: { "Content-Type": "application/json", Authorization: `Bearer ${token}` },
    body: JSON.stringify(payload),
  });
  const body = await response.json().catch(() => ({}));
  if (!response.ok) fail(body.message || body.error || `the site answered ${response.status}`);
  return body;
}

try {
  if (command === "whoami") {
    out(session.label());
    process.exit(session.signedIn ? 0 : 1);
  } else if (command === "login") {
    const handle = await session.login({
      forcePrompt: rest.includes("--fresh"),
      onUrl: (url) => note(`Sign in at\n  ${url}`),
    });
    out(handle ? `Signed in as @${handle}` : "Signed in · no handle yet");
  } else if (command === "logout") {
    out(session.logout({ browser: rest.includes("--browser") }) ? "Signed out" : "Already signed out");
  } else if (command === "publish") {
    const [file, slug = ""] = rest.filter((argument) => !argument.startsWith("--"));
    if (!file) fail("usage: aesthetic publish <file> [slug]");
    if (process.env.AESEL_DRY_RUN === "1") {
      const plan = planPublish({ file, slug, handle: session.handle || "handle" });
      out(`would publish ${plan.path} as ${plan.route}`);
    } else {
      const result = await publishPiece({ file, slug, session, onStep: (step) => note(`${step}…`) });
      out(result.route);
      if (!result.verified) note("published, but the live file did not read back yet");
    }
  } else if (command === "mime") {
    // A file you made, posted to mime.ac as an opening post under your
    // @handle; its MIME type picks the board. Anything after the file is the
    // caption. Prints the thread's address.
    const [file, ...words] = rest.filter((argument) => !argument.startsWith("--"));
    if (!file) fail('usage: ac mime <file> ["caption"]');
    const plan = planMime(file, { caption: words.join(" ") });
    if (process.env.AESEL_DRY_RUN === "1") {
      out(`would post ${plan.name} (${plan.type}, ${(plan.size / 1024).toFixed(0)} KB) to mime.ac as @${session.handle || "handle"}${plan.caption ? ` · "${plan.caption}"` : ""}`);
    } else {
      if (!session.handle) fail("sign in first: ac login");
      note(`posting ${plan.name} as @${session.handle}…`);
      const posted = await postMime(plan, { session });
      out(posted.url);
    }
  } else if (command === "check") {
    // Run a piece for real (headless Chrome) and print its errors: a local
    // file before publishing, or a published @handle/slug or URL after.
    const target = rest[0];
    if (!target) fail("usage: ac check <piece.mjs | @handle/slug | url>");
    const { checkPiece } = await import("./check-piece.mjs");
    const { target: name, problems } = await checkPiece(target);
    if (!problems.length) { out(`✓ ${name} ran without errors`); process.exit(0); }
    out(`✕ ${name}: ${problems.length} problem${problems.length === 1 ? "" : "s"}`);
    for (const p of problems) out(`  ${p}`);
    process.exit(1);
  } else if (command === "profile") {
    // What your profile shows, read from the same public endpoints the
    // profile page uses: handle, colours, latest mood, and where it lives.
    const handle = session.handle;
    if (!handle) fail("sign in first: ac login");
    const get = async (path) => { const r = await fetch(`${SITE}${path}`); return r.ok ? r.json() : {}; };
    const [profile, colors] = await Promise.all([get(`/api/profile/@${handle}`), get(`/api/handle-colors?handle=${encodeURIComponent(handle)}`)]);
    const hex = (c) => `#${[c.r, c.g, c.b].map((v) => v.toString(16).padStart(2, "0")).join("")}`;
    out(`@${handle}`);
    out(`  page    ${SITE}/@${handle}`);
    out(`  colors  ${Array.isArray(colors.colors) ? colors.colors.map(hex).join(" ") : "theme default"}`);
    out(`  mood    ${profile.mood?.mood || profile.mood || "—"}`);
  } else if (command === "mood") {
    const text = rest.join(" ").trim();
    if (!session.handle) fail("sign in first: ac login");
    if (!text) fail('usage: ac mood "how you feel"');
    const body = await authorizedPost("/api/mood", { mood: text });
    out(`@${session.handle} · ${body.mood || text}`);
  } else if (command === "handle") {
    const wanted = (rest[0] || "").replace(/^@/, "");
    if (!session.signedIn) fail("sign in first: ac login");
    if (!wanted) fail("usage: ac handle <new-handle>");
    if (process.env.AESEL_DRY_RUN === "1") out(`would change @${session.handle} to @${wanted}`);
    else {
      const body = await authorizedPost("/api/handle", { handle: wanted });
      out(`@${body.handle || wanted}`);
    }
  } else if (command === "colors") {
    // Your @handle's letters, painted: one colour repeats, several cycle.
    // Names (orange, teal, …) or hex. The site keeps them; everything that
    // draws your handle — the prompt, Aesel, Slab's rocks — reads them back.
    const handle = session.handle;
    if (!handle) fail("sign in first: ac login");
    if (!rest.length) fail("usage: ac colors <colour…>   e.g. ac colors orange teal");
    const colors = handleColorPlan(`@${handle}`, rest);
    if (process.env.AESEL_DRY_RUN === "1") out(JSON.stringify(colors));
    else {
      const token = await session.token();
      const response = await fetch(`${SITE}/api/handle-colors`, {
        method: "POST",
        headers: { "Content-Type": "application/json", Authorization: `Bearer ${token}` },
        body: JSON.stringify({ handle, colors }),
      });
      const body = await response.json().catch(() => ({}));
      if (!response.ok) fail(body.message || `the site answered ${response.status}`);
      out(`@${handle} · ${rest.join(" ")}`);
    }
  } else {
    fail(`unknown command: ${command || "(none)"}`);
  }
} catch (error) {
  fail(error?.message || String(error));
}
