#!/usr/bin/env node
// cli.mjs — account, publish and handle-colour subcommands behind `aesthetic`.
import "./env.mjs";
import process from "node:process";
import { ACSession } from "./ac-session.mjs";
import { planPublish, publishPiece } from "./publish.mjs";
import { handleColorPlan } from "./handle-colors.mjs";
import { SITE } from "./ac-session.mjs";

const [command = "", ...rest] = process.argv.slice(2);
const session = new ACSession();
const out = (text) => process.stdout.write(`${text}\n`);
const note = (text) => process.stderr.write(`${text}\n`);
const fail = (message) => {
  process.stderr.write(`aesthetic: ${message}\n`);
  process.exit(1);
};

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
