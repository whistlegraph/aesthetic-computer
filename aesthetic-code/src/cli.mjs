#!/usr/bin/env node
// cli.mjs — account and publish subcommands behind `aesthetic`.
import process from "node:process";
import { ACSession } from "./ac-session.mjs";
import { planPublish, publishPiece } from "./publish.mjs";

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
    if (process.env.AESTHETIC_CODE_DRY_RUN === "1") {
      const plan = planPublish({ file, slug, handle: session.handle || "handle" });
      out(`would publish ${plan.path} as ${plan.route}`);
    } else {
      const result = await publishPiece({ file, slug, session, onStep: (step) => note(`${step}…`) });
      out(result.route);
      if (!result.verified) note("published, but the live file did not read back yet");
    }
  } else {
    fail(`unknown command: ${command || "(none)"}`);
  }
} catch (error) {
  fail(error?.message || String(error));
}
