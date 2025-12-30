#!/usr/bin/env node
// fill-godaddy.mjs - Generate JS snippets for GoDaddy login

import fs from "fs/promises";
import path from "path";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const VAULT_PATH = path.join(__dirname, "../aesthetic-computer-vault");

async function loadCredentials(domain) {
  const credPath = path.join(VAULT_PATH, "gigs", domain, "credentials.json");
  const data = await fs.readFile(credPath, "utf8");
  return JSON.parse(data);
}

async function main() {
  const domain = process.argv[2] || "thomaslawson.com";
  
  try {
    const creds = await loadCredentials(domain);
    if (!creds.godaddy) {
      console.error("No GoDaddy credentials for", domain);
      process.exit(1);
    }

    const { username, password } = creds.godaddy;
    
    console.log("");
    console.log("GoDaddy Login for:", domain);
    console.log("User:", username);
    console.log("");
    console.log("STEP 1: Open https://sso.godaddy.com in Simple Browser");
    console.log("");
    console.log("STEP 2: Paste in DevTools console (F12):");
    console.log("");
    console.log("// Fill username");
    console.log("(function() {");
    console.log("  const i = document.querySelector(\"input[id=username], input[name=username]\");");
    console.log("  if (i) { i.focus(); i.value = \"" + username + "\"; i.dispatchEvent(new Event(\"input\", {bubbles:true})); console.log(\"Done\"); }");
    console.log("})();");
    console.log("");
    console.log("STEP 3: Click Next, then paste:");
    console.log("");
    console.log("// Fill password");
    console.log("(function() {");
    console.log("  const i = document.querySelector(\"input[type=password]\");");
    console.log("  if (i) { i.focus(); i.value = \"" + password + "\"; i.dispatchEvent(new Event(\"input\", {bubbles:true})); console.log(\"Done\"); }");
    console.log("})();");
    console.log("");
    console.log("STEP 4: Click Sign In");
  } catch (err) {
    console.error("Error:", err.message);
    process.exit(1);
  }
}

main();
