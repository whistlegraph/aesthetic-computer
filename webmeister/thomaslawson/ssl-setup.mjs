#!/usr/bin/env node
/**
 * Automated Let's Encrypt SSL for thomaslawson.com
 * Combines certbot DNS challenge with GoDaddy DNS automation
 */

import { spawn } from "child_process";
import { readFileSync, existsSync, mkdirSync } from "fs";
import { join } from "path";
import readline from "readline";
import { PageController, listPages } from "../browser.mjs";

const DOMAIN = "thomaslawson.com";
const CERT_DIR = "/workspaces/aesthetic-computer/aesthetic-computer-vault/gigs/thomaslawson.com/ssl";

// Ensure cert directory exists
if (!existsSync(CERT_DIR)) mkdirSync(CERT_DIR, { recursive: true });

async function getGoDaddyDNSPage() {
  const pages = await listPages();
  let dnsPage = pages.find(p => p.url.includes("dcc.godaddy.com") && p.url.includes("dns"));
  if (dnsPage) return PageController.connect("dcc.godaddy.com");
  
  const gdPage = pages.find(p => p.url.includes("godaddy.com"));
  if (gdPage) {
    const ctrl = await PageController.connect("godaddy.com");
    await ctrl.goto(`https://dcc.godaddy.com/manage/${DOMAIN}/dns`);
    await ctrl.wait(3000);
    return ctrl;
  }
  throw new Error("Open GoDaddy in Chrome first");
}

async function addACMEChallenge(token) {
  console.log("\n🔧 Adding ACME challenge TXT record...");
  const ctrl = await getGoDaddyDNSPage();
  
  // Click Add New Record
  await ctrl.eval(() => {
    const btn = [...document.querySelectorAll("button")].find(b => b.textContent.includes("Add New Record"));
    if (btn) btn.click();
  });
  await ctrl.wait(2000);
  
  // Get current content to understand the form
  const html = await ctrl.getHtml();
  console.log("Form opened, filling fields...");
  
  // The form should now be visible - fill it via page evaluation
  await ctrl.eval((tkn) => {
    // Find type selector and set to TXT
    const selects = document.querySelectorAll("select");
    for (const sel of selects) {
      const opts = [...sel.options].map(o => o.value);
      if (opts.includes("TXT")) {
        sel.value = "TXT";
        sel.dispatchEvent(new Event("change", { bubbles: true }));
        break;
      }
    }
  }, token);
  await ctrl.wait(500);
  
  await ctrl.eval((tkn) => {
    // Fill name field with _acme-challenge
    const inputs = document.querySelectorAll("input[type=text], input:not([type])");
    for (const inp of inputs) {
      const placeholder = inp.placeholder || "";
      const name = inp.name || "";
      if (placeholder.includes("Name") || placeholder.includes("Host") || name.includes("name") || name.includes("host")) {
        inp.value = "_acme-challenge";
        inp.dispatchEvent(new Event("input", { bubbles: true }));
        inp.dispatchEvent(new Event("change", { bubbles: true }));
        break;
      }
    }
  }, token);
  await ctrl.wait(500);
  
  await ctrl.eval((tkn) => {
    // Fill value/data field with the token
    const inputs = document.querySelectorAll("input[type=text], input:not([type]), textarea");
    for (const inp of inputs) {
      const placeholder = inp.placeholder || "";
      const name = inp.name || "";
      if (placeholder.includes("Value") || placeholder.includes("Data") || name.includes("value") || name.includes("data")) {
        inp.value = tkn;
        inp.dispatchEvent(new Event("input", { bubbles: true }));
        inp.dispatchEvent(new Event("change", { bubbles: true }));
        break;
      }
    }
  }, token);
  
  console.log("✅ Form filled with _acme-challenge TXT record");
  console.log("   Value:", token);
  console.log("\n⚠️  Please click SAVE in the browser to add the record!");
  return ctrl;
}

async function prompt(question) {
  const rl = readline.createInterface({ input: process.stdin, output: process.stdout });
  return new Promise(resolve => rl.question(question, ans => { rl.close(); resolve(ans); }));
}

async function main() {
  console.log("🔒 Let's Encrypt SSL Generator for", DOMAIN);
  console.log("================================================\n");
  
  console.log("This script will:");
  console.log("1. Run certbot to get challenge tokens");
  console.log("2. Help you add TXT records to GoDaddy DNS");
  console.log("3. Generate the SSL certificate\n");
  
  const token = await prompt("Enter the ACME challenge token (from certbot): ");
  if (!token) {
    console.log("No token provided. Run certbot manually first:");
    console.log("  certbot certonly --manual --preferred-challenges dns -d thomaslawson.com -d www.thomaslawson.com");
    return;
  }
  
  await addACMEChallenge(token.trim());
  
  console.log("\nAfter saving the DNS record:");
  console.log("1. Wait 2-5 minutes for DNS propagation");
  console.log("2. Verify with: dig TXT _acme-challenge.thomaslawson.com");
  console.log("3. Press Enter in certbot to continue");
}

const cmd = process.argv[2];
if (cmd === "add") {
  const token = process.argv[3];
  if (token) {
    await addACMEChallenge(token);
    process.exit(0);
  }
}
await main();
