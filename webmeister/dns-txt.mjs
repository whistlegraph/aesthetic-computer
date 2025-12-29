#!/usr/bin/env node
/**
 * GoDaddy DNS TXT Record Helper for Let's Encrypt
 * Usage: node dns-txt.mjs add <token>
 */

import { PageController, listPages } from "./browser.mjs";

const DOMAIN = "thomaslawson.com";

async function getGoDaddyDNS() {
  const pages = await listPages();
  const dnsPage = pages.find(p => p.url.includes("dcc.godaddy.com"));
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

export async function addTXT(name, value) {
  const ctrl = await getGoDaddyDNS();
  
  // Click Add New Record
  await ctrl.eval(() => {
    const btn = [...document.querySelectorAll("button")].find(b => b.textContent.includes("Add New Record"));
    if (btn) btn.click();
  });
  await ctrl.wait(1500);
  
  // Select TXT type
  await ctrl.eval(() => {
    const sel = document.querySelector("#dnsRecordIdDropdown");
    if (sel) { sel.value = "TXT"; sel.dispatchEvent(new Event("change", { bubbles: true })); }
  });
  await ctrl.wait(800);
  
  // Fill name
  await ctrl.eval((n) => {
    const inp = document.querySelector("#nameDnsFieldInput");
    if (inp) { inp.focus(); inp.value = n; inp.dispatchEvent(new Event("input", { bubbles: true })); }
  }, name);
  await ctrl.wait(300);
  
  // Fill value
  await ctrl.eval((v) => {
    const inp = document.querySelector("[data-testid=dataDnsFieldInput]");
    if (inp) { inp.focus(); inp.value = v; inp.dispatchEvent(new Event("input", { bubbles: true })); }
  }, value);
  
  console.log("✅ TXT record form filled:");
  console.log("   Name:", name);
  console.log("   Value:", value);
  return ctrl;
}

export async function clickSave(ctrl) {
  await ctrl.eval(() => {
    const btn = [...document.querySelectorAll("button")].find(b => 
      b.textContent.includes("Save") || b.textContent.includes("Add Record")
    );
    if (btn) btn.click();
  });
  await ctrl.wait(2000);
  console.log("✅ Save clicked");
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const [cmd, arg1, arg2] = process.argv.slice(2);
  
  if (cmd === "add" && arg1) {
    const name = arg2 ? arg1 : "_acme-challenge";
    const value = arg2 || arg1;
    const ctrl = await addTXT(name, value);
    console.log("\n⚠️  Click SAVE in browser to add the record");
    await ctrl.close();
  } else if (cmd === "acme" && arg1) {
    const ctrl = await addTXT("_acme-challenge", arg1);
    console.log("\n⏳ Clicking save...");
    await clickSave(ctrl);
    console.log("\n📋 Verify with: dig TXT _acme-challenge.thomaslawson.com");
    await ctrl.close();
  } else {
    console.log("GoDaddy DNS TXT Record Helper");
    console.log("");
    console.log("Usage:");
    console.log("  node dns-txt.mjs add <value>           - Add _acme-challenge TXT (manual save)");
    console.log("  node dns-txt.mjs add <name> <value>    - Add custom TXT record (manual save)");
    console.log("  node dns-txt.mjs acme <token>          - Add ACME challenge and auto-save");
  }
}
