#!/usr/bin/env node
/**
 * GoDaddy DNS automation for thomaslawson.com
 * Adds/removes TXT records for Let's Encrypt DNS-01 challenge
 */

import { PageController, listPages } from "../browser.mjs";

const DOMAIN = "thomaslawson.com";

export async function getGoDaddyDNSPage() {
  // First check if we have a GoDaddy DNS page open
  const pages = await listPages();
  let dnsPage = pages.find(p => p.url.includes("godaddy.com") && p.url.includes("dns"));
  
  if (dnsPage) {
    return PageController.connect("godaddy.com");
  }
  
  // Try to find any GoDaddy page and navigate to DNS
  const gdPage = pages.find(p => p.url.includes("godaddy.com"));
  if (gdPage) {
    const ctrl = await PageController.connect("godaddy.com");
    await ctrl.goto(`https://dcc.godaddy.com/manage/${DOMAIN}/dns`);
    await ctrl.wait(3000);
    return ctrl;
  }
  
  throw new Error("No GoDaddy page found - open GoDaddy in Chrome first");
}

export async function addTXTRecord(name, value) {
  console.log(`Adding TXT record: ${name} = ${value}`);
  const ctrl = await getGoDaddyDNSPage();
  
  // Click "Add New Record" button
  await ctrl.eval(() => {
    const btn = [...document.querySelectorAll("button, a")].find(b => 
      b.textContent.includes("Add") || b.textContent.includes("New Record")
    );
    if (btn) btn.click();
  });
  await ctrl.wait(1000);
  
  // Select TXT type
  await ctrl.eval(() => {
    const typeSelect = document.querySelector("select[name*=\"type\"], [data-testid*=\"type\"]");
    if (typeSelect) {
      typeSelect.value = "TXT";
      typeSelect.dispatchEvent(new Event("change", { bubbles: true }));
    }
  });
  await ctrl.wait(500);
  
  // Fill in the name (host) field
  await ctrl.eval((n) => {
    const nameInput = document.querySelector("input[name*=\"name\"], input[name*=\"host\"], [data-testid*=\"name\"]");
    if (nameInput) {
      nameInput.value = n;
      nameInput.dispatchEvent(new Event("input", { bubbles: true }));
    }
  }, name);
  
  // Fill in the value field
  await ctrl.eval((v) => {
    const valueInput = document.querySelector("input[name*=\"value\"], textarea[name*=\"value\"], [data-testid*=\"value\"]");
    if (valueInput) {
      valueInput.value = v;
      valueInput.dispatchEvent(new Event("input", { bubbles: true }));
    }
  }, value);
  
  console.log("TXT record fields filled - review and save in browser");
  return ctrl;
}

export async function listDNSRecords() {
  const ctrl = await getGoDaddyDNSPage();
  await ctrl.wait(2000);
  
  const content = await ctrl.getContent();
  console.log("DNS Records page content:");
  console.log(content);
  return ctrl;
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const [cmd, name, value] = process.argv.slice(2);
  
  if (cmd === "list") {
    const ctrl = await listDNSRecords();
    await ctrl.close();
  } else if (cmd === "add" && name && value) {
    const ctrl = await addTXTRecord(name, value);
    await ctrl.close();
  } else if (cmd === "goto") {
    const ctrl = await getGoDaddyDNSPage();
    console.log("Navigated to DNS page:", ctrl.url());
    await ctrl.close();
  } else {
    console.log("GoDaddy DNS automation for thomaslawson.com");
    console.log("");
    console.log("Usage:");
    console.log("  node godaddy-dns.mjs list              - List DNS records");
    console.log("  node godaddy-dns.mjs goto              - Navigate to DNS page");
    console.log("  node godaddy-dns.mjs add <name> <val>  - Add TXT record");
    console.log("");
    console.log("For Let's Encrypt DNS challenge:");
    console.log("  node godaddy-dns.mjs add _acme-challenge \"<token>\"");
  }
}
