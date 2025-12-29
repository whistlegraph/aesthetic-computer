#!/usr/bin/env node
/**
 * Let's Encrypt certificate generator for thomaslawson.com
 * Uses DNS-01 challenge (manual mode)
 */

import { spawn } from "child_process";
import { readFileSync, existsSync } from "fs";
import { join } from "path";

const DOMAIN = "thomaslawson.com";
const CERT_DIR = "/workspaces/aesthetic-computer/aesthetic-computer-vault/gigs/thomaslawson.com/ssl";

async function runCertbot() {
  console.log("Starting certbot for", DOMAIN);
  console.log("This will use DNS-01 challenge - you will need to add a TXT record to DNS");
  console.log("");
  
  const proc = spawn("certbot", [
    "certonly",
    "--manual",
    "--preferred-challenges", "dns",
    "-d", DOMAIN,
    "-d", `www.${DOMAIN}`,
    "--config-dir", CERT_DIR,
    "--work-dir", `${CERT_DIR}/work`,
    "--logs-dir", `${CERT_DIR}/logs`,
    "--agree-tos",
    "--email", "hi@aesthetic.computer"
  ], {
    stdio: "inherit"
  });
  
  return new Promise((resolve, reject) => {
    proc.on("close", code => {
      if (code === 0) resolve();
      else reject(new Error(`certbot exited with code ${code}`));
    });
  });
}

function showCerts() {
  const livePath = join(CERT_DIR, "live", DOMAIN);
  if (!existsSync(livePath)) {
    console.log("No certs found at", livePath);
    return;
  }
  
  console.log("Certificates generated:");
  console.log("  Certificate:", join(livePath, "cert.pem"));
  console.log("  Private Key:", join(livePath, "privkey.pem"));
  console.log("  Full Chain:", join(livePath, "fullchain.pem"));
  console.log("  CA Bundle:", join(livePath, "chain.pem"));
}

const cmd = process.argv[2];

if (cmd === "generate") {
  await runCertbot();
  showCerts();
} else if (cmd === "show") {
  showCerts();
} else {
  console.log("Usage:");
  console.log("  node letsencrypt.mjs generate  - Generate new certificate");
  console.log("  node letsencrypt.mjs show      - Show existing cert paths");
}
