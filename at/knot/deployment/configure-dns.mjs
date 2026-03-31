#!/usr/bin/env node
// Configure Cloudflare DNS for Tangled Knot deployment
// Creates A record for knot.aesthetic.computer

import { readFileSync } from "fs";
import { resolve } from "path";

const vaultEnvPath = resolve(
  process.env.HOME,
  "aesthetic-computer/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env",
);

const envContent = readFileSync(vaultEnvPath, "utf-8");
const env = {};
envContent.split("\n").forEach((line) => {
  const match = line.match(/^([^#=]+)=(.*)$/);
  if (match) {
    const key = match[1].trim();
    let value = match[2].trim();
    if (
      (value.startsWith('"') && value.endsWith('"')) ||
      (value.startsWith("'") && value.endsWith("'"))
    ) {
      value = value.slice(1, -1);
    }
    env[key] = value;
  }
});

const CLOUDFLARE_EMAIL = env.CLOUDFLARE_EMAIL;
const CLOUDFLARE_API_KEY = env.CLOUDFLARE_API_KEY;
const CLOUDFLARE_BASE_URL = "https://api.cloudflare.com/client/v4";

if (!CLOUDFLARE_EMAIL || !CLOUDFLARE_API_KEY) {
  console.error("x Missing Cloudflare credentials in vault");
  console.error("  Expected in:", vaultEnvPath);
  process.exit(1);
}

const headers = {
  "X-Auth-Email": CLOUDFLARE_EMAIL,
  "X-Auth-Key": CLOUDFLARE_API_KEY,
  "Content-Type": "application/json",
};

const dropletIP = readFileSync(
  resolve(import.meta.dirname, ".droplet_ip"),
  "utf-8",
).trim();

console.log("Configuring DNS for Tangled Knot");
console.log(`  Domain: knot.aesthetic.computer`);
console.log(`  IP: ${dropletIP}`);
console.log("");

async function fetchZone(zoneName) {
  const response = await fetch(`${CLOUDFLARE_BASE_URL}/zones`, { headers });
  const zones = await response.json();
  return zones.result?.find((zone) => zone.name === zoneName);
}

async function fetchARecord(zoneId, recordName) {
  const response = await fetch(
    `${CLOUDFLARE_BASE_URL}/zones/${zoneId}/dns_records?type=A&name=${recordName}`,
    { headers },
  );
  return response.json();
}

async function updateDNSRecord(zoneId, recordId, data) {
  const response = await fetch(
    `${CLOUDFLARE_BASE_URL}/zones/${zoneId}/dns_records/${recordId}`,
    { method: "PUT", headers, body: JSON.stringify(data) },
  );
  return response.json();
}

async function createDNSRecord(zoneId, data) {
  const response = await fetch(
    `${CLOUDFLARE_BASE_URL}/zones/${zoneId}/dns_records`,
    { method: "POST", headers, body: JSON.stringify(data) },
  );
  return response.json();
}

async function createOrUpdateARecord(subdomain, ip) {
  const rootDomain = "aesthetic.computer";
  const zoneId = (await fetchZone(rootDomain))?.id;

  if (!zoneId) {
    console.error(`x Zone ID not found for ${rootDomain}`);
    return false;
  }

  const recordResponse = await fetchARecord(zoneId, subdomain);
  const record = recordResponse.result?.[0];

  const data = {
    type: "A",
    name: subdomain,
    content: ip,
    ttl: 600,
    proxied: false, // Direct connection for git SSH
  };

  if (record) {
    if (record.content === ip) {
      console.log(`✓ ${subdomain} already points to ${ip}`);
      return true;
    }
    console.log(`i Updating ${subdomain}: ${record.content} -> ${ip}`);
    const result = await updateDNSRecord(zoneId, record.id, data);
    if (result.success) {
      console.log(`✓ Updated ${subdomain} -> ${ip}`);
      return true;
    } else {
      console.error(`x Failed to update ${subdomain}:`, result.errors);
      return false;
    }
  } else {
    console.log(`i Creating A record: ${subdomain} -> ${ip}`);
    const result = await createDNSRecord(zoneId, data);
    if (result.success) {
      console.log(`✓ Created ${subdomain} -> ${ip}`);
      return true;
    } else {
      console.error(`x Failed to create ${subdomain}:`, result.errors);
      return false;
    }
  }
}

const success = await createOrUpdateARecord("knot.aesthetic.computer", dropletIP);
if (success) {
  console.log("");
  console.log("DNS configured: knot.aesthetic.computer -> " + dropletIP);
} else {
  console.error("DNS configuration failed");
}
process.exit(success ? 0 : 1);
