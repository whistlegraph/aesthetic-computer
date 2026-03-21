#!/usr/bin/env node
// Configure Cloudflare DNS for PDS deployment
// Creates A records for at.aesthetic.computer and *.at.aesthetic.computer

import { readFileSync } from "fs";
import { resolve } from "path";

// Load Cloudflare credentials from vault
const vaultEnvPath = resolve(
  process.env.HOME,
  "aesthetic-computer/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env"
);

// Parse environment file manually
const envContent = readFileSync(vaultEnvPath, "utf-8");
const env = {};
envContent.split("\n").forEach((line) => {
  const match = line.match(/^([^#=]+)=(.*)$/);
  if (match) {
    const key = match[1].trim();
    let value = match[2].trim();
    // Remove quotes if present
    if ((value.startsWith('"') && value.endsWith('"')) || 
        (value.startsWith("'") && value.endsWith("'"))) {
      value = value.slice(1, -1);
    }
    env[key] = value;
  }
});

const CLOUDFLARE_EMAIL = env.CLOUDFLARE_EMAIL;
const CLOUDFLARE_API_KEY = env.CLOUDFLARE_API_KEY;
const CLOUDFLARE_API_TOKEN = env.CLOUDFLARE_API_TOKEN;
const CLOUDFLARE_BASE_URL = "https://api.cloudflare.com/client/v4";

if (!CLOUDFLARE_EMAIL || !CLOUDFLARE_API_KEY) {
  console.error("❌ Missing Cloudflare credentials in vault");
  console.error("   Expected in:", vaultEnvPath);
  process.exit(1);
}

// Use X-Auth-Email and X-Auth-Key authentication (same as conductor.mjs)
// Note: Use API_KEY not API_TOKEN for this authentication method
const headers = {
  "X-Auth-Email": CLOUDFLARE_EMAIL,
  "X-Auth-Key": CLOUDFLARE_API_KEY,
  "Content-Type": "application/json",
};

// Read droplet IP from file
const dropletIP = readFileSync(
  resolve(import.meta.dirname, ".droplet_ip"),
  "utf-8"
).trim();

console.log("📡 Configuring DNS for PDS deployment");
console.log(`   Domain: at.aesthetic.computer`);
console.log(`   IP: ${dropletIP}`);
console.log("");

async function fetchZones() {
  const response = await fetch(`${CLOUDFLARE_BASE_URL}/zones`, { headers });
  return response.json();
}

async function fetchZone(zoneName) {
  const zones = await fetchZones();
  return zones.result?.find((zone) => zone.name === zoneName);
}

async function fetchARecord(zoneId, recordName) {
  const response = await fetch(
    `${CLOUDFLARE_BASE_URL}/zones/${zoneId}/dns_records?type=A&name=${recordName}`,
    { headers }
  );
  return response.json();
}

async function updateDNSRecord(zoneId, recordId, data) {
  const response = await fetch(
    `${CLOUDFLARE_BASE_URL}/zones/${zoneId}/dns_records/${recordId}`,
    { method: "PUT", headers, body: JSON.stringify(data) }
  );
  return response.json();
}

async function createDNSRecord(zoneId, data) {
  const response = await fetch(
    `${CLOUDFLARE_BASE_URL}/zones/${zoneId}/dns_records`,
    { method: "POST", headers, body: JSON.stringify(data) }
  );
  return response.json();
}

async function createOrUpdateARecord(subdomain, ip) {
  const rootDomain = "aesthetic.computer";
  const zoneId = (await fetchZone(rootDomain))?.id;

  if (!zoneId) {
    console.error(`❌ Zone ID not found for ${rootDomain}`);
    return false;
  }

  const recordResponse = await fetchARecord(zoneId, subdomain);
  const record = recordResponse.result?.[0];

  const data = {
    type: "A",
    name: subdomain,
    content: ip,
    ttl: 600,
    proxied: false, // Don't proxy PDS traffic through Cloudflare
  };

  if (record) {
    // Update existing record
    if (record.content === ip) {
      console.log(`✓ ${subdomain} already points to ${ip}`);
      return true;
    }
    
    console.log(`ℹ Updating ${subdomain}: ${record.content} → ${ip}`);
    const result = await updateDNSRecord(zoneId, record.id, data);
    
    if (result.success) {
      console.log(`✓ Updated ${subdomain} → ${ip}`);
      return true;
    } else {
      console.error(`❌ Failed to update ${subdomain}:`, result.errors);
      return false;
    }
  } else {
    // Create new record
    console.log(`ℹ Creating new A record: ${subdomain} → ${ip}`);
    const result = await createDNSRecord(zoneId, data);
    
    if (result.success) {
      console.log(`✓ Created ${subdomain} → ${ip}`);
      return true;
    } else {
      console.error(`❌ Failed to create ${subdomain}:`, result.errors);
      return false;
    }
  }
}

async function configureDNS() {
  try {
    // Create/update main PDS record
    const mainSuccess = await createOrUpdateARecord(
      "at.aesthetic.computer",
      dropletIP
    );
    
    // Create/update wildcard record for user handles
    const wildcardSuccess = await createOrUpdateARecord(
      "*.at.aesthetic.computer",
      dropletIP
    );
    
    if (mainSuccess && wildcardSuccess) {
      console.log("");
      console.log("✅ DNS configuration complete!");
      console.log("   Main: at.aesthetic.computer → " + dropletIP);
      console.log("   Wildcard: *.at.aesthetic.computer → " + dropletIP);
      console.log("");
      console.log("⏱️  DNS propagation may take a few minutes...");
      return true;
    } else {
      console.error("");
      console.error("❌ DNS configuration failed");
      return false;
    }
  } catch (error) {
    console.error("❌ Error configuring DNS:", error.message);
    return false;
  }
}

// Run the configuration
const success = await configureDNS();
process.exit(success ? 0 : 1);
