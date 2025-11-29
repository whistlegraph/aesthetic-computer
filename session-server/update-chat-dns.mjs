#!/usr/bin/env node
// Update Chat DNS, 25.11.28
// Updates Cloudflare DNS records to point chat subdomains to session-server

import dotenv from "dotenv";
dotenv.config({ path: "../aesthetic-computer-vault/nanos/conductor.env" });

const CLOUDFLARE_EMAIL = process.env.CLOUDFLARE_EMAIL;
const CLOUDFLARE_API_TOKEN = process.env.CLOUDFLARE_API_TOKEN;
const CLOUDFLARE_BASE_URL = "https://api.cloudflare.com/client/v4";

// Session server IP (DigitalOcean)
const SESSION_SERVER_IP = "157.245.134.225";

const headers = {
  "X-Auth-Email": CLOUDFLARE_EMAIL,
  "X-Auth-Key": CLOUDFLARE_API_TOKEN,
  "Content-Type": "application/json",
};

// Chat subdomains to migrate
const chatDomains = [
  { subdomain: "chat-system.aesthetic.computer", zone: "aesthetic.computer" },
  { subdomain: "chat-clock.aesthetic.computer", zone: "aesthetic.computer" },
  { subdomain: "chat.sotce.net", zone: "sotce.net" },
];

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

async function createOrUpdateARecord(subdomain, zone, ip) {
  console.log(`\n🔄 Processing ${subdomain}...`);
  
  const zoneId = (await fetchZone(zone))?.id;
  if (!zoneId) {
    console.error(`  ❌ Zone ID not found for ${zone}`);
    return false;
  }
  console.log(`  📍 Zone ID: ${zoneId}`);

  const recordResponse = await fetchARecord(zoneId, subdomain);
  const record = recordResponse.result?.[0];

  const data = {
    type: "A",
    name: subdomain,
    content: ip,
    ttl: 1, // Auto
    proxied: true,
  };

  let response;
  if (record) {
    console.log(`  📝 Updating existing record (current: ${record.content})`);
    response = await updateDNSRecord(zoneId, record.id, data);
  } else {
    console.log(`  ➕ Creating new record`);
    response = await createDNSRecord(zoneId, data);
  }

  if (response.success) {
    console.log(`  ✅ Success: ${subdomain} -> ${ip}`);
    return true;
  } else {
    console.log(`  ❌ Failed:`, response.errors);
    return false;
  }
}

async function main() {
  console.log("🌐 Chat DNS Migration Tool");
  console.log("==========================");
  console.log(`Target IP: ${SESSION_SERVER_IP}`);
  
  if (!CLOUDFLARE_EMAIL || !CLOUDFLARE_API_TOKEN) {
    console.error("\n❌ Missing Cloudflare credentials!");
    console.log("Make sure aesthetic-computer-vault/nanos/conductor.env exists with:");
    console.log("  CLOUDFLARE_EMAIL=...");
    console.log("  CLOUDFLARE_API_TOKEN=...");
    process.exit(1);
  }

  console.log(`\nCloudflare Email: ${CLOUDFLARE_EMAIL}`);

  const dryRun = process.argv.includes("--dry-run");
  if (dryRun) {
    console.log("\n⚠️  DRY RUN MODE - No changes will be made\n");
  }

  let successCount = 0;
  let failCount = 0;

  for (const { subdomain, zone } of chatDomains) {
    if (dryRun) {
      console.log(`\n🔍 Would update: ${subdomain} -> ${SESSION_SERVER_IP}`);
      successCount++;
    } else {
      const success = await createOrUpdateARecord(subdomain, zone, SESSION_SERVER_IP);
      if (success) successCount++;
      else failCount++;
    }
  }

  console.log("\n==========================");
  console.log(`✅ Success: ${successCount}`);
  console.log(`❌ Failed: ${failCount}`);
  
  if (!dryRun && successCount === chatDomains.length) {
    console.log("\n🎉 All DNS records updated successfully!");
    console.log("\n⏳ DNS propagation may take a few minutes.");
    console.log("Test with: curl https://chat-system.aesthetic.computer");
  }
}

main().catch(console.error);
