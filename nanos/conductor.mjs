// Conductor, 24.03.31.14.01
// Orchestrates Cloudlfare DNS updates and deployment of unikernel servers
// on GCP that they point to. 

/* #region 🏁 TODO 
  - [] Add a function for making an image as well.
  - [] Spawn a GCP instance from this script / run 'ops'. 
#endregion */

const CLOUDFLARE_EMAIL = process.env.CLOUDFLARE_EMAIL;
const CLOUDFLARE_API_TOKEN = process.env.CLOUDFLARE_API_TOKEN;
const CLOUDFLARE_BASE_URL = "https://api.cloudflare.com/client/v4";

const headers = {
  "X-Auth-Email": CLOUDFLARE_EMAIL,
  "X-Auth-Key": CLOUDFLARE_API_TOKEN,
  "Content-Type": "application/json",
};

async function fetchZones() {
  const response = await fetch(`${CLOUDFLARE_BASE_URL}/zones`, { headers });
  return response.json();
}

async function fetchZone(zoneName) {
  const zones = await fetchZones();
  return zones.result.find((zone) => zone.name === zoneName);
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
    {
      method: "PUT",
      headers,
      body: JSON.stringify(data),
    },
  );
  return response.json();
}

async function update(sub = "chat.aesthetic.computer", ip = "3.3.3.3") {
  const zoneId = (await fetchZone("aesthetic.computer")).id;
  const record = await fetchARecord(zoneId, sub);
  if (!record.result?.[0]) {
    console.log("No result found for:", sub);
    return; // Exist if no result.
  }
  const recordId = record.result[0].id;
  console.log(zoneId, recordId);

  // TODO: What data to send here?
  // TODO: Take it from the npm scripts.
  const data = {
    type: "A",
    name: sub,
    content: ip, // IP address the record should point to
    ttl: 1, // Time to live for DNS record
    proxied: true,
  };
  const response = await updateDNSRecord(zoneId, recordId, data);
  if (response.success) {
    console.log("🌟 Updated the record for:", sub, "to:", ip);
  } else {
    console.log("🔴 Failed to update:", sub, "to:", ip);
  }
}

await update();