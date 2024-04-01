// Conductor, 24.03.31.14.01
// Orchestrates Cloudlfare DNS updates and deployment of unikernel servers
// on GCP that they point to.

/* #region 🏁 TODO 
  - [] Add a function for making an image as well.
  - [] Spawn a GCP instance from this script / run 'ops'. 
  - [] Remodel this script so it can be deployed in online from a container /
       just run it off the old udp server?
#endregion */

import { exec, spawn } from "child_process";
import "dotenv/config";

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
  console.log("Zones:", zones);
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
    {
      method: "PUT",
      headers,
      body: JSON.stringify(data),
    },
  );
  return response.json();
}

async function createDNSRecord(zoneId, data) {
  const response = await fetch(
    `${CLOUDFLARE_BASE_URL}/zones/${zoneId}/dns_records`,
    {
      method: "POST",
      headers,
      body: JSON.stringify(data),
    },
  );
  return response.json();
}

async function update(sub = "chat.aesthetic.computer", ip = "3.3.3.3") {
  const zoneId = (await fetchZone("aesthetic.computer"))?.id;
  if (!zoneId) {
    console.error("🔴 Zone ID not found for aesthetic.computer. Halting.");
    return;
  }
  const recordResponse = await fetchARecord(zoneId, sub);
  const record = recordResponse.result?.[0];

  const data = {
    type: "A",
    name: sub,
    content: ip,
    ttl: 1,
    proxied: true,
  };

  let response;

  if (record) {
    console.log(`🟡 Updating existing record for ${sub}`);
    response = await updateDNSRecord(zoneId, record.id, data);
  } else {
    console.log(`🟡 Creating new record for ${sub}`);
    response = await createDNSRecord(zoneId, data);
  }

  if (response.success) {
    console.log(
      `🌟 Success: ${record ? "Updated" : "Created"} the record for ${sub}`,
    );
    return true;
  } else {
    console.log(
      `🔴 Failed to ${record ? "update" : "create"} the record for ${sub}`,
    );
    return false;
  }
}

function executeCommand(command) {
  return new Promise((resolve, reject) => {
    exec(command, (error, stdout, stderr) => {
      if (error) {
        reject(`Error: ${error.message}`);
      }
      if (stderr) {
        reject(`Stderr: ${stderr}`);
      }
      resolve(stdout);
    });
  });
}

function streamCommand(command, args = []) {
  return new Promise((resolve, reject) => {
    const childProcess = spawn(command, args, { shell: true });
    let lastOut = "";

    childProcess.stdout.on("data", (data) => {
      process.stdout.write(data);
      lastOut += data.toString(); // Append the data to lastOut
    });

    childProcess.stderr.on("data", (data) => {
      console.error(`🔴 ${data}`);
    });

    childProcess.on("close", (code) => {
      console.log(`🛑 Child process exited with code ${code}`);

      if (code === 0) {
        console.log("🟡 Deploy success...");
        resolve(lastOut); // Resolve the promise with lastOut
      } else {
        reject(`Child process exited with code ${code}`);
      }
    });
  });
}

// GCP

async function gcpDeploy(instanceName) {
  await gcpDestroy(instanceName);
  await streamCommand("ops", [
    "image",
    "create",
    "-c",
    "config-gcp.json",
    "--package",
    "eyberg/node:20.5.0",
    "-a",
    "chat.mjs",
    "-i",
    "aesthetic-chat",
    "-t",
    "gcp",
  ]);
  await streamCommand("ops", [
    "instance",
    "create",
    "aesthetic-chat",
    "-t",
    "gcp",
    "-c",
    "config-gcp.json",
    "-i",
    instanceName,
  ]);
  await streamCommand("ops", [
    "image",
    "list",
    "-t",
    "gcp",
    "-c",
    "config-gcp.json",
  ]);
  await streamCommand("ops", [
    "instance",
    "list",
    "-j",
    "-t",
    "gcp",
    "-c",
    "config-gcp.json",
  ]);
}

async function gcpDestroy(instanceName) {
  await streamCommand("ops", [
    "instance",
    "delete",
    instanceName,
    "-t",
    "gcp",
    "-c",
    "config-gcp.json",
  ]);
  await streamCommand("ops", [
    "image",
    "delete",
    "aesthetic-chat",
    "-t",
    "gcp",
    "-c",
    "config-gcp.json",
    "--assume-yes",
  ]);
}

async function deploy() {
  // const out = await streamCommand("npm", [
  //   "run",
  //   "gcp:deploy",
  //   process.argv[2],
  // ]);

  const out = await gcpDeploy(process.argv[2]);

  console.log("🟡 Deploy success...");
  const parsed = JSON.parse(out);
  const ip = parsed[0]?.PublicIps?.[0];
  if (ip) {
    console.log("🟢 IP:", ip);
    const sub = `${process.argv[2] || "chat"}.aesthetic.computer`;
    if (await update(sub, ip)) {
      // TODO: Now try and curl and websocat.
      console.log(`curl https://${sub}`);
      console.log(`websocat wss://${sub}`);
      try {
        const curlOutput = await streamCommand("curl", [`https://${sub}`]);
        console.log("curl output:", curlOutput);

        const websocatOutput = await streamCommand("websocat", [`wss://${sub}`]);
        console.log("websocat output:", websocatOutput);
      } catch (error) {
        console.error("Command failed:", error);
      }
    } else {
      // Or die if the record update fails?
      // This should probably also destroy the instance...
      return;
    }
  }

  // TODO: This needs to be divided into a series of steps.
}

// Call the function
try {
  await deploy();
} catch (err) {
  console.error("🔴 Deploy failed:", err);
}
