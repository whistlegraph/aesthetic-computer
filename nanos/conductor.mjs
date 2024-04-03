// Conductor, 24.03.31.14.01
// Orchestrates Cloudlfare DNS updates and deployment of unikernel servers
// on GCP that they point to.

/* #region 🏁 TODO
  - [] A. Chat System
    - [] Can be a single instance for now.
    - [] Sub is prefixed with chat-system.
      - [] The database connects using the sub to make a record.
      - [] It's a simple websocket connection.

  - [] Deploy chat.
        
  - 🌟 
  - [] B. Future Multi-Instance Management / 
          Check to see if an instance exists under the label.
          If it does, then just return that instance.
          Otherwise make a new instance or request one to be unpaused.
    - [] Adapt conductor to be an http api.
    - [] Does the instance already exist at the requested url?
      - [] Yes!
        - [] Return information that the client can connect / success.
      - [] No...
        - [] Unpause an instance / boot an instance from the pool.
        - [] Assign DNS to it.
        - [] Return information that the client can connect / success.

  - [] How will HTTP traffic be blocked / the ip be masked?
#endregion */

import { spawn } from "child_process";
import { MongoClient } from "mongodb";

import "dotenv/config";

// #region 📊 Configuration
const CLOUDFLARE_EMAIL = process.env.CLOUDFLARE_EMAIL;
const CLOUDFLARE_API_TOKEN = process.env.CLOUDFLARE_API_TOKEN;
const CLOUDFLARE_BASE_URL = "https://api.cloudflare.com/client/v4";

const MONGODB_CONNECTION_STRING = process.env.MONGODB_CONNECTION_STRING;
const MONGODB_NAME = process.env.MONGODB_NAME;

const headers = {
  "X-Auth-Email": CLOUDFLARE_EMAIL,
  "X-Auth-Key": CLOUDFLARE_API_TOKEN,
  "Content-Type": "application/json",
};
// #endregion

async function deploy() {
  const out = await gcpDeploy(process.argv[2], process.argv[3] === "false" ? false : true);

  const parsed = JSON.parse(out);
  const ip = parsed[0]?.PublicIps?.[0];
  if (ip) {
    console.log("🧲 Instance IP:", ip);
    const sub = `${process.argv[2] || "chat"}.aesthetic.computer`;
    if (await createOrUpdateARecord(sub, ip)) {
      // Retry `curl` for about 5 seconds

      async function attempt(command) {
        const maxAttempts = 15;
        let attempts = 0, success = false;

        while (attempts < maxAttempts && !success) {
          try {
            await command();
            success = true; // If curl succeeds, set success to true
            console.log("\n");
          } catch (error) {
            attempts += 1;
            console.error("🔴 Attempt", attempts, "failed:", error);
            if (attempts < maxAttempts) {
              await new Promise((resolve) => setTimeout(resolve, 1000));
            }
          }
        }

        if (!success) {
          console.error("🔴 Failed after", maxAttempts, "attempts");
          return;
        }
      }

      console.log("💻 Testing HTTP connection...");

      await attempt(async () => {
        await run("curl", ["-s", `https://${sub}`]);
      });

      console.log("🧦 Testing WebSocket connection...");

      await attempt(async () => {
        const websocatCommand = `echo "hello" | websocat -1 wss://${sub}`;
        await run("bash", ["-c", websocatCommand]);
      });

    } else {
      console.error("🔴 Failed to update A record for instance:", sub, "->", ip);
      // This should also maybe destroy the instance in certain conditions.  
      return;
    }
  }
}

// 🌟 Initialization

try {
  const start = new Date();
  const startMillis = performance.now();
  console.log("-> Starting deployment:", start);
  await deploy();
  const end = new Date();
  const endMillis = performance.now();
  console.log("-> Completed:", end);
  console.log("-> Elapsed:", (endMillis - startMillis) / 1000, "seconds");
} catch (err) {
  console.error("🔴 Deploy failed:", err);
}

// #region 🌥️ Cloudflare

async function fetchZones() {
  const response = await fetch(`${CLOUDFLARE_BASE_URL}/zones`, { headers });
  return response.json();
}

async function fetchZone(zoneName) {
  const zones = await fetchZones();
  // console.log("Zones:", zones);
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

async function createOrUpdateARecord(
  sub = "chat.aesthetic.computer",
  ip = "3.3.3.3",
) {
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
      `🟢 Success: ${record ? "Updated" : "Created"} the record for ${sub}`,
    );
    return true;
  } else {
    console.log(
      `🔴 Failed to ${record ? "update" : "create"} the record for ${sub}`,
    );
    return false;
  }
}

// #endregion

// #region 🗺️ MongoDB

async function makeMongoConnection() {
  const client = new MongoClient(MONGODB_CONNECTION_STRING);
  await client.connect();
  const db = client.db(MONGODB_NAME);
  return { client, db }
}

async function storeImageNameInMongo(imageId) {
  const { client, db } = await makeMongoConnection();

  // Use an upsert operation to ensure only one record is stored
  const result = await db
    .collection("servers")
    .updateOne(
      { _id: "lastImage" }, { $set: { name: imageId } }, { upsert: true },
    );

  console.log(result);
  console.log("📥 Image name cached in MongoDB:", imageId);
  await client.close();
}

async function getImageIdFromMongo() {
  const { client, db } = await makeMongoConnection();

  const result = await db.collection("servers").findOne({ _id: "lastImage" });

  await client.close();
  return result ? result.name : null;
}

// #endregion

// #region 📚 Google Cloud

async function gcpDeploy(instanceName, newImage) {
  // A. Image Deployment
  // Come up with a unique timestamp identifier string for the "current" image.

  // Destroy any running instances. (Will cause down-time)
  // await gcpDestroyInstance(instanceName);
  let imageName;

  if (newImage) {
    imageName = "aesthetic-" + new Date().getTime();
    await gcpPublishImage(imageName); // Upload new image.
    await storeImageNameInMongo(imageName); // Store the image name in MongoDB.
  } else {
    imageName = await getImageIdFromMongo(); // Try and retrieve from MongoDB...
    if (!imageName) {
      console.log("🟡 No existing image found... publishing a new one.");
      return await gcpDeploy(instanceName);
    }
    console.log("🗺️ Retrieved image identifier from MongoDB:", imageName);
  }

  console.log("🟡 Deploying instance...", instanceName);
  // Create this new instance.
  await run("ops", [
    "instance",
    "create",
    imageName,
    "-t",
    "gcp",
    "-c",
    "config-gcp.json",
    "-i",
    instanceName,
  ]);

  console.log("🟡 Deploy success:", instanceName);

  // List instances.
  return await run("ops", [
    "instance",
    "list",
    "-j",
    "-t",
    "gcp",
    "-c",
    "config-gcp.json",
  ]);
}

async function gcpPublishImage(imageName) {
  // console.log("🟡 Deleting existing image...");
  // Delete existing image.
  // try {
  //   await run("ops", [
  //     "image",
  //     "delete",
  //     "aesthetic-chat",
  //     "-t",
  //     "gcp",
  //     "-c",
  //     "config-gcp.json",
  //     "--assume-yes",
  //   ]);
  // } catch (err) {
  //   console.log("🔴 Could not delete image:", err);
  // }

  // Upload new image.
  console.log("📥️ Publishing new image:", imageName);
  await run("ops", [
    "image",
    "create",
    "-c",
    "config-gcp.json",
    "--package",
    "eyberg/node:20.5.0",
    "-a",
    "chat.mjs",
    "-i",
    imageName,
    "-t",
    "gcp",
  ]);

  // List images.
  await run("ops", ["image", "list", "-t", "gcp", "-c", "config-gcp.json"]);
}

async function gcpDestroyInstance(instanceName) {
  console.log("🟡 Deleting instance:", instanceName);
  try {
    await run("ops", [
      "instance",
      "delete",
      instanceName,
      "-t",
      "gcp",
      "-c",
      "config-gcp.json",
    ]);
  } catch (err) {
    console.error("🔴 Could not delete instance:", err);
  }
}

// async function rebootInstances() {
// TODO: I would need to find all instances running this image, and then
//       re-initialize / rebuild them?
// }

//#endregion

// ⚙️  Utilities

function run(command, args = []) {
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
      if (code === 0) {
        resolve(lastOut); // Resolve the promise with lastOut
      } else {
        reject(`Child process exit code ${code}`);
      }
    });
  });
}
