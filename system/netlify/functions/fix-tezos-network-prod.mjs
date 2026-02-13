// fix-tezos-network-prod.mjs - One-time function to fix tezos-kidlisp network in production
// DELETE THIS FILE AFTER RUNNING ONCE

import { connect } from "../../backend/database.mjs";
import { hasAdmin } from "../../backend/authorization.mjs";

export async function handler(event) {
  // Require admin access
  const authHeader = event.headers.authorization;
  if (!authHeader) {
    return {
      statusCode: 401,
      body: JSON.stringify({ error: "Authorization required" })
    };
  }

  const token = authHeader.replace(/^Bearer /, "");
  const isAdmin = await hasAdmin(token);

  if (!isAdmin) {
    return {
      statusCode: 403,
      body: JSON.stringify({ error: "Admin access required" })
    };
  }

  try {
    const database = await connect();
    const secrets = database.db.collection("secrets");

    // Check current value
    const current = await secrets.findOne({ _id: "tezos-kidlisp" });

    const log = [];
    log.push("Current configuration:");
    log.push(`  - address: ${current.address}`);
    log.push(`  - network: ${current.network}`);

    if (current.network === "mainnet") {
      log.push("\n✅ Already set to mainnet!");
      await database.disconnect();
      return {
        statusCode: 200,
        body: JSON.stringify({ message: log.join("\n"), changed: false })
      };
    }

    // Update to mainnet
    log.push("\n🔧 Updating network from 'ghostnet' to 'mainnet'...");

    const result = await secrets.updateOne(
      { _id: "tezos-kidlisp" },
      { $set: { network: "mainnet" } }
    );

    if (result.modifiedCount > 0) {
      log.push("✅ Successfully updated!");

      // Verify
      const updated = await secrets.findOne({ _id: "tezos-kidlisp" });
      log.push("\nVerified:");
      log.push(`  - address: ${updated.address}`);
      log.push(`  - network: ${updated.network}`);
    }

    await database.disconnect();

    return {
      statusCode: 200,
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        message: log.join("\n"),
        changed: true,
        result: result.modifiedCount
      })
    };
  } catch (error) {
    return {
      statusCode: 500,
      body: JSON.stringify({ error: error.message })
    };
  }
}
