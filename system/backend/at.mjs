import { AtpAgent } from "@atproto/api";
import { shell } from "./shell.mjs";
import { userEmailFromID } from "./authorization.mjs";
import crypto from "crypto";

const DEFAULT_PDS_URL = "https://at.aesthetic.computer";

const FALLBACK_ERROR_MESSAGES = [
  "Reserved handle",
  "Invalid handle",
  "must be a valid handle",
  "Handle too short",
  "Handle already taken",
];

function getPdsUrl() {
  return process.env.PDS_URL || DEFAULT_PDS_URL;
}

function getAdminPassword() {
  return process.env.PDS_ADMIN_PASSWORD;
}

export function sanitizeHandleForPds(handle) {
  if (!handle) return handle;
  return handle.replace(/[._]/g, "-");
}

function isRetryableHandleError(error) {
  if (!error?.message) return false;
  return FALLBACK_ERROR_MESSAGES.some((msg) => error.message.includes(msg));
}

export async function updateAtprotoHandle(database, sub, handle) {
  const users = database.db.collection("users");
  const userRecord = await users.findOne({ _id: sub });

  if (!userRecord) {
    shell.log("🪪 No user record for ATProto sync:", sub);
    return { updated: false, reason: "user-missing" };
  }

  const atproto = userRecord.atproto;
  if (!atproto?.password) {
    shell.log("🪪 No ATProto credentials stored for:", sub);
    return { updated: false, reason: "missing-credentials" };
  }

  const sanitizedHandle = sanitizeHandleForPds(handle);
  const desiredHandle = `${sanitizedHandle}.at.aesthetic.computer`;
  const currentHandle = atproto.handle;
  const identifier = currentHandle || atproto.did;

  if (!identifier) {
    shell.log("🪪 No ATProto identifier available for:", sub);
    return { updated: false, reason: "missing-identifier" };
  }

  if (desiredHandle === currentHandle) {
    return {
      updated: false,
      reason: "already-synced",
      handle: desiredHandle,
      sanitized: sanitizedHandle,
    };
  }

  const agent = new AtpAgent({ service: getPdsUrl() });

  try {
    await agent.login({ identifier, password: atproto.password });
  } catch (error) {
    shell.log("🪪 Failed to login to PDS for handle sync:", error);
    return { updated: false, reason: "login-failed", error: error.message };
  }

  const performUpdate = async (targetHandle) => {
    await agent.com.atproto.identity.updateHandle({ handle: targetHandle });
    await users.updateOne(
      { _id: sub },
      {
        $set: {
          "atproto.handle": targetHandle,
          "atproto.syncedAt": new Date().toISOString(),
        },
      },
    );
    shell.log("🪪 Updated PDS handle to:", targetHandle);
    return targetHandle;
  };

  try {
    const finalHandle = await performUpdate(desiredHandle);
    return {
      updated: true,
      handle: finalHandle,
      sanitized: sanitizedHandle,
    };
  } catch (error) {
    const fallbackHandle = userRecord.code
      ? `${userRecord.code}.at.aesthetic.computer`
      : null;

    if (fallbackHandle && isRetryableHandleError(error)) {
      shell.log(
        "🪪 Falling back to user code handle for PDS:",
        fallbackHandle,
        "Reason:",
        error.message,
      );

      try {
        const finalHandle = await performUpdate(fallbackHandle);
        return {
          updated: true,
          handle: finalHandle,
          sanitized: fallbackHandle.replace(/\.at\.aesthetic\.computer$/, ""),
          fallback: true,
          error: error.message,
        };
      } catch (fallbackError) {
        shell.log("🪪 Fallback PDS handle update failed:", fallbackError);
        return {
          updated: false,
          reason: "fallback-failed",
          error: fallbackError.message,
        };
      }
    }

    shell.log("🪪 PDS handle update failed:", error);
    return { updated: false, reason: "update-failed", error: error.message };
  }
}

export async function deleteAtprotoAccount(database, sub) {
  const adminPassword = getAdminPassword();
  if (!adminPassword) {
    shell.log("🪦 Skipping PDS deletion, missing admin password env.");
    return { deleted: false, reason: "missing-admin-password" };
  }

  const users = database.db.collection("users");
  const userRecord = await users.findOne({ _id: sub });

  if (!userRecord?.atproto?.did) {
    shell.log("🪦 No ATProto DID on record for:", sub);
    return { deleted: false, reason: "missing-did" };
  }

  const did = userRecord.atproto.did;
  const auth = Buffer.from(`admin:${adminPassword}`).toString("base64");

  try {
    const response = await fetch(
      `${getPdsUrl()}/xrpc/com.atproto.admin.deleteAccount`,
      {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
          Authorization: `Basic ${auth}`,
        },
        body: JSON.stringify({ did }),
      },
    );

    if (!response.ok) {
      const errorText = await response.text();
      throw new Error(`status ${response.status}: ${errorText}`);
    }

    await users.updateOne({ _id: sub }, { $unset: { atproto: "" } });
    shell.log("🪦 Deleted ATProto account for:", sub);
    return { deleted: true };
  } catch (error) {
    shell.log("🪦 Failed to delete ATProto account:", error.message);
    return { deleted: false, reason: "request-failed", error: error.message };
  }
}

async function generateInviteCode() {
  const adminPassword = getAdminPassword();
  if (!adminPassword) {
    throw new Error("PDS_ADMIN_PASSWORD environment variable is required");
  }

  const auth = Buffer.from(`admin:${adminPassword}`).toString("base64");

  const response = await fetch(
    `${getPdsUrl()}/xrpc/com.atproto.server.createInviteCode`,
    {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        Authorization: `Basic ${auth}`,
      },
      body: JSON.stringify({ useCount: 1 }),
    },
  );

  if (!response.ok) {
    throw new Error(`Failed to create invite code: ${response.statusText}`);
  }

  const data = await response.json();
  return data.code;
}

function generatePassword() {
  const chars =
    "ABCDEFGHJKLMNPQRSTUVWXYZabcdefghjkmnpqrstuvwxyz23456789!@#$%&*";
  let password = "";
  for (let i = 0; i < 32; i++) {
    const randomIndex = crypto.randomInt(0, chars.length);
    password += chars[randomIndex];
  }
  return password;
}

export async function createAtprotoAccount(database, sub, email) {
  try {
    shell.log(`🦋 Creating ATProto account for: ${sub}`);

    const users = database.db.collection("users");
    const handles = database.db.collection("@handles");

    // Check if account already exists
    const existingUser = await users.findOne({ _id: sub });
    if (existingUser?.atproto?.did) {
      shell.log(`🦋 ATProto account already exists for: ${sub}`);
      return {
        created: false,
        reason: "already-exists",
        did: existingUser.atproto.did,
        handle: existingUser.atproto.handle,
      };
    }

    // Get email if not provided
    if (!email) {
      const tenant = sub.startsWith("sotce-") ? "sotce" : "aesthetic";
      const result = await userEmailFromID(sub, tenant);
      if (!result?.email) {
        throw new Error(`No email found for user: ${sub}`);
      }
      email = result.email;
    }

    shell.log(`📧 Email: ${email}`);

    // Generate password
    const password = generatePassword();

    // Determine handle from AC handle or user code
    const handleRecord = await handles.findOne({ _id: sub });
    let pdsHandle;
    if (handleRecord?.handle) {
      const sanitizedHandle = sanitizeHandleForPds(handleRecord.handle);
      pdsHandle = `${sanitizedHandle}.at.aesthetic.computer`;
      shell.log(`🏷️  Using AC handle: ${pdsHandle}`);
    } else if (existingUser?.code) {
      pdsHandle = `${existingUser.code}.at.aesthetic.computer`;
      shell.log(`🏷️  Using user code: ${pdsHandle}`);
    } else {
      throw new Error(`No handle or user code found for: ${sub}`);
    }

    // Generate invite code
    shell.log(`🎫 Generating invite code...`);
    const inviteCode = await generateInviteCode();

    // Create account on PDS
    const agent = new AtpAgent({ service: getPdsUrl() });
    const tenant = sub.startsWith("sotce-") ? "sotce" : "aesthetic";

    let accountCreated = false;
    let finalDid, finalHandle;
    let currentEmail = email;
    let attempts = 0;

    while (!accountCreated && attempts < 3) {
      attempts++;

      try {
        const response = await agent.createAccount({
          email: currentEmail,
          handle: pdsHandle,
          password,
          inviteCode,
        });

        finalDid = response.data.did;
        finalHandle = response.data.handle;
        accountCreated = true;
      } catch (error) {
        // Handle duplicate email by appending tenant
        if (
          error.message.includes("Email already taken") &&
          attempts === 1 &&
          tenant === "sotce"
        ) {
          shell.log(`⚠️  Email "${currentEmail}" already taken, adding +sotce`);
          const [localPart, domain] = currentEmail.split("@");
          currentEmail = `${localPart}+sotce@${domain}`;
        }
        // Try fallback to user code if handle fails
        else if (
          isRetryableHandleError(error) &&
          attempts <= 2 &&
          existingUser?.code
        ) {
          shell.log(`⚠️  Handle failed, falling back to user code`);
          pdsHandle = `${existingUser.code}.at.aesthetic.computer`;
        } else {
          throw error;
        }
      }
    }

    if (!accountCreated) {
      throw new Error("Failed to create account after all attempts");
    }

    shell.log(`✅ Created ATProto account: ${finalDid}`);

    // Store atproto data in MongoDB
    const atprotoData = {
      did: finalDid,
      handle: finalHandle,
      password: password,
      createdAt: new Date().toISOString(),
    };

    await users.updateOne(
      { _id: sub },
      { $set: { atproto: atprotoData } },
      { upsert: true },
    );

    return {
      created: true,
      did: finalDid,
      handle: finalHandle,
      email: currentEmail,
    };
  } catch (error) {
    shell.log(`❌ Failed to create ATProto account: ${error.message}`);
    return {
      created: false,
      reason: "creation-failed",
      error: error.message,
    };
  }
}
