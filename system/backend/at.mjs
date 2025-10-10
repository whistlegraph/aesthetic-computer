import { AtpAgent } from "@atproto/api";
import { shell } from "./shell.mjs";

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
