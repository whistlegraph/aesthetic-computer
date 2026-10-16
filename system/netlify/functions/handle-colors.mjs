// handle-colors, 2026.02.13.02.35
// API endpoint for saving and retrieving per-character handle colors.

import { authorize, findSisterSub } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { shell } from "../../backend/shell.mjs";
import * as logger from "../../backend/logger.mjs";
import { hasMonthlySupport } from "../../backend/monthly-support.mjs";

export function createHandleColorsHandler(deps = {}) {
  const authenticate = deps.authorize || authorize;
  const openDatabase = deps.connect || connect;
  const sister = deps.findSisterSub || findSisterSub;
  const support = deps.support || hasMonthlySupport;
  const broadcast = deps.broadcast || logger.broadcast;
  return async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, "");
  // GET: Retrieve handle colors for a user (no auth required for GET)
  if (event.httpMethod === "GET") {
    const handle = event.queryStringParameters?.handle;

    if (typeof handle !== "string" || !handle || handle.length > 32) {
      return respond(400, { message: "Missing handle parameter" });
    }

    try {
      const database = await openDatabase();
      const handles = database.db.collection("@handles");

      // Find by handle (case-insensitive)
      const user = await handles.findOne({
        handle: { $regex: new RegExp(`^${handle.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}$`, "i") },
      });

      await database.disconnect();

      if (!user || !user.colors) {
        // No custom colors set — return null so clients use their native theme color
        return respond(200, { colors: null });
      }

      return respond(200, { colors: user.colors });
    } catch (error) {
      shell.log("Failed to retrieve handle colors:", error);
      return respond(500, { message: "Failed to retrieve handle colors" });
    }
  }

  // POST: Save handle colors for a user
  if (event.httpMethod === "POST") {
    let body;
    try {
      body = JSON.parse(event.body);
    } catch (error) {
      return respond(400, { message: "Invalid JSON body" });
    }

    const { handle, colors, tenant = "aesthetic" } = body || {};

    if (typeof handle !== "string" || !handle || handle.length > 32 || !Array.isArray(colors) ||
        !["aesthetic", "sotce"].includes(tenant)) {
      return respond(400, {
        message: "Missing or invalid parameters: handle, colors required",
      });
    }

    // Validate colors array structure
    const handleWithAt = "@" + handle;
    if (colors.length !== handleWithAt.length) {
      return respond(400, {
        message: `Colors array length must match handle length (${handleWithAt.length})`,
      });
    }

    // Validate each color object
    for (const color of colors) {
      if (
        !color || !Number.isInteger(color.r) ||
        !Number.isInteger(color.g) ||
        !Number.isInteger(color.b) ||
        color.r < 0 ||
        color.r > 255 ||
        color.g < 0 ||
        color.g > 255 ||
        color.b < 0 ||
        color.b > 255
      ) {
        return respond(400, {
          message: "Invalid color format: each color must have r, g, b (0-255)",
        });
      }
    }

    // Authorize the user
    const user = await authenticate(event.headers, tenant);

    if (!user) {
      return respond(401, { message: "unauthorized" });
    }

    if (!user.email_verified) {
      return respond(401, { message: "unverified" });
    }

    let database;
    try {
      database = await openDatabase();
      const handles = database.db.collection("@handles");

      let sub = user.sub;
      if (tenant === "sotce") sub = "sotce-" + user.sub;

      // Find the user's handle document
      let primarySub = sub;
      let existingUser = await handles.findOne({ _id: sub });

      // If not found, check sister sub (e.g., user has multiple auth providers)
      if (!existingUser) {
        const sisterSub = await sister(sub, { prefixed: true });
        if (sisterSub) {
          existingUser = await handles.findOne({ _id: sisterSub });
          if (existingUser) primarySub = sisterSub;
        }
      }

      if (!existingUser) {
        return respond(404, { message: "Handle not found for this user" });
      }

      // Verify the handle matches
      if (existingUser.handle.toLowerCase() !== handle.toLowerCase()) {
        return respond(403, {
          message: "Cannot modify colors for another user's handle",
        });
      }

      // Public reads retain saved colors even after support ends. Only edits
      // require an active monthly AC gift; previews never write anything.
      let monthlySupporter;
      try { monthlySupporter = await support(user); }
      catch { return respond(503, { message: "Support check unavailable" }); }
      if (!monthlySupporter) return respond(403, { code: "monthly_support_required",
        message: "Monthly support unlocks handle colors across AC." });

      // Update the colors
      await handles.updateOne({ _id: primarySub }, { $set: { colors } });

      // Broadcast color change to all chat servers (no DB log entry).
      await broadcast(
        `@${handle} updated their handle colors`,
        { user: sub, action: "handle:colors", value: JSON.stringify({ handle, colors }) },
      );

      return respond(200, { message: "Colors saved successfully", colors });
    } catch (error) {
      console.error("Failed to save handle colors:", error);
      return respond(500, { message: "Failed to save handle colors" });
    } finally { await database?.disconnect(); }
  }

  return respond(405, { message: "Method Not Allowed" });
  };
}

export const handler = createHandleColorsHandler();
