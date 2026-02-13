// handle-colors, 2026.02.13.02.35
// API endpoint for saving and retrieving per-character handle colors.

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import * as logger from "../../backend/logger.mjs";
import { shell } from "../../backend/shell.mjs";

export async function handler(event, context) {
  // GET: Retrieve handle colors for a user (no auth required for GET)
  if (event.httpMethod === "GET") {
    const handle = event.queryStringParameters?.handle;

    if (!handle) {
      return respond(400, { message: "Missing handle parameter" });
    }

    try {
      const database = await connect();
      const handles = database.db.collection("@handles");

      // Find by handle (case-insensitive)
      const user = await handles.findOne({
        handle: { $regex: new RegExp(`^${handle}$`, "i") },
      });

      await database.disconnect();

      if (!user || !user.colors) {
        // Return default colors (white for all characters)
        const defaultColors = ("@" + handle).split("").map(() => ({
          r: 255,
          g: 255,
          b: 255,
        }));
        return respond(200, { colors: defaultColors });
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

    const { handle, colors, tenant = "aesthetic" } = body;

    if (!handle || !colors || !Array.isArray(colors)) {
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
        typeof color.r !== "number" ||
        typeof color.g !== "number" ||
        typeof color.b !== "number" ||
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
    shell.log(`🎨 Attempting to authorize user for tenant: ${tenant}`);
    const user = await authorize(event.headers, tenant);

    shell.log(`🎨 Authorization result:`, user ? `User ID: ${user.sub}, Email verified: ${user.email_verified}` : "No user");

    if (!user) {
      shell.log(`🎨 Authorization failed: no user`);
      return respond(401, { message: "unauthorized" });
    }

    if (!user.email_verified) {
      shell.log(`🎨 Authorization failed: email not verified`);
      return respond(401, { message: "unverified" });
    }

    try {
      shell.log(`🎨 Saving handle colors for ${tenant}:`, handle);

      const database = await connect();
      const handles = database.db.collection("@handles");

      let sub = user.sub;
      if (tenant === "sotce") sub = "sotce-" + user.sub;

      // Find the user's handle document
      const existingUser = await handles.findOne({ _id: sub });

      if (!existingUser) {
        await database.disconnect();
        return respond(404, { message: "Handle not found for this user" });
      }

      // Verify the handle matches
      if (existingUser.handle.toLowerCase() !== handle.toLowerCase()) {
        await database.disconnect();
        return respond(403, {
          message: "Cannot modify colors for another user's handle",
        });
      }

      // Update the colors
      const updateResult = await handles.updateOne({ _id: sub }, { $set: { colors } });

      shell.log(`🎨 MongoDB update result:`, updateResult);
      shell.log(`🎨 Saved ${colors.length} colors for @${handle}`);

      // Verify the save by reading back
      const verifyUser = await handles.findOne({ _id: sub });
      shell.log(`🎨 Verification - colors in DB:`, verifyUser?.colors ? `${verifyUser.colors.length} colors` : "no colors");

      // Log the color change
      if (!sub.startsWith("sotce-")) {
        await logger.link(database);
        await logger.log(`@${handle} updated their handle colors`, {
          user: sub,
          action: "handle:colors:update",
          value: JSON.stringify(colors),
        });
      }

      await database.disconnect();

      return respond(200, { message: "Colors saved successfully", colors });
    } catch (error) {
      console.error("Failed to save handle colors:", error);
      return respond(500, { message: "Failed to save handle colors" });
    }
  }

  return respond(405, { message: "Method Not Allowed" });
}
