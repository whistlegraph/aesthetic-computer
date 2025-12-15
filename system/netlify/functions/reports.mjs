// Reports API for false.work builds
// GET: Fetch all reports
// POST: Save a new report
// Requires password authentication via Authorization header

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

const BUILDS_PASSWORD = process.env.BUILDS_PASSWORD;

export async function handler(event, context) {
  if (event.httpMethod === "OPTIONS") {
    return respond(204, "");
  }

  // Verify password
  const authHeader = event.headers.authorization || event.headers.Authorization;
  const password = authHeader?.replace("Bearer ", "");

  if (!password || password !== BUILDS_PASSWORD) {
    return respond(401, { error: "Unauthorized" });
  }

  try {
    const database = await connect();
    const reports = database.db.collection("false.work-reports");

    // GET - Fetch all reports
    if (event.httpMethod === "GET") {
      const allReports = await reports
        .find({})
        .sort({ createdAt: -1 })
        .toArray();

      await database.disconnect();
      return respond(200, { reports: allReports });
    }

    // POST - Save a new report
    if (event.httpMethod === "POST") {
      const body = JSON.parse(event.body);
      const { 
        buildPlatform, 
        buildVersion, 
        tester, 
        summary, 
        lowHangingFruit, 
        onTheSide, 
        biggerThoughts 
      } = body;

      if (!buildPlatform || !buildVersion) {
        await database.disconnect();
        return respond(400, { error: "Missing buildPlatform or buildVersion" });
      }

      const report = {
        buildPlatform,
        buildVersion,
        tester: tester || "Anonymous",
        summary: summary || "",
        lowHangingFruit: lowHangingFruit || [],
        onTheSide: onTheSide || [],
        biggerThoughts: biggerThoughts || [],
        createdAt: new Date(),
        updatedAt: new Date(),
        deleted: false
      };

      const result = await reports.insertOne(report);
      await database.disconnect();

      return respond(201, { 
        success: true,
        reportId: result.insertedId,
        report
      });
    }

    await database.disconnect();
    return respond(405, { error: "Method not allowed" });

  } catch (err) {
    console.error("Reports API error:", err);
    return respond(500, { error: "Failed to process request" });
  }
}
