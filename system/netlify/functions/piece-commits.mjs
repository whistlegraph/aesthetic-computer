// Piece Commits, 2026.02.12
// Returns last commit info for system pieces
// For now, returns empty - can be populated via a build-time script
// GET - Returns { commits: { pieceName: { message: string, date: string, author: string, hash: string } } }

import { respond } from "../../backend/http.mjs";
import fs from "fs";
import path from "path";

// Try to load pre-generated commit data from build time
function loadCommitData() {
  try {
    const dataPath = path.join(process.cwd(), "public", ".piece-commits.json");
    if (fs.existsSync(dataPath)) {
      const data = fs.readFileSync(dataPath, "utf-8");
      return JSON.parse(data);
    }
  } catch (err) {
    console.warn("Failed to load piece-commits.json:", err.message);
  }
  return {};
}

export async function handler(event) {
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Method not allowed" });
  }

  // Load commit data (could be generated at build time)
  // Build script would run:
  // for each .mjs in disks/, run: git log -1 --format="%H|%ai|%an|%s" -- path/to/piece.mjs
  const commits = loadCommitData();

  return respond(200, {
    commits,
    timestamp: new Date().toISOString(),
  });
}
