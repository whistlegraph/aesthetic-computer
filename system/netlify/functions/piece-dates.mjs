// Piece Dates, 2025.01.16
// Returns creation dates for system pieces
// For now returns empty - can be populated via a build step that runs git blame
// GET - Returns { dates: { pieceName: { created: timestamp, author: string } } }

import { respond } from "../../backend/http.mjs";

// This would ideally be populated at build time by running:
// git log --format="%ai" --follow --diff-filter=A -- system/public/aesthetic.computer/disks/PIECE.mjs
// For each piece in the disks folder

// For now, return empty - dates can be fetched from piece-hits.firstHit as a fallback
export async function handler(event) {
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Method not allowed" });
  }

  // Placeholder - could be generated at build time
  // Format: { dates: { "box": { created: "2024-01-15T00:00:00Z", author: "jeffrey" }, ... } }
  return respond(200, { dates: {} });
}
