// Piece Metadata, 2026.02.12
// Returns metadata about a piece (trust level, author, etc.) for security checks

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

export async function handler(event, context) {
  if (event.httpMethod === 'OPTIONS') {
    return respond(204, '');
  }

  if (event.httpMethod !== 'GET') {
    return respond(405, { error: 'Method not allowed' });
  }

  const code = event.queryStringParameters?.code;

  if (!code) {
    return respond(400, { error: 'Missing code parameter' });
  }

  let database;
  try {
    database = await connect();
  } catch (connectError) {
    console.error('❌ MongoDB connection failed:', connectError.message);
    return respond(503, { error: 'Database temporarily unavailable' });
  }

  try {
    const collection = database.db.collection('pieces');

    // Find piece by code
    const piece = await collection.findOne({ code });

    if (!piece) {
      // Piece not found - return default untrusted metadata
      await database.disconnect();
      return respond(200, {
        code,
        trustLevel: "untrusted",
        anonymous: true
      });
    }

    // Return metadata
    const metadata = {
      code: piece.code,
      trustLevel: piece.trustLevel || "untrusted",
      anonymous: piece.anonymous !== false, // Default to anonymous if not specified
      authorSub: piece.authorSub || null,
      when: piece.when,
    };

    await database.disconnect();
    return respond(200, metadata);

  } catch (error) {
    console.error('❌ Piece metadata error:', error);
    await database.disconnect();
    return respond(500, { error: 'Internal server error' });
  }
}
