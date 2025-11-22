// chat-messages, 25.11.21.18.30
// GET: Returns recent chat messages from a specific chat instance.
//      Examples: "clock" for Laer-Klokken, "system" for main chat

/* #region 🏁 TODO 
  - [] Add pagination support
  - [] Add date range filtering
#endregion */

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { shell } from "../../backend/shell.mjs";

export async function handler(event, context) {
  if (event.httpMethod === "OPTIONS") {
    return respond(204, null);
  }

  if (event.httpMethod !== "GET") {
    return respond(405, { message: "Method Not Allowed" });
  }

  try {
    const params = event.queryStringParameters || {};
    const instance = params.instance || "system";
    const limit = parseInt(params.limit || "50", 10);
    
    if (limit > 100) {
      return respond(400, { message: "Limit cannot exceed 100" });
    }

    shell.log(`📨 Fetching ${limit} messages for chat instance: ${instance}`);

    const database = await connect();
    
    // Use different collections based on instance
    const collectionName = instance === "clock" ? "chat-clock" : "chat-system";
    const collection = database.db.collection(collectionName);
    
    shell.log(`📂 Using collection: ${collectionName} for instance: ${instance}`);

    // Query for messages, sorted by timestamp descending
    const messages = await collection
      .find({})
      .sort({ when: -1 })
      .limit(limit)
      .toArray();

    // Reverse to get chronological order (oldest to newest)
    messages.reverse();

    // Resolve handles from user subs using the handle API
    const messagesWithHandles = await Promise.all(
      messages.map(async (msg) => {
        let handle = "anon";
        
        if (msg.user) {
          try {
            // Determine prefix based on instance
            let prefix = "";
            if (instance === "sotce") prefix = "sotce-";
            
            // Build handle lookup URL - use local endpoint in dev
            const baseUrl = process.env.NETLIFY_DEV === "true" 
              ? "https://localhost:8888"
              : "https://aesthetic.computer";
            const handleUrl = `${baseUrl}/handle?for=${prefix}${msg.user}`;
            
            // Disable TLS verification for local dev (self-signed cert)
            const fetchOptions = process.env.NETLIFY_DEV === "true"
              ? { 
                  agent: new (await import('https')).Agent({ rejectUnauthorized: false })
                }
              : {};
            
            const handleResponse = await fetch(handleUrl, fetchOptions);
            
            if (handleResponse.status === 200) {
              const handleData = await handleResponse.json();
              if (handleData.handle) {
                handle = "@" + handleData.handle;
              }
            } else {
              shell.log(`⚠️ Handle lookup failed for user ${msg.user}: ${handleResponse.status}`);
            }
          } catch (error) {
            shell.error(`❌ Handle lookup error for user ${msg.user}:`, error.message);
          }
        }
        
        return {
          from: handle,
          text: msg.text,
          when: msg.when,
        };
      })
    );

    await database.disconnect();

    shell.log(`✅ Found ${messagesWithHandles.length} messages for instance: ${instance}`);

    return respond(200, {
      instance,
      count: messagesWithHandles.length,
      messages: messagesWithHandles,
    });
  } catch (error) {
    shell.error("Error fetching chat messages:", error);
    return respond(500, { message: error.message });
  }
}
