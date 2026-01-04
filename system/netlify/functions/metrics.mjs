// Metrics, 24.10.20.00.06
// Reports platform activity and statistics for Aesthetic Computer and Sotce Net.
// Now with Redis caching (30 min TTL) to reduce MongoDB load.

// 1. GET `api/metrics`
// Return JSON data.

/* #region 🏁 TODO 
#endregion */

import { activeUsers } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { getOrCompute, CACHE_KEYS, CACHE_TTLS } from "../../backend/cache.mjs";
const dev = process.env.CONTEXT === "dev";

import { shell } from "../../backend/shell.mjs";

export async function handler(event, context) {
  if (event.httpMethod === "GET") {
    try {
      // Check if client wants fresh data (bypass cache)
      const bypassCache = event.queryStringParameters?.fresh === 'true';
      
      if (bypassCache) {
        console.log("📊 Metrics: bypassing cache (fresh=true)");
        return respond(200, await computeMetrics());
      }
      
      // Use cached metrics (30 min TTL)
      const metrics = await getOrCompute(
        CACHE_KEYS.METRICS,
        computeMetrics,
        CACHE_TTLS.METRICS
      );
      
      return respond(200, metrics);
    } catch (error) {
      shell.log(error);
      return respond(500, { message: error });
    }
  } else {
    return respond(405, { message: "Method Not Allowed" });
  }
}

// Extracted metrics computation for caching
async function computeMetrics() {
  const { got } = await import("got");

  // Get active users from `auth0`.
  const active = await activeUsers("aesthetic");
  const sotceActive = await activeUsers("sotce");

  // Pull some stats from the Mongo database.
  const database = await connect();

  const handlesCollection = database.db.collection("@handles");

  const handles = await handlesCollection.countDocuments();

  // sotce-net
  const sotceHandles = await handlesCollection.countDocuments({
    _id: { $regex: "^sotce-" },
  });

  const pieces = await database.db.collection("pieces").countDocuments();
  const paintings = await database.db
    .collection("paintings")
    .countDocuments();
  const moods = await database.db.collection("moods").countDocuments();
  const logs = await database.db.collection("logs").countDocuments();
  const chatMessages = await database.db
    .collection("chat-system")
    .countDocuments();
  const tickets = await database.db.collection("tickets").countDocuments();
  const kidlisp = await database.db.collection("kidlisp").countDocuments();
  const pages = database.db.collection("sotce-pages");
  const pagesCrumpled = await pages.countDocuments({ state: "crumpled" });
  const pagesPublished = await pages.countDocuments({ state: "published" });
  const touches = await database.db
    .collection("sotce-touches")
    .countDocuments();
  await database.disconnect();

  // Get sotce subscriptions.
  let sotceSubscribers = null;
  try {
    sotceSubscribers = (
      await (await fetch("https://sotce.net/sotce-net/subscribers")).json()
    ).subscribers;
  } catch (err) {
    shell.error("🔴 Could not retrieve `sotce-net` subscriber count:", err);
  }

  const printsOrdered = await getTotalPrintfulPrintQuantity();

  const time = new Date().toLocaleString("en-US", {
    weekday: "long",
    year: "numeric",
    month: "short",
    day: "numeric",
    hour: "numeric",
    minute: "numeric",
    second: "numeric",
    timeZoneName: "short",
  });

  return {
    handles,
    active,
    pieces,
    paintings,
    moods,
    kidlisp,
    chatMessages,
    logs,
    printsOrdered,
    tickets,
    sotceActive,
    sotceHandles,
    sotceSubscribers,
    pagesPublished,
    pagesCrumpled,
    touches,
    time,
    cached: true, // Indicate this may be cached
  };
}

const printfulKey = process.env.PRINTFUL_API_TOKEN;

async function getTotalPrintfulPrintQuantity() {
  const { got } = await import("got");
  const API = "https://api.printful.com";
  const headers = {
    Authorization: "Bearer " + printfulKey,
    "Content-Type": "application/json",
  };

  try {
    let page = 1;
    let totalQuantity = 0;

    while (true) {
      const response = await got(`${API}/orders`, {
        headers,
        searchParams: {
          status: "fulfilled",
          offset: (page - 1) * 100,
          limit: 100,
        },
        responseType: "json",
      });

      const orders = response.body.result;

      // Sum the quantity of each item in the order.
      for (const order of orders) {
        for (const item of order.items) {
          totalQuantity += item.quantity;
        }
      }

      if (orders.length < 100) break; // No more orders to fetch.
      page++;
    }

    // shell.log(`Total prints ordered via Printful: ${totalQuantity}`);
    return totalQuantity;
  } catch (error) {
    shell.error("Error querying Printful orders:", error.message);
    return 0;
  }
}
