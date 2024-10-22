// Metrics, 24.10.20.00.06
// Reports platform activity and statistics for Aesthetic Computer and Sotce Net.

// 1. GET `api/metrics`
// Return JSON data.

/* #region 🏁 TODO 
#endregion */

import { activeUsers } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
const dev = process.env.CONTEXT === "dev";

import { shell } from "../../backend/shell.mjs";

export async function handler(event, context) {
  if (event.httpMethod === "GET") {
    try {
      const { got } = await import("got");

      // Get active users from `auth0`.
      const active = await activeUsers("aesthetic");
      const sotceActive = await activeUsers("sotce");

      // Pull some stats from the Mongo database.
      const database = await connect();

      const handlesCollection = database.db.collection("@handles");
      //const allHandles = await handlesCollection.countDocuments();

      // aesthetic-computer
      const handles = await handlesCollection.countDocuments({
        _id: { $not: { $regex: "^sotce-" } },
      });

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

      // TODO: Add data from either Cloudflare or Google Analytics. 24.10.21.00.30
      //       Cloudflare URL: https://dash.cloudflare.com/a23b54e8877a833a1cf8db7765bce3ca/aesthetic.computer/analytics/traffic

      const time = new Date().toLocaleString("en-US", {
        weekday: "long", // Day of the week
        year: "numeric",
        month: "short", // Short month name
        day: "numeric",
        hour: "numeric",
        minute: "numeric",
        second: "numeric",
        timeZoneName: "short", // Includes time zone
      });

      return respond(200, {
        handles, // shared
        // `aesthetic-computer`
        active,
        pieces,
        paintings,
        moods,
        chatMessages,
        logs,
        printsOrdered,
        tickets,
        // `sotce-net`
        sotceActive,
        sotceHandles,
        sotceSubscribers,
        pagesPublished,
        pagesCrumpled,
        touches,
        time,
      });
    } catch (error) {
      shell.log(error);
      return respond(500, { message: error });
    }
  } else {
    return respond(405, { message: "Method Not Allowed" });
  }
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
