// @handles, 24.05.27.07.50
// GET: Retrieve a flat list of all user handles.

/* #region 🏁 TODO 
#endregion */

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

const dev = process.env.CONTEXT === "dev";

export async function handler(event, context) {
  // A GET request to get a handle from a user `sub`.
  const database = await connect(); // 📕 Database
  const collection = database.db.collection("@handles");

  // A GET request to get a handle from a user `sub`.
  if (event.httpMethod === "GET") {
    try {
      const handles = await collection.find({}).toArray();
      return respond(200, { handles });
    } catch (error) {
      return respond(500, { message: "Internal Server Error" });
    }
  } else {
    return respond(405, { message: "Method Not Allowed" });
  }
}
