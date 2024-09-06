// @handles, 24.05.27.07.50
// GET: Retrieve a flat list of all user handles.

/* #region 🏁 TODO 
#endregion */

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { userEmailFromID } from "../../backend/authorization.mjs";

const dev = process.env.CONTEXT === "dev";

export async function handler(event, context) {
  // A GET request to get a handle from a user `sub`.
  const database = await connect(); // 📕 Database
  const collection = database.db.collection("@handles");

  // A GET request to get a handle from a user `sub`.
  if (event.httpMethod === "GET") {
    try {
      const tenant = event.queryStringParameters.tenant;

      let handles;
      if (tenant === "aesthetic") {
        handles = await collection
          .find({ _id: { $not: { $regex: "^sotce-" } } })
          .toArray();
      } else if (tenant === "sotce") {
        handles = await collection
          .find({ _id: { $regex: "^sotce-" } })
          .toArray();
      } else {
        handles = await collection.find({}).toArray();
      }

      const withEmails = false; // Switch for debugging purposes. 24.09.04.21.39
      if (withEmails) {
        handles = await Promise.all(
          handles.map(async (h) => {
            const email = await userEmailFromID(h._id);
            console.log(email)
            return { ...h, ...email }; // Spread properties from emailData into h
          }),
        );
      }

      return respond(200, { handles });
    } catch (error) {
      return respond(500, { message: "Internal Server Error" });
    }
  } else {
    return respond(405, { message: "Method Not Allowed" });
  }
}
