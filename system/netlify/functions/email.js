// email, 23.09.12.20.59 📧
// POST: Set or replace a user's email via auth0's api and resend verification.

/* #region 🏁 TODO 
  - [] Satisfy the conditions in the comment above. 
#endregion */

import {
  authorize,
  setEmailAndReverify,
} from "../../backend/authorization.mjs";
import { respond } from "../../backend/http.mjs";

const dev = process.env.CONTEXT === "dev";

export async function handler(event, context) {
  if (event.httpMethod !== "POST") {
    return respond(405, { message: "Method Not Allowed" });
  }

  // Parse the body of the HTTP request
  let body;
  try {
    body = JSON.parse(event.body);
    const email = body.email;
    const user = await authorize(event.headers);
    if (user) {
      // 🔑 We are logged in!
      const emailChanged = await setEmailAndReverify(user.sub, email);
      if (emailChanged.success) {
        return respond(200, { email });
      } else {
        return respond(500, { message: emailChanged.message });
      }
    } else {
      return respond(401, { message: "unauthorized" });
    }
  } catch (error) {
    return respond(400, { message: "Cannot parse input body." });
  }
}
