// email, 23.09.12.20.59 📧
// POST: Set or replace a user's email via auth0's api and resend verification.
// Works across both the `aesthetic` and `sotce-net` tenant simultaneously
// to keep accounts connected.

// TODO: Users still have to verify both tenants separately after changing
//       their email (clicking two links). 24.08.31.22.57

/* #region 🏁 TODO 
  - [] Satisfy the conditions in the comment above. 
#endregion */

import {
  authorize,
  setEmailAndReverify,
  userIDFromEmail,
} from "../../backend/authorization.mjs";
import { respond } from "../../backend/http.mjs";
import { shell } from "../../backend/shell.mjs";
import Stripe from "stripe";

// 💳 Payment
import {
  SOTCE_STRIPE_API_PRIV_KEY,
  SOTCE_STRIPE_API_PUB_KEY,
  SOTCE_STRIPE_API_TEST_PRIV_KEY,
  SOTCE_STRIPE_API_TEST_PUB_KEY,
  SOTCE_STRIPE_ENDPOINT_DEV_SECRET,
  SOTCE_STRIPE_ENDPOINT_SECRET,
  priceId,
  productId,
} from "../../backend/sotce-net-constants.mjs";

const dev = process.env.NETLIFY_DEV;

export async function handler(event, context) {
  if (event.httpMethod !== "POST") {
    return respond(405, { message: "Method Not Allowed" });
  }

  // Parse the body of the HTTP request
  let body;
  try {
    body = JSON.parse(event.body);
    const email = body.email;
    const name = body.name;
    const tenant = body.tenant;
    const user = await authorize(event.headers, tenant);

    shell.log("Updating email.")
    shell.log("User after authorization:", user);

    if (user) {
      // 🔑 We are logged in!

      // See if there is a matching user on the sister tenant, and
      // if so, then also reset their email.
      // (This keeps handles tied together)

      const sisterTenant = tenant === "sotce" ? "aesthetic" : "sotce";

      const sister = await userIDFromEmail(user.email, sisterTenant);

      const emailChanged = await setEmailAndReverify(
        user.sub,
        email,
        name,
        tenant,
      );

      let sisterEmailChanged;
      if (sister?.userID) {
        sisterEmailChanged = await setEmailAndReverify(
          sister.userID,
          email,
          name,
          sisterTenant,
        );
      }

      // Update the Stripe customer's email address for `sotce-net` if needed.
      try {
        if (tenant === "sotce" || (sisterTenant === "sotce" && sister?.userID)) {
          const sub = tenant === "sotce" ? user.sub : sister.userID;

          const key = dev
            ? SOTCE_STRIPE_API_TEST_PRIV_KEY
            : SOTCE_STRIPE_API_PRIV_KEY;

          const stripe = Stripe(key);

          // Fetch customer by user ID (sub) from metadata
          const customers = await stripe.customers.search({
            query: "metadata['sub']:'" + sub + "'",
          });

          // Update the customer's email address if they exist.
          const customer = customers.data[0];
          if (customer) await stripe.customers.update(customer.id, { email });
        }
      } catch (err) {
        shell.error("Could not update Stripe customer email for:", email);
      }

      if (emailChanged.success && (!sister?.userID || sisterEmailChanged?.success)) {
        return respond(200, { email });
      } else {
        return respond(500, { message: "Could not change emails." });
      }
    } else {
      return respond(401, { message: "unauthorized" });
    }
  } catch (error) {
    shell.error(error);
    return respond(400, {
      message: "Cannot parse input body.",
      error: error.message,
    });
  }
}
