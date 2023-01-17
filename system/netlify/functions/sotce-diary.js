// Sotce Diary
// Allows sotce to give Patreon subscribers access to her diary (Tumblr).

/* #region todo 📓 
 - [] 
+ Done
#endregion */

const dev = process.env.NETLIFY_DEV;
// const patreonAccessToken = process.env.SOTCE_PATREON_ACCESS_TOKEN;
// const patreonRefreshToken = process.env.SOTCE_PATREON_REFRESH_TOKEN;

import crypto from "crypto"; // Import crypto library.
import { patreon as patreonAPI, oauth as patreonOAuth } from "patreon";

async function fun(event, context) {
  const client = process.env.SOTCE_PATREON_CLIENT_ID;
  const secret = process.env.SOTCE_PATREON_CLIENT_SECRET;

  const host = dev ? "localhost" : "sotce.aesthetic.computer";
  const origin = dev
    ? "https://localhost:8888/sotce-diary"
    : "https://sotce.aesthetic.computer";
  const redirect = "https://sotce.aesthetic.computer"; // This also needs to be set on the Patreon developer account.

  const code = event.queryStringParameters.code;

  // 1. Return an entry page if it's a GET.
  if (event.httpMethod === "GET") {
    // If the code parameter is present, exchange it for an access token
    if (code) {
      //const { got } = await import("got"); // Import the "got" http library.

      // const patreonAPI = patreon(client, secret);
      const patreonOAuthClient = patreonOAuth(client, secret);

      let pledgeTier;

      try {
        const tokensResponse = await patreonOAuthClient.getTokens(code, redirect);


        // Get the subscriber's pledge tier, if any.
        const userResponse = await patreonOAuthClient.getUser(
          tokensResponse.access_token
        );

        const user = userResponse.data;
        pledgeTier = user.attributes.pledge_relationship.pledge_tier_id;

        console.log(pledgeTier);

        /*
        if (pledgeTier === "access-tier-id") {
          // redirect to access page
        } else {
          // redirect to denied page
        }
        */

      } catch (err) {
        // TODO: Show an access denied page?
        //       Is this what happens if the user doesn't allow?
        console.error("Error!");
        return {
          statusCode: 500,
          body: "Error!",
        };
      }

      // Redirect the user to a new URL with the access token
      return {
        statusCode: 307,
        headers: {
          Location: `https://sotce.aesthetic.computer/?pledge_tier=${pledgeTier}`,
        },
      };
    }

    // If the code parameter is not present, return the HTML template with the authorization link
    const state = crypto.randomBytes(32).toString("hex");
    const loginUrl = `https://www.patreon.com/oauth2/authorize?response_type=code&client_id=${client}&redirect_uri=${redirect}&state=${state}`;

    return {
      statusCode: 200,
      headers: {
        "Content-Type": "text/html",
      },
      body: `
      <h1>Sotce's Diary</h1>
      <a href="${loginUrl}">Enter</a>
    `,
    };
  } else {
    return {
      statusCode: 405,
      body: "Unauthorized HTTP Method",
    };
  }
}

export const handler = fun;