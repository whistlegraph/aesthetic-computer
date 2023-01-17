// Sotce Blog
// Allows sotce to give Patreon subscribers access to her blog (Tumblr).
// Patreon dev docs: https://www.patreon.com/portal

/* #region todo 📓 
 - [-] Create a session and hook it up to the Tumblr somehow?
+ Done
 - [x] Hook up to blog.sotce.com.
 - [x] Write the logic that detects whether a user is sotce or a subscriber. 
#endregion */

const dev = process.env.NETLIFY_DEV;
// const patreonAccessToken = process.env.SOTCE_PATREON_ACCESS_TOKEN;
// const patreonRefreshToken = process.env.SOTCE_PATREON_REFRESH_TOKEN;

import crypto from "crypto"; // Import crypto library.

async function fun(event, context) {
  const client = process.env.SOTCE_PATREON_CLIENT_ID;
  const secret = process.env.SOTCE_PATREON_CLIENT_SECRET;
  const vanity = "blog.sotce.com"; // "sotce.aesthetic.computer"
  const redirect = dev
    ? "https://localhost:8888/sotce-blog"
    : `https://${vanity}`;
  // Both values above also must be set (space separated) in the
  // Patreon developer account's client redirect URI field.

  // 1. Return an entry page if it's a GET.
  if (event.httpMethod === "GET") {
    // If the code parameter is not present, return the HTML template with the authorization link
    const scope = encodeURIComponent("identity identity.memberships"); // Space separated... identity[email]
    //                                                           See also https://docs.patreon.com/#scopes
    const state = crypto.randomBytes(32).toString("hex"); // For securing the request.
    const loginUrl = `https://www.patreon.com/oauth2/authorize?response_type=code&client_id=${client}&redirect_uri=${redirect}&state=${state}&scope=${scope}`;

    const gateBody = `
      <html>
        <head>
          <style>
            body {
              margin: 0;
              padding: 1em;
              background: gray;
              display: flex;
            }
            h1 {
              font-family: helvetica, arial, sans-serif;
              font-weight: normal;
            }
            #wrapper {
              margin: auto;
            }
          </style>
        </head>
        <body>
          <div id="wrapper">
            <h1><a href="${loginUrl}">enter</a></h1>
          </div>
        </body>
      </html>
    `;

    // If the code parameter is present, exchange it for an access token
    const code = event.queryStringParameters.code;
    if (code) {
      const { got } = await import("got"); // Import the "got" http library.

      let hasBlogAccess = false;

      try {
        // 1. Get the user's Patreon access token.
        const tokenResponse = await got.post(
          "https://www.patreon.com/api/oauth2/token",
          {
            form: {
              code,
              client_id: client,
              client_secret: secret,
              redirect_uri: redirect,
              grant_type: "authorization_code",
            },
          }
        );

        const accessToken = JSON.parse(tokenResponse.body).access_token;

        // 2. Check patronage.
        // Read the docs here to build a query: https://docs.patreon.com/#get-api-oauth2-v2-identity
        // Note: Encode square [] brackets as %5B and %5D
        const identityResponse = await got(
          `https://www.patreon.com/api/oauth2/v2/identity?include=memberships.currently_entitled_tiers&fields%5Buser%5D=email,is_email_verified&fields%5Bmember%5D=currently_entitled_amount_cents,patron_status`,
          {
            headers: {
              Authorization: `Bearer ${accessToken}`,
              Accept: "application/json",
            },
          }
        );

        const id = JSON.parse(identityResponse.body);

        // Check to see if the user is sotce.
        const isSotce =
          id.data.attributes.email === "hellosotce@gmail.com" &&
          id.data.attributes.is_email_verified;

        // Get membership info for this user associated with sotce's campaign.
        const sotceCampaignID = "4483adce-b1bb-41c7-aea4-d0bacd02b0ed";
        const membership = id.included?.filter(
          (v) => v.id === sotceCampaignID
        )[0]?.attributes;

        // Give only sotce's account & $3 > tier accounts access.
        hasBlogAccess =
          isSotce ||
          (membership?.currently_entitled_amount_cents >= 300 &&
            membership?.patron_status === "active_patron");
      } catch (err) {
        // TODO: - [] Show an access denied page?
        //            Is this what happens if the user doesn't allow?
        // TODO: - [] What if they are not logged into patreon?

        return {
          headers: {
            "Content-Type": "application/json",
          },
          statusCode: 500,
          body: JSON.stringify({ hasBlogAccess, error: err }),
        };
      }

      // Return a json response that determines the user's access level.

      let blogBody;

      if (hasBlogAccess) {
        blogBody = ``; // Just show a totally empty body.
      } else {
        // Rejected case... maybe do a redirect here or what...
        blogBody = gateBody;
      }

      return {
        statusCode: 200,
        headers: {
          "Content-Type": "text/html",
        },
        body: blogBody,
      };
    }

    return {
      statusCode: 200,
      headers: {
        "Content-Type": "text/html",
      },
      body: gateBody,
    };
  } else {
    return {
      statusCode: 405,
      body: "Unauthorized HTTP Method",
    };
  }
}

export const handler = fun;
