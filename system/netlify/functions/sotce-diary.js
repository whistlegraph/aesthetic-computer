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
      // console.log(patreonAPI)

      const patreonOAuthClient = patreonOAuth(client, secret);

      let tokensResponse;

      try {
        tokensResponse = await patreonOAuthClient.getTokens(
          code,
          redirect
        );
        console.log("Tokens...", tokensResponse);
      } catch (err) {
        // TODO: Show an access denied page?
        //       Is this what happens if the user doesn't allow?
        console.error("Error!");
        return {
          statusCode: 500,
          body: "Error!"
        }
      }

      const accessToken = tokensResponse?.access_token;

      // const tokens = await patreonAPI.exchangeCodeForToken(code, redirect);
      // const accessToken = tokens.access_token;

      // TODO: How to I make a post request using the patreon library with the code, to
      // see what tier this user is in in order to redirect them below according to
      // whether they have access or not.

      // Redirect the user to a new URL with the access token
      return {
        statusCode: 307,
        headers: {
          Location: `https://sotce.aesthetic.computer/?access_token=${accessToken}`,
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
// Authorization URL

// TODO: How to change loginURL to the

/*
    let loginUrl = `https://www.patreon.com/oauth2/authorize?response_type=code&client_id=${client}&redirect_uri=${redirect}&state=${crypto
      .randomBytes(32)
      .toString("hex")}`;

    return {
      statusCode: 200,
      headers: {
        "Content-Type": "text/html",
      },
      body: `
        <h1>Sotce's Diary</h1>
        <a href="${loginUrl}">Enter</button>
      `,
    };
  } else if (event.httpMethod !== "POST") {
    // 2. Or deny access if it's not POST.
    return {
      statusCode: 405,
      body: "Unauthorized HTTP Method",
    };
  }
  */

// TODO: How can I change the returned html based on the presence of the code attribute to trigger a post request
//       or redirect the user for authorization?

//const isSubscribed = true;

// Return the result
//return {
//  statusCode: 200,
//  body: JSON.stringify({ isSubscribed }),
//};

// 3. Otherwise identify the user.
// const { got } = await import("got"); // Import the "got" http library.

// Send the request to the Patreon OAuth2 authorization endpoint

// const url = new URL(body);
// const code = url.searchParams.get("code");

// console.log(url, code);

// Get the access token
/*
  const { body } = await got.post("https://www.patreon.com/api/oauth2/token", {
    json: true,
    form: {
      grant_type: "authorization_code",
      client_id: client,
      client_secret: secret,
      redirect_uri: redirect,
      code,
    },
  });

  console.log(body);
  */

/*
  // Extract the access token from the response
  const access_token = body.access_token;
  // Extract the patron_id from the response
  const patron_id = body.refresh_token;
  // Extract the tier from the event
  const { tier } = JSON.parse(event.body);

  // Make a GET request to the Patreon API to retrieve the patron's membership
  const { body: membershipData } = await got(
    `https://www.patreon.com/api/oauth2/v2/members/${patron_id}`,
    {
      headers: {
        Authorization: `Bearer ${access_token}`,
        "Content-Type": "application/json",
      },
      json: true,
    }
  );

  // Check if the user is subscribed to the specified tier
  const isSubscribed = membershipData.included.some(
    (membership) => membership.attributes.tier_name === tier
  );
  */
