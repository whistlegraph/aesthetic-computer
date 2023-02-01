// Auth
// Logs a user into a session via passwordless email confirmation.

/* #region todo 📓 
  - [😫] Integrate the GoTrue API instead: https://github.com/netlify/gotrue-js
    - [] Can the GoTrue API do passwordless sign-up?
  - [] Also read this: https://auth0.com/docs/authenticate/passwordless/implement-login/embedded-login/relevant-api-endpoints
  - [] Read this: https://answers.netlify.com/t/possible-to-login-with-gotrue-identity-via-serverless-function/36028/3
  - [] I probably want to use something different from this...
#endregion */

import GoTrue from "gotrue-js";

async function fun(event) {
  const api = GoTrue({
    APIUrl: "https://aesthetic.computer/.netlify/identity",
    audience: "",
    setCookie: false,
  });

  // *** Passwordless Authentication ***
  // Send an email to the user.
  if (event.httpMethod === "POST" && event.path === "/auth") {
    try {
      const body = JSON.parse(event.body);
      const email = body.email;

      await api.requestPasswordlessLogin({ email });
      return {
        statusCode: 200,
        body: "Passwordless login link sent!",
      };
    } catch (err) {
      return {
        statusCode: 500,
        body: "Error sending passwordless login link: " + err.message,
      };
    }
  }

  // *** Logging In ***
  // Via query string parameter from emailed link above.
  if (event.httpMethod === "GET" && event.path === "/auth") {
    try {
      const token = event.queryStringParameters.token;
      await api.acceptPasswordlessToken({ token });
      return {
        statusCode: 200,
        body: "Logged in!",
      };
    } catch (err) {
      return {
        statusCode: 500,
        body: "Error logging in: " + err.message,
      };
    }
  }
}

export const handler = fun;

/*
import {
  requestPasswordRecovery,
  confirmPasswordRecovery,
} from "@netlify/identity-client";

async function fun(event) {
  // *** Passwordless Authentication ***
  // Send an email to the user.
  if (event.httpMethod === "POST" && event.path === "/auth") {
    try {
      const body = JSON.parse(event.body);
      const email = body.email;

      await requestPasswordRecovery({ email });
      return {
        statusCode: 200,
        body: "Passwordless login link sent!",
      };
    } catch (err) {
      return {
        statusCode: 500,
        body: "Error sending passwordless login link: " + err.message,
      };
    }
  }

  // *** Logging In ***
  // Via query string parameter from emailed link above.
  if (event.httpMethod === "GET" && event.path === "/auth") {
    try {
      const recoveryToken = event.queryStringParameters.recovery_token;
      await confirmPasswordRecovery({ recoveryToken });
      return {
        statusCode: 200,
        body: "Logged in!",
      };
    } catch (err) {
      return {
        statusCode: 500,
        body: "Error logging in: " + err.message,
      };
    }
  }
}

export const handler = fun;
*/
