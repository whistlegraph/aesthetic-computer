// Authorization, 23.04.30.17.47

// Authenticates a user to make sure they are logged in
// and their local keys match the user database.
// 🧠 (And so they can run authorized server functions.)

import { connect } from "./database.mjs";
import * as KeyValue from "./kv.mjs";
import { shell } from "./shell.mjs";
const dev = process.env.CONTEXT === "dev";

const aestheticBaseURI = "https://aesthetic.us.auth0.com";
const sotceBaseURI = "https://sotce.us.auth0.com";

export async function authorize({ authorization }, tenant = "aesthetic") {
  try {
    const { got } = await import("got");
    const baseURI = tenant === "aesthetic" ? aestheticBaseURI : sotceBaseURI;
    shell.log(`🔐 Authorizing \`${tenant}\` user.`);
    return (
      await got(`${baseURI}/userinfo`, {
        headers: { Authorization: authorization },
        responseType: "json",
      })
    ).body;
  } catch (err) {
    shell.error("Authorization error:", err, err?.code);
    return undefined;
  }
}

export async function hasAdmin(user, tenant = "aesthetic") {
  if (tenant === "aesthetic") {
    const handle = await handleFor(user.sub);
    return (
      user &&
      user.email_verified &&
      handle === "jeffrey" &&
      user.sub === process.env.ADMIN_SUB
    );
  } else if (tenant === "sotce") {
    const subs = process.env.SOTCE_ADMIN_SUBS?.split(",");
    const handle = await handleFor(user.sub, "sotce");
    return (
      user &&
      user.email_verified &&
      ((user.sub === subs[0] && handle === "jeffrey") ||
        (user.sub === subs[1] && (handle === "amelia" || handle === "sotce")))
    );
  }
}

// Get the user ID via their email.
export async function userIDFromEmail(email, tenant = "aesthetic", got, token) {
  try {
    if (!got) got = (await import("got")).got;
    if (!token) token = await getAccessToken(got, tenant);
    const baseURI = tenant === "aesthetic" ? aestheticBaseURI : sotceBaseURI;

    const userResponse = await got(`${baseURI}/api/v2/users-by-email`, {
      searchParams: { email },
      headers: { Authorization: `Bearer ${token}` },
      responseType: "json",
    });

    const user = userResponse.body[0];
    const userID = user?.user_id;
    return { userID, email_verified: user?.email_verified, tenant };
  } catch (error) {
    shell.error(`Error retrieving user ID from Auth0: ${error}`);
    return undefined;
  }
}

// Pick between the below functions based on sub prefix.
export async function findSisterSub(sub, options) {
  if (sub.startsWith("sotce-")) {
    return await aestheticSubFromSotceSub(sub);
  } else {
    return await sotceSubFromAestheticSub(sub, options);
  }
}

// Get `aesthetic` user id from a sotce user, if it exists.
// TODO: Cache this in redis to be faster? 24.09.01.00.40
export async function aestheticSubFromSotceSub(sotceSub) {
  const emailRes = await userEmailFromID(sotceSub, "sotce");
  if (emailRes?.email && emailRes?.email_verified) {
    const idRes = await userIDFromEmail(emailRes.email, "aesthetic");
    if (idRes?.userID && idRes?.email_verified) {
      return idRes.userID;
    }
  }
  return undefined;
}

// Get `sotce` user id from an aesthetic user, if it exists.
// TODO: Cache this in redis to be faster? 24.09.01.00.40
export async function sotceSubFromAestheticSub(aestheticSub, options) {
  const emailRes = await userEmailFromID(aestheticSub, "aesthetic");
  if (emailRes?.email && emailRes?.email_verified) {
    const idRes = await userIDFromEmail(emailRes.email, "sotce");
    if (idRes?.userID && idRes?.email_verified) {
      return (options?.prefixed ? "sotce-" : "") + idRes.userID;
    }
  }
  return undefined;
}

// Get the user email via their user ID.
export async function userEmailFromID(sub, tenant = "aesthetic", got, token) {
  try {
    if (!got) got = (await import("got")).got;
    if (sub.startsWith("sotce-")) tenant = "sotce"; // Switch tenant based on prefix.
    if (tenant === "sotce") sub = sub.replace("sotce-", "");
    const baseURI = tenant === "aesthetic" ? aestheticBaseURI : sotceBaseURI;
    if (!token) token = await getAccessToken(got, tenant);

    const userResponse = await got(`${baseURI}/api/v2/users/${sub}`, {
      headers: { Authorization: `Bearer ${token}` },
      responseType: "json",
    });

    const user = userResponse.body;
    const email = user?.email;
    return { email, email_verified: user?.email_verified };
  } catch (error) {
    shell.error(`Error retrieving user email from Auth0 (${tenant}): ${error}`);
    return undefined;
  }
}

// Takes in a user ID (sub) and returns the user's @handle (preferred) or email.
export async function getHandleOrEmail(sub) {
  try {
    // Attempt to get the user's handle.
    const handle = await handleFor(sub);
    if (handle) return "@" + handle;

    // If no handle is found, fetch the user's email from Auth0.
    const { got } = await import("got");
    const token = await getAccessToken(got); // Get access token for auth0.
    const userResponse = await got(
      `https://aesthetic.us.auth0.com/api/v2/users/${encodeURIComponent(sub)}`,
      { headers: { Authorization: `Bearer ${token}` }, responseType: "json" }
    );

    return userResponse.body.email;
  } catch (error) {
    shell.error(`Error retrieving user handle or email: ${error}`);
    return undefined;
  }
}

// Connects to the Redis cache or MongoDB database to obtain a user's handle
// from their ID (across tenants).
export async function handleFor(id, tenant = "aesthetic") {
  // const time = performance.now();

  // 📖 Get an aggregate list of all handles.
  if (id === "all") {
    const database = await connect();
    const collection = database.db.collection("@handles");
    const randomHandles = await collection
      .aggregate([{ $sample: { size: 100 } }, { $project: { handle: 1 } }])
      .toArray();
    await database.disconnect();
    return randomHandles.map((doc) => "@" + doc.handle);
  } else {
    // 🙆 Get a specific user handle.
    if (tenant === "sotce" && !id.startsWith("sotce-")) id = "sotce-" + id;
    shell.log("Retrieving handle for...", id);

    await KeyValue.connect();
    const cachedHandle = await KeyValue.get("userIDs", id);

    if (cachedHandle) {
      await KeyValue.disconnect();
      return cachedHandle;
    }

    const database = await connect();
    const collection = database.db.collection("@handles");
    let existingUser = await collection.findOne({ _id: id });

    // If no handle was found then try again on the sister tenant.
    if (!existingUser) {
      const sisterSub = await findSisterSub(id, { prefixed: true });

      if (sisterSub) {
        let foundHandle = await KeyValue.get("userIDs", sisterSub);
        if (foundHandle) {
          // Make sure to cache the original id for this handle.
          await KeyValue.set("userIDs", id, foundHandle);
          await KeyValue.disconnect();
          await database.disconnect();
          return foundHandle;
        } else {
          // Then in the database.
          existingUser = await collection.findOne({ _id: sisterSub });
          id = sisterSub;
        }
      }
    }

    // Cache the handle in redis for quick look up.
    if (existingUser?.handle) {
      await KeyValue.set("userIDs", existingUser._id, existingUser.handle);
    }

    await database.disconnect();
    await KeyValue.disconnect();

    // console.log("Time taken...", performance.now() - time);
    return existingUser?.handle;
  }
}

// Connects to the MongoDB database to obtain a user ID from a handle.
// Handle should not be prefixed with "@".

// ❤️‍🔥
// TODO: This could return a "sotce-" prefixed id which
//       would be incompatible with aesthetic computer if
//       an account does not exist / this function may need a
//       `tenant` parameter.
export async function userIDFromHandle(
  handle,
  database,
  keepKV,
  tenant = "aesthetic"
) {
  // Read from redis, otherwise check the database, and store in redis after.
  let userID;
  // const time = performance.now();
  await KeyValue.connect();
  const cachedUserID = await KeyValue.get("@handles", handle);

  if (tenant === "aesthetic" && cachedUserID?.startsWith("sotce-")) {
    return await aestheticSubFromSotceSub(cachedUserID);
  }

  if (!cachedUserID) {
    // Look in database.
    // if (dev) console.log("Handle: Looking in database...");
    const keepOpen = database; // Keep the db connection if database is defined.
    // if (dev) console.log("Handle: Connecting...", time);
    if (!database) database = await connect();
    const collection = database.db.collection("@handles");
    const user = await collection.findOne({ handle });
    userID = user?._id;
    if (!keepOpen) database.disconnect();
    if (tenant === "aesthetic" && userID.startsWith("sotce-")) {
      return await aestheticSubFromSotceSub(userID);
    }
  } else {
    // if (dev) console.log("Handle: Found in redis...");
    userID = cachedUserID;
  }

  // Cache userID in redis...
  if (!cachedUserID && userID) {
    if (dev) shell.log("Caching primary handle key in redis...", handle);
    await KeyValue.set("@handles", handle, userID);
    if (!keepKV) await KeyValue.disconnect();
  }

  // console.log("Time taken...", performance.now() - time);
  return userID;
}

// Assume prefixed handle.
// ⚠️ TODO: Make sure we are knowing what id we want from what network... 24.08.31.01.21
export async function userIDFromHandleOrEmail(handleOrEmail, database, tenant) {
  if (!handleOrEmail) return;
  if (handleOrEmail.startsWith("@")) {
    return userIDFromHandle(
      handleOrEmail.slice(1),
      database,
      undefined,
      tenant
    );
  } else {
    return userIDFromEmail(handleOrEmail, tenant); // Assume email.
  }
}

// Sets the user's email and triggers a re-verification email.
export async function setEmailAndReverify(
  id,
  email,
  name,
  tenant = "aesthetic"
) {
  try {
    const { got } = await import("got");
    const baseURI = tenant === "aesthetic" ? aestheticBaseURI : sotceBaseURI;

    const token = await getAccessToken(got, tenant);

    shell.log(
      "👮 📧 Setting and re-verifying email for:",
      email,
      "on:",
      tenant,
      "via:",
      id
    );

    // 1. Update the user's email and ('name' which is equivalent to email
    //    in auth0 but generally unused by Aesthetic Computer.)
    let updateEmailResponse;
    try {
      updateEmailResponse = await got(
        `${baseURI}/api/v2/users/${encodeURIComponent(id)}`,
        {
          method: "PATCH",
          headers: {
            Authorization: `Bearer ${token}`,
            "Content-Type": "application/json",
          },
          json: { name, email, email_verified: false },
          responseType: "json",
        }
      );
    } catch (err) {
      shell.error("🔴 Error:", err);
    }

    if (!updateEmailResponse.body) {
      throw new Error("Failed to update user email");
    }

    // 2. Trigger the verification email
    const verificationResponse = await got(
      `${baseURI}/api/v2/jobs/verification-email`,
      {
        method: "POST",
        headers: {
          Authorization: `Bearer ${token}`,
          "Content-Type": "application/json",
        },
        json: { user_id: id },
        responseType: "json",
      }
    );

    if (!verificationResponse.body) {
      throw new Error("Failed to send verification email");
    }

    return {
      success: true,
      message: "Email updated and verification email sent successfully!",
    };
  } catch (error) {
    shell.error(`Error setting email and sending verification: ${error}`);
    return {
      success: false,
      message: error.message,
    };
  }
}

// Deletes a user from auth0.
export async function deleteUser(userId, tenant = "aesthetic") {
  try {
    const { got } = await import("got");
    const token = await getAccessToken(got, tenant);
    const baseURI = tenant === "aesthetic" ? aestheticBaseURI : sotceBaseURI;

    await got(`${baseURI}/api/v2/users/${encodeURIComponent(userId)}`, {
      method: "DELETE",
      headers: { Authorization: `Bearer ${token}` },
    });

    shell.log(
      `❌ User with ID ${userId} deleted from Auth0. Tenant: ${tenant}`
    );
    return { success: true, message: "User deleted successfully from Auth0." };
  } catch (error) {
    shell.error(`⚠️  Error deleting user from Auth0: ${error}`);
    return { success: false, message: error.message };
  }
}

// Queries the total number of signed-up users by including totals in the response.
export async function querySignups(tenant = "aesthetic") {
  try {
    const { got } = await import("got");
    const baseURI = tenant === "aesthetic" ? aestheticBaseURI : sotceBaseURI;
    const token = await getAccessToken(got, tenant);

    const response = await got(`${baseURI}/api/v2/users`, {
      searchParams: {
        page: 0,
        per_page: 1, // Fetch minimal data to reduce overhead.
        include_totals: true, // Include the total user count.
      },
      headers: { Authorization: `Bearer ${token}` },
      responseType: "json",
    });

    return response.body.total || 0; // Return the total user count.
  } catch (error) {
    shell.error(`Error querying signups from Auth0: ${error}`);
    return undefined;
  }
}

// Retrieves daily stats for logins and signups from the Auth0 stats endpoint.
//
export async function activeUsers(tenant = "aesthetic") {
  try {
    const { got } = await import("got");
    const baseURI = tenant === "aesthetic" ? aestheticBaseURI : sotceBaseURI;
    const token = await getAccessToken(got, tenant);

    const response = await got(`${baseURI}/api/v2/stats/active-users`, {
      headers: { Authorization: `Bearer ${token}` },
      responseType: "json",
    });

    return response.body; // Returns an array of daily stats.
  } catch (error) {
    shell.error(`Error fetching daily stats from Auth0: ${error}`);
    return undefined;
  }
}

// 📚 Library (Useful functions used throughout the file.)
// Obtain an auth0 access token for our M2M API.
async function getAccessToken(got, tenant = "aesthetic") {
  let baseURI, client_id, client_secret;
  if (tenant === "aesthetic") {
    baseURI = aestheticBaseURI;
    client_id = process.env.AUTH0_M2M_CLIENT_ID;
    client_secret = process.env.AUTH0_M2M_SECRET;
  } else {
    // assume tenant is `sotce`.
    baseURI = sotceBaseURI;
    client_id = process.env.SOTCE_AUTH0_M2M_CLIENT_ID;
    client_secret = process.env.SOTCE_AUTH0_M2M_SECRET;
  }

  const tokenResponse = await got(`${baseURI}/oauth/token`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    json: {
      client_id,
      client_secret,
      audience: `${baseURI}/api/v2/`,
      grant_type: "client_credentials", // Use "client_credentials" for M2M
    },
    responseType: "json",
  });

  return tokenResponse.body.access_token;
}
