// `aesthetic.computer` Router, 23.05.02.12.39
// This is a cloudflare worker that manages requests for user uploaded media
// on aesthetic.computer.

// Routes:

// GET `/media/{slug_with_extension}` will download media from the art
//     bucket.

// GET `/media/${userHandleOrEmail}/${slug_with_extension}` will download
//     individual files by proxy. ✅

// GET `/media/${userHandleOrEmail}/${media_type} will fetch json of all
//     media of that type (subdirectory) directly from the buckets,
//     sorted by upload date.

/* #region 📝 TODO 
  + Now
  - [] Try making this a Netlify worker. If only to close the development loop. 
#endregion */

/* #region 📔 readme
  - See the `package.json` for tasks.
#endregion */

let dev = false;
export default {
  async fetch(request, env, ctx) {
    if (env.MODE === "development") dev = true;
    return await handleRequest(request);
  },
};

async function handleRequest(request) {
  let url = new URL(request.url);
  let path = url.pathname.split("/");
  let newUrl;

  if (path[1] === "media") {
    if (path[2].indexOf("@") === -1) {
      // 💁 Guest Media
      newUrl = `https://art.aesthetic.computer/${path[2]}`;
    } else {
      // 🖼️ User Media
      const userId = await queryUserID(path[2]);
      let newPath = `${userId}/${path.slice(3).join("/")}`;

      if (!newPath.endsWith(".png")) newPath += ".png"; // Allow routing without extensions. (default to PNG).

      if (newPath.split("/").pop().split(".")[1]?.length > 0) {
        // The path has a file extension / points to an individual file.
        newUrl = `https://user.aesthetic.computer/${newPath}`;
      } else {
        // The path should return a collection.
        const path = encodeURIComponent(newPath.replace("/media", ""));
        newUrl = `https://aesthetic.computer/media-collection?for=${path}`;
      }
    }

    // Use the resolveOverride property to rewrite the request instead
    // of fetching the whole file.
    request = new Request(newUrl, {
      ...request,
      cf: { ...request.cf, resolveOverride: newUrl },
    });

    return fetch(request);
  } else {
    // For other paths, just return a response.
    return new Response("💾 Not a `media` path.", { status: 500 });
  }
}

async function queryUserID(username) {
  const host = "https://aesthetic.computer";
  // const host = dev ? "https://localhost:8888" : "https://aesthetic.computer";
  // ❓ For some reason I cannot call netlify functions locally with :8888
  //    (403 errors) so I'm turning it off for now.
  //    This is probably because local Cloudflare workers / wrangler does
  //    not boot an `https` server. 23.05.03.11.25
  const url = `${host}/user?from=${username}`;
  try {
    const res = await fetch(url);
    if (res.ok) {
      const json = await res.json();
      return json.sub;
    } else {
      console.error(`Error: ${res.status} ${res.statusText}`);
      console.error(
        `Response headers: ${JSON.stringify(Array.from(res.headers.entries()))}`
      );
    }
  } catch (error) {
    console.error(`Fetch failed: ${error}`);
  }
}

