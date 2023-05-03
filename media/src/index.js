// Media, 23.05.02.12.39
// This is a cloudflare worker that manages requests for user uploaded media
// on aesthetic.computer.

// GET `/media/{@userHandleOrEmail}/{slug_with_extension}` will download
//     individual files by proxy. ✅

/* #region 📔 readme
	Welcome to Cloudflare Workers! This is your first worker.
	- Run `npx wrangler dev src/index.js` in your terminal to start a dev server
	- Open a browser tab at http://localhost:8787/ to see your worker in action
	- Run `npx wrangler publish src/index.js --name my-worker` to publish
	Learn more at https://developers.cloudflare.com/workers/
#endregion */

let dev = false;
export default {
  async fetch(request, env) {
    if (env.MODE === "development") dev = true;
    return await handleRequest(request);
  },
};

async function handleRequest(request) {
  let url = new URL(request.url);
  let path = url.pathname.split("/");
  if (path[1] === "media") {
    const userId = await queryUserID(path[2]);
    const newPath = `${userId}/${path.slice(3).join("/")}`;
    let response;
    if (newPath.split("/").pop().split(".")[1]?.length > 0) {
      // The path has a file extension / points to an individual file.
      response = await fetch(`https://user.aesthetic.computer/${newPath}`);
    } else {
      // The path should return a collection.
      const path = encodeURIComponent(newPath.replace("/media", ""));

      // ❓ This url also not be tested locally for the same reasons as below.
      response = await fetch(
        `https://aesthetic.computer/media-collection?for=${path}`
      );
    }

    return response;
  } else {
    // For other paths, just fetch the resource as is
    return fetch(request);
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
