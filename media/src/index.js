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
    console.log(env);
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
    const newUrl = `https://user.aesthetic.computer/${newPath}`;
    const response = await fetch(newUrl);
    return response;
  } else {
    // For other paths, just fetch the resource as is
    return fetch(request);
  }
}

async function queryUserID(username) {
  const host = dev ? "https://localhost:8888" : "https://aesthetic.computer";
	// TODO: This doesn't seem to work locally...
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