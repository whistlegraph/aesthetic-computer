// File location: netlify/edge-functions/media-handler.js

export default async (event) => {
  const { request } = event;
  let url = new URL(request.url);
  let path = url.pathname.split("/");
  let newUrl;

  if (path[1] === "media") {
    if (path[2].indexOf("@") === -1) {
      newUrl = `https://art.aesthetic.computer/${path[2]}`;
    } else {
      const userId = await queryUserID(path[2]);
      let newPath = `${userId}/${path.slice(3).join("/")}`;

      if (newPath.split("/").pop().split(".")[1]?.length > 0) {
        newUrl = `https://user.aesthetic.computer/${newPath}`;
      } else {
        const path = encodeURIComponent(newPath.replace("/media", ""));
        newUrl = `https://aesthetic.computer/media-collection?for=${path}`;
      }
    }

    return fetch(newUrl, {
      method: request.method,
      headers: request.headers,
    });
  } else {
    return new Response("💾 Not a `media` path.", { status: 500 });
  }
};

export const config = { path: "/media/*" };

async function queryUserID(username) {
  const host = "https://aesthetic.computer";
  const url = `${host}/user?from=${encodeURIComponent(username)}`;
  try {
    const res = await fetch(url);
    if (res.ok) {
      const json = await res.json();
      return json.sub;
    } else {
      console.error(`Error: ${res.status} ${res.statusText}`);
      console.error(
        `Response headers: ${JSON.stringify(
          Array.from(res.headers.entries()),
        )}`,
      );
    }
  } catch (error) {
    console.error(`Fetch failed: ${error}`);
  }
}
