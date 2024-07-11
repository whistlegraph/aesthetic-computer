// Media

export const config = { path: "/media/*" };

export default async function handleRequest(request) {
  const url = new URL(request.url);
  const path = url.pathname.split("/");
  let newUrl;

  if (path[1] === "media") {
    if (path[2].indexOf("@") === -1) {
      newUrl = `https://art.aesthetic.computer/${path[2]}`;
      return fetch(encodeURI(newUrl));
    } else {
      const userId = await queryUserID(path[2]);
      const newPath = `${userId}/${path.slice(3).join("/")}`;

      if (newPath.split("/").pop().split(".")[1]?.length > 0) {
        if (newPath.split(".").pop() === "mjs") {
          newUrl = `https://user-aesthetic-computer.sfo3.digitaloceanspaces.com/${newPath}`;
        } else {
          newUrl = `https://user.aesthetic.computer/${newPath}`;
        }
        // TODO: How can I ensure that Allow-Origin * can be here?
        const response = await fetch(encodeURI(newUrl));
        // Create a new Response object using the fetched response's body

        const contentType = response.headers.get("Content-Type");

        const moddedResponse = new Response(response.body, {
          // Copy all the fetched response's headers
          headers: { ...response.headers },
          status: response.status,
          statusText: response.statusText,
        });
        // // Set the Access-Control-Allow-Origin header to *
        moddedResponse.headers.set("Access-Control-Allow-Origin", "*");
        moddedResponse.headers.set("Content-Type", contentType);
        return moddedResponse;
        // return fetch(encodeURI(newUrl));
      } else {
        const path = newPath.replace("/media", "");
        newUrl = `/media-collection?for=${path}`;
        return new URL(newUrl, request.url);
      }
    }
  } else {
    return new Response("💾 Not a `media` path.", { status: 500 });
  }
}

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