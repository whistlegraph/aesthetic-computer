// Media

export const config = { path: "/media/*" };

export default async function handleRequest(request) {
  const url = new URL(request.url);
  const path = url.pathname.split("/");
  let newUrl;

  if (path[1] === "media") {
    const resourcePath = path.slice(2).join("/");

    if (!path[2]?.includes("@") && !path[2]?.match(/^ac[a-z0-9]+$/i)) {
      // No @ prefix and not a user code (acXXXXX format) - treat as direct file path
      const extension = resourcePath.split(".").pop()?.toLowerCase();
      const baseUrl =
        extension === "mjs"
          ? "https://user-aesthetic-computer.sfo3.digitaloceanspaces.com"
          : "https://user.aesthetic.computer";

      newUrl = `${baseUrl}/${resourcePath}`;
      const response = await fetch(encodeURI(newUrl));
      const contentType = response.headers.get("Content-Type");
      const moddedResponse = new Response(response.body, {
        headers: { ...response.headers },
        status: response.status,
        statusText: response.statusText,
      });
      moddedResponse.headers.set("Access-Control-Allow-Origin", "*");
      if (contentType) {
        moddedResponse.headers.set("Content-Type", contentType);
      }
      return moddedResponse;
    } else {
      // Handle both @username and acXXXXX user code formats
      const userIdentifier = path[2];
      const userId = await queryUserID(userIdentifier);
      
      if (!userId) {
        return new Response(`User not found: ${userIdentifier}`, { status: 404 });
      }
      
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

async function queryUserID(userIdentifier) {
  // Use localhost in dev, production URL otherwise
  const host = Deno.env.get("CONTEXT") === "dev" 
    ? "https://localhost:8888" 
    : "https://aesthetic.computer";
  
  // Determine if it's a user code (acXXXXX) or handle (@username)
  let url;
  if (userIdentifier.match(/^ac[a-z0-9]+$/i)) {
    // User code format - query by code
    url = `${host}/user?code=${encodeURIComponent(userIdentifier)}`;
  } else {
    // Handle format (with or without @)
    url = `${host}/user?from=${encodeURIComponent(userIdentifier)}`;
  }
  
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
      return null;
    }
  } catch (error) {
    console.error(`Fetch failed: ${error}`);
    return null;
  }
}