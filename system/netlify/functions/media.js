// Media Proxy Function
// Proxies media requests to DigitalOcean Spaces, converting handles to user IDs
// This is for local development - production uses the edge function

import { userIDFromHandleOrEmail } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";

export async function handler(event, context) {
  const path = event.path.split("/");
  
  if (path[1] !== "media") {
    return {
      statusCode: 500,
      body: "💾 Not a `media` path.",
    };
  }

  const resourcePath = path.slice(2).join("/");
  
  // If the path doesn't include @, it's a direct resource path
  if (!path[2]?.includes("@")) {
    const extension = resourcePath.split(".").pop()?.toLowerCase();
    const baseUrl =
      extension === "mjs"
        ? "https://user-aesthetic-computer.sfo3.digitaloceanspaces.com"
        : "https://user.aesthetic.computer";

    const targetUrl = `${baseUrl}/${resourcePath}`;
    
    try {
      const response = await fetch(encodeURI(targetUrl));
      const body = await response.arrayBuffer();
      const contentType = response.headers.get("Content-Type");

      return {
        statusCode: response.status,
        headers: {
          "Content-Type": contentType || "application/octet-stream",
          "Access-Control-Allow-Origin": "*",
        },
        body: Buffer.from(body).toString("base64"),
        isBase64Encoded: true,
      };
    } catch (error) {
      console.error("Media fetch error:", error);
      return {
        statusCode: 500,
        body: `Error fetching media: ${error.message}`,
      };
    }
  } else {
    // Handle @username paths - convert handle to user ID
    const database = await connect();
    const userId = await userIDFromHandleOrEmail(path[2], database);
    await database.disconnect();

    if (!userId) {
      return {
        statusCode: 404,
        body: "User not found",
      };
    }

    const newPath = `${userId}/${path.slice(3).join("/")}`;

    // Check if this is a file request (has extension)
    if (newPath.split("/").pop().split(".")[1]?.length > 0) {
      const extension = newPath.split(".").pop();
      let targetUrl;
      
      if (extension === "mjs") {
        targetUrl = `https://user-aesthetic-computer.sfo3.digitaloceanspaces.com/${newPath}`;
      } else {
        targetUrl = `https://user.aesthetic.computer/${newPath}`;
      }

      try {
        const response = await fetch(encodeURI(targetUrl));
        const body = await response.arrayBuffer();
        const contentType = response.headers.get("Content-Type");

        return {
          statusCode: response.status,
          headers: {
            "Content-Type": contentType || "application/octet-stream",
            "Access-Control-Allow-Origin": "*",
          },
          body: Buffer.from(body).toString("base64"),
          isBase64Encoded: true,
        };
      } catch (error) {
        console.error("Media fetch error:", error);
        return {
          statusCode: 500,
          body: `Error fetching media: ${error.message}`,
        };
      }
    } else {
      // If no file extension, redirect to media-collection
      const collectionPath = newPath.replace("/media", "");
      return {
        statusCode: 302,
        headers: {
          Location: `/media-collection?for=${collectionPath}`,
        },
        body: "",
      };
    }
  }
}
