// Parser, 2022.7.14.17.53
// Parses everything that can be typed into the `prompt` piece and anything
// that appears after `aesthetic.computer/` in the address bar of the browser.

// TODO:
// [] This should eventually have tests that run?

// Notes:
// Allowed URL fragments include: https://stackoverflow.com/a/2849800/8146077
// ! $ & ' ( ) * + , ; = - . _ ~ : @ / ?

// Returns a hostname, piece path, and parameters to load a piece by.
// - Used in both the URL bar of the browser, and the `prompt` piece.
// Accepts: bpm 180
//          bpm~180
//          niki/bpm 180
//          niki/bpm~180
//          game.jas.life/bpm~180?mute=true (not working yet?) 22.09.22.12.05
//          niki

function parse(text, location = self?.location) {
  let path, host, params, search, hash;

  // Extract remote path from text if it begins with https and ends with `.mjs`.
  let externalPath;
  if (text.startsWith("https") && text.endsWith(".mjs")) {
    const url = new URL(text);
    location = { hostname: url.hostname, port: url.port };
    externalPath = url.pathname.split("/").slice(0, -1).join("/").slice(1);
    text = text.split("https://")[1].split(".mjs")[0].split("/").pop();
  }

  // Check for any anon path (begins with $).
  // if (text.startsWith("$")) {
  //   text = text.slice(1);
  //   const url = new URL(`https://art.aesthetic.computer/${text.slice(1)}.mjs`);
  //   location = { hostname: url.hostname, port: url.port };
  //   externalPath = ""; //url.pathname.split("/").slice(0, -1).join("/").slice(1);
  //   console.log(location, externalPath);
  // }

  // -1. Clear any spaces.
  text = text.trim();

  // Squish any spaces inside of colon parameters.
  text = text.replace(/\s*:\s*/g, ":"); // Squash space before & after colons.
  text = text.replace(/ /g, "~"); // Replace all spaces with "~".
  text = decodeURIComponent(text); // Decode any URL encoded characters.

  // 0. Pull off any "hash" from text, filtering the edge case for #hex colors.
  [text, hash] = text.replaceAll("~#", "~0x").split("#");

  // 1. Pull off any "search" from `text`, ignoring any question mark
  //    characters that were part of the piece slug.

  if (text[0] === "?") {
    // Special case for empty path with a search param.
    search = text.slice(1);
    text = window?.acSTARTING_PIECE || "prompt";
  } else {
    // TODO: This does not keep the question marks on this string but it should... https://localhost:8888/line:5~?~?~?
    let searchIndex = text.search(/[^~]\?[^~]/); // Filter single "?" params.
    if (searchIndex >= 0) [text, search] = text.split("?");
  }

  // TODO: When to parse the search query string into a URLSearchParams object?
  //       https://developer.mozilla.org/en-US/docs/Web/API/URLSearchParams

  // 1.5 Strip any trailing slash off of 'text'. (So stuff like prompt/#nodebug works)
  // console.log(text);
  if (text.endsWith("/")) text = text.slice(0, -1);

  // 2. Tokenize on " " or "~".
  const tokens = text.split("~");

  // 3. Determine the host and path.
  let handlePiece = false;
  // Set a `customHost` flag if we are loading a @user/piece.
  if (tokens[0].indexOf("@") === 0 && tokens[0].indexOf("/") !== -1) {
    handlePiece = true;
    // tokens[0] = tokens[0].substring(1);
  }

  // Extract colon parameter...
  let colonParam;
  const colonSplit = tokens[0].split(":");

  if (colonSplit.length > 0) {
    tokens[0] = colonSplit[0];
    colonParam = colonSplit.slice(1);
  }

  const piece = tokens[0];

  if (handlePiece) {
    // Route the piece to the local `/media/@handle/code/piece-name` path.
    host = location.hostname;
    if (location.port) host += ":" + location.port;
    const [handle, name] = tokens[0].split("/");
    path = `media/${handle}/piece/${name}`;
  } else {
    host = location.hostname;
    if (location.port) host += ":" + location.port;

    if (externalPath !== undefined) {
      if (externalPath.length === 0) {
        path = tokens[0];
      } else {
        path = externalPath + "/" + tokens[0];
      }
    } else {
      path = "aesthetic.computer/disks/" + tokens[0];
    }
  }

  // 4. Get params. (Everything that comes after the path and host)
  params = tokens.slice(1);
  return { host, path, piece, colon: colonParam, params, search, hash, text };
}

// Cleans a url for feeding into `parse` as the text parameter.
function slug(url) {
  // Remove http protocol and host from current url before feeding it to parser.
  return url
    .replace(/^http(s?):\/\//i, "")
    .replace(window.location.hostname + ":" + window.location.port + "/", "")
    .replace(window.location.hostname + "/", "")
    .split("#")[0]; // Remove any hash.
  // .split("?")[0]; // Remove any search param.
}

// Generates some metadata fields that are shared both on the client and server.
function metadata(host, slug, pieceMetadata) {
  // Use a default title if there is no override.
  const title =
    pieceMetadata?.title ||
    (slug !== "prompt" ? slug + " · Aesthetic Computer" : "Aesthetic Computer");
  // Use existing or default description.
  const desc = pieceMetadata?.desc || "An Aesthetic Computer piece.";

  // See also: `index.js`
  let ogImage, twitterImage;
  let icon;
  if (pieceMetadata?.image_url) {
    ogImage = twitterImage = pieceMetadata.image_url;
  } else {
    ogImage = `https://${host}/thumbnail/1200x630/${slug}.png`;
    twitterImage = `https://${host}/thumbnail/1800x900/${slug}.png`;
  }

  icon = pieceMetadata?.icon_url || `https://${host}/icon/128x128/${slug}.png`;

  return { title, desc, ogImage, twitterImage, icon };
}

export { parse, slug, metadata };
