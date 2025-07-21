// Parser, 2022.7.14.17.53
// Parses everything that can be typed into the `prompt` piece and anything
// that appears after `aesthetic.computer/` in the address bar of the browser.

import { isKidlispSource, decodeKidlispFromUrl } from "./kidlisp.mjs";

// Helper function to distinguish actual kidlisp from music notation
function isActualKidlisp(text) {
  // Music notation typically contains note letters (a-g), octave numbers, and musical symbols
  // Kidlisp typically contains function names like wipe, ink, line, etc.
  
  // If it contains clear kidlisp function names, it's kidlisp
  if (/\b(wipe|ink|line|box|def|later|circle|poly|resolution|width|height|\+|\*|-|\/)\b/.test(text)) {
    return true;
  }
  
  // If it starts with parentheses and looks like music notation, it's probably not kidlisp
  // Music notation patterns: (abc), (cdefg), (c*d), (eee*ggg), etc.
  const musicPattern = /^\([a-g#\d\s\*\.\-\_\<\>]*\)$/i;
  if (musicPattern.test(text.trim())) {
    return false;
  }
  
  // If it contains multiple music-like groups in parentheses, probably music
  const multiMusicPattern = /\([a-g#\d\s\*\.\-\_\<\>]*\)\s*\([a-g#\d\s\*\.\-\_\<\>]*\)/i;
  if (multiMusicPattern.test(text)) {
    return false;
  }
  
  // Default to treating it as kidlisp if we can't determine
  return true;
}

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
  if (
    text.startsWith("https") &&
    (text.endsWith(".mjs") || text.endsWith(".lisp"))
  ) {
    const url = new URL(text);
    location = { hostname: url.hostname, port: url.port };
    externalPath = url.pathname.split("/").slice(0, -1).join("/").slice(1);
    text = text
      .split("https://")[1]
      .split(/\.mjs|\.lisp/)[0]
      .split("/")
      .pop();
  }
  text = text.trim(); // Clear any spaces.  
  
  // 🚨 Special case for prompt~ slugs - ALWAYS route to prompt piece
  // This prevents prompt~(wipe blue) from being treated as kidlisp function call
  if (text.startsWith("prompt~")) {
    const promptContent = text.slice(7); // Remove "prompt~" prefix
    console.log("🔍 PARSE: Handling prompt~ slug. Original text:", text, "Prompt content:", promptContent);
    
    // Check if this is actually kidlisp vs music notation or other content
    let decodedContent;
    if (isKidlispSource(promptContent) && isActualKidlisp(promptContent)) {
      decodedContent = decodeKidlispFromUrl(promptContent);
      console.log("🔍 PARSE: Decoded as kidlisp:", decodedContent);
    } else {
      // For music notation like (ccc*___) or regular piece names, use as-is
      decodedContent = promptContent;
      console.log("🔍 PARSE: Using as-is (not kidlisp):", decodedContent);
    }
    
    const result = {
      host: location.hostname + (location.port ? ":" + location.port : ""),
      path: "aesthetic.computer/disks/prompt",
      piece: "prompt",
      colon: undefined,
      params: [decodedContent], // Pass the content as a parameter
      search: undefined,
      hash: undefined,
      text: text,
    };
    console.log("🔍 PARSE: Returning prompt result:", result);
    return result;
  }
  
  // 🤖 Early kidlisp detection - ONLY for URL-encoded kidlisp (not regular input)
  // This catches cases like /(wipe_blue) or /wipe_blue_line from URL refresh
  // BUT NOT regular multiline kidlisp input from the prompt
  // AND NOT piece URLs like share~(wipe_blue) where "share" is the piece name
  const kidlispCheck = isKidlispSource(text);
  const hasSpecialChars = text.includes("§") ||
    text.includes("_") ||
    text.includes("\n") ||
    text.startsWith("(") ||
    text.startsWith(";");
  // NOTE: Removed text.includes("~") from hasSpecialChars because ~ is used
  // as a parameter separator in regular piece URLs like "line~red"
  
  // Check if this looks like a piece with parameters (piece~params format)
  const tildeIndex = text.indexOf("~");
  const hasPiecePrefix = tildeIndex > 0 && tildeIndex < text.length - 1;
  const firstPart = hasPiecePrefix ? text.substring(0, tildeIndex) : text;
  
  // Only treat as standalone kidlisp if it passes kidlisp detection, has special chars,
  // AND doesn't look like a piece name with parameters
  if (kidlispCheck && hasSpecialChars && !hasPiecePrefix) {
    const decodedSource = decodeKidlispFromUrl(text);
    return {
      host: location.hostname + (location.port ? ":" + location.port : ""),
      path: "(...)", // Use a special path indicator for kidlisp
      piece: "(...)",
      colon: undefined,
      params: [],
      search: undefined,
      hash: undefined,
      text: decodedSource,
      source: decodedSource, // Include the decoded source code
      name: decodedSource, // Use the source as the name too
    };
  }

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

  // 🤖 Check if this is a standalone kidlisp source (no piece name prefix)
  if (tokens.length === 1 && isKidlispSource(tokens[0])) {
    // This is pure kidlisp source code, decode it
    const decodedSource = decodeKidlispFromUrl(tokens[0]);
    return {
      host: location.hostname + (location.port ? ":" + location.port : ""),
      path: "(...)", // Use a special path indicator for kidlisp
      piece: "(...)",
      colon: undefined,
      params: [],
      search: undefined,
      hash: undefined,
      text: decodedSource,
      source: decodedSource, // Include the decoded source code
      name: decodedSource, // Use the source as the name too
    };
  }

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
  let cleanedUrl = url
    .replace(/^http(s?):\/\//i, "")
    .replace(window.location.hostname + ":" + window.location.port + "/", "")
    .replace(window.location.hostname + "/", "")
    .split("#")[0] // Remove any hash.
    .split("?")[0]; // Remove any search params (important for kidlisp with session params)

  // Decode URL-encoded characters first
  cleanedUrl = decodeURIComponent(cleanedUrl);

  // Use centralized kidlisp URL decoding
  return decodeKidlispFromUrl(cleanedUrl);
}

// Read first two lines of JavaScript or Lisp source to pull off a title & desc.
function inferTitleDesc(source) {
  let title, desc;
  const lines = source.split("\n");

  // Check for JavaScript-style comments or Lisp comments
  if (lines[0].startsWith("//") || lines[0].startsWith(";")) {
    title = lines[0].split(",")[0].slice(2).trim();
  }

  if (lines[1]?.startsWith("//") || lines[1]?.startsWith(";")) {
    desc = lines[1].slice(2).trim();
  }
  
  // If title was extracted from a Lisp comment (starts with ;), mark it as standalone
  const standaloneTitle = lines[0]?.startsWith(";") && !!title;
  
  return { title, desc, standaloneTitle };
}

// Generates some metadata fields that are shared both on the client and server.
function metadata(host, slug, pieceMetadata) {
  // Use a default title if there is no override.
  const notAesthetic =
    host.indexOf("sotce") > -1 ||
    host.indexOf("botce") > -1 ||
    host.indexOf("wipppps.world") > -1;
  
  // Check if this is a standalone title from Lisp comment (skip " · Aesthetic Computer")
  const isStandaloneTitle = pieceMetadata?.standaloneTitle === true;
  
  let title;
  if (pieceMetadata?.title) {
    // Use the piece's custom title, with or without suffix based on standaloneTitle flag
    title = pieceMetadata.title + (notAesthetic || isStandaloneTitle ? "" : " · Aesthetic Computer");
  } else {
    // Fallback to slug-based title
    title = slug !== "prompt" ? slug + " · Aesthetic Computer" : "Aesthetic Computer";
  }
  // Use existing or default description.
  const desc = pieceMetadata?.desc || "An Aesthetic Computer piece.";

  // See also: `index.js`
  let ogImage, twitterImage;
  let icon;
  if (pieceMetadata?.image_url) {
    ogImage = twitterImage = pieceMetadata.image_url;
  } else {
    ogImage = `https://${host}/preview/1200x630/${slug}.png`;
    twitterImage = `https://${host}/preview/1800x900/${slug}.png`;
  }

  icon = pieceMetadata?.icon_url || `https://${host}/icon/128x128/${slug}.png`;

  const manifest = `https://${host}/manifest.json`;
  return { title, desc, ogImage, twitterImage, icon, manifest };
}

// Modify source code imports etc / pre-process.
function updateCode(
  sourceToRun,
  host,
  debug,
  protocol = location?.protocol,
  serverRewrites = false,
  serverRewritesPath = "",
) {
  let updatedCode = sourceToRun;

  // Automatically replace relative imports with absolute ones.
  const twoDots =
    /^(import|export) {([^{}]*?)} from ["'](\.\.\/|\.\.|\.\/)(.*?)["'];?/gm;
  const oneDot = /^(import|export) \* as ([^ ]+) from ["']\.?\/(.*?)["'];?/gm;

  const replaceTwoDots = (match, p1, p2, p3, p4) => {
    let url;
    if (serverRewrites) {
      url = `..${serverRewritesPath}/public/aesthetic.computer${
        p3 === "./" ? "/disks" : ""
      }/${p4.replace(/\.\.\//g, "")}`;
    } else {
      url = `${protocol}//${host}/aesthetic.computer${
        p3 === "./" ? "/disks" : ""
      }/${p4.replace(/\.\.\//g, "")}`;
    }
    return `${p1} {${p2}} from "${url}";`;
  };

  // /workspaces/aesthetic-computer
  const replaceOneDot = (match, p1, p2, p3) => {
    let url;
    if (serverRewrites) {
      url = `..${serverRewritesPath}/public/aesthetic.computer${
        p3.startsWith("disks/") ? "" : "/disks"
      }/${p3.replace(/^disks\//, "")}`;
    } else {
      url = `${protocol}//${host}/aesthetic.computer${
        p3.startsWith("disks/") ? "" : "/disks"
      }/${p3.replace(/^disks\//, "")}`;
    }
    return `${p1} * as ${p2} from "${url}";`;
  };

  updatedCode = updatedCode.replace(twoDots, replaceTwoDots);
  updatedCode = updatedCode.replace(oneDot, replaceOneDot);

  // 💉 Constant Injection (for pieces to use)
  // Inject the DEBUG constant into the updatedCode
  // ⚠️ Always make sure to document 📚 added constants in `docs`!
  updatedCode = `const DEBUG = ${debug};\n${updatedCode}`;

  // 📥 Hunt for and preloading any user media.
  updatedCode = addExportsToCode(updatedCode);
  return updatedCode;
}

// Find top-level whitelisted functions and auto-export them for `.mjs` pieces.
function addExportsToCode(code) {
  // Define the whitelist of functions to export
  const whitelist = [
    "paint",
    "boot",
    "act",
    "sim",
    "meta",
    "brush",
    "preview",
    "icon",
    "beat",
    "brush",
    "filter",
  ];
  // Remove comments from the code more carefully
  const codeWithoutComments = code
    .split("\n")
    .map((line) => {
      // Only remove comments that are not inside strings
      let inString = false;
      let stringChar = "";
      let result = "";

      for (let i = 0; i < line.length; i++) {
        const char = line[i];
        const nextChar = line[i + 1];

        if (!inString && (char === '"' || char === "'" || char === "`")) {
          inString = true;
          stringChar = char;
          result += char;
        } else if (inString && char === stringChar && line[i - 1] !== "\\") {
          inString = false;
          stringChar = "";
          result += char;
        } else if (!inString && char === "/" && nextChar === "/") {
          // Found a comment outside of a string, stop processing this line
          break;
        } else if (!inString && char === "/" && nextChar === "*") {
          // Found start of block comment outside string, skip until end
          i += 2;
          while (i < line.length - 1) {
            if (line[i] === "*" && line[i + 1] === "/") {
              i += 2;
              break;
            }
            i++;
          }
          i--; // Adjust for loop increment
        } else {
          result += char;
        }
      }

      return result;
    })
    .join("\n");

  // Check if the file already contains an 'export { ... }' statement
  const hasExportObject = /export\s+{[^}]*}/m.test(codeWithoutComments);

  // If no 'export { ... }' statement exists, build an export statement for top-level functions in the whitelist
  if (!hasExportObject) {
    // Regex to match top-level function definitions
    const topLevelFunctionRegex = /^function\s+(\w+)\s*\(/gm;
    const topLevelFunctions = [];

    let match;
    while ((match = topLevelFunctionRegex.exec(codeWithoutComments)) !== null) {
      const functionName = match[1];
      if (whitelist.includes(functionName)) {
        topLevelFunctions.push(functionName);
      }
    }

    if (topLevelFunctions.length > 0) {
      code += `\nexport { ${topLevelFunctions.join(", ")} };`;
    }
  }

  return code;
}

export { parse, slug, metadata, updateCode, inferTitleDesc, addExportsToCode };
