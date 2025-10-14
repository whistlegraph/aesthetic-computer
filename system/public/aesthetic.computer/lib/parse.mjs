// Parser, 2022.7.14.17.53
// Parses everything that can be typed into the `prompt` piece and anything
// that appears after `aesthetic.computer/` in the address bar of the browser.

import { isKidlispSource, decodeKidlispFromUrl } from "./kidlisp.mjs";
import { checkPackMode } from "./pack-mode.mjs";

// List of legitimate query parameters that should be preserved
const LEGITIMATE_PARAMS = [
  "icon",
  "preview",
  "signup",
  "supportSignUp",
  "success",
  "code",
  "supportForgotPassword",
  "message",
  "vscode",
  "nogap",
  "nolabel",
  "density",
  "zoom",
  "duration",
  "session-aesthetic",
  "session-sotce",
  "notice",
  "tv",
  "highlight",
];

const LEGITIMATE_PARAM_SET = new Set(
  LEGITIMATE_PARAMS.map((param) => param.toLowerCase()),
);

// Auth0 parameters that should be stripped from paths
const AUTH0_PARAMS_TO_STRIP = ["state", "code", "error", "error_description"];

function isInsideParentheses(text, index) {
  let depth = 0;
  for (let i = 0; i < index; i++) {
    if (text[i] === "(") {
      depth += 1;
    } else if (text[i] === ")" && depth > 0) {
      depth -= 1;
    }
  }
  return depth > 0;
}

function shouldTreatAsQuery(text, questionIndex) {
  if (questionIndex <= 0 || questionIndex >= text.length) return false;

  const prevChar = text[questionIndex - 1];
  const nextChar = text[questionIndex + 1];

  // Ignore cases like ~?~ which are positional arguments
  if (prevChar === "~" || nextChar === "~") return false;

  // Ignore question marks inside parenthetical KidLisp content
  if (isInsideParentheses(text, questionIndex)) return false;

  const rawCandidate = text.slice(questionIndex + 1);
  if (!rawCandidate) return false;

  // Strip leading tildes which represent prior spacing
  const candidate = rawCandidate.replace(/^~+/, "");
  if (!candidate) return false;

  const firstChunk = candidate.split(/[~&#]/)[0] || "";
  if (!firstChunk) return false;

  const [key] = firstChunk.split("=");
  const loweredKey = key.toLowerCase();

  if (
  LEGITIMATE_PARAM_SET.has(loweredKey) ||
    AUTH0_PARAMS_TO_STRIP.includes(loweredKey)
  ) {
    return true;
  }

  // Generic key=value patterns should be treated as queries
  if (firstChunk.includes("=")) return true;

  // Also allow generic boolean flags (letters, numbers, dashes, underscores)
  if (/^[a-z0-9_-]+$/i.test(firstChunk) && firstChunk.length > 1) {
    return true;
  }

  return false;
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
  // console.log("🐛 parse() called with text:", text);
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
    
    // Always try to decode - decodeKidlispFromUrl will return unchanged if not encoded
    let decodedContent = decodeKidlispFromUrl(promptContent);
    
    // Check if the DECODED content is KidLisp
    if (!isKidlispSource(decodedContent)) {
      // If decoded version isn't KidLisp, use the original (might be a piece name)
      decodedContent = promptContent;
    }
    
    return {
      host: location.hostname + (location.port ? ":" + location.port : ""),
      path: "aesthetic.computer/disks/prompt",
      piece: "prompt",
      colon: undefined,
      params: [decodedContent], // Pass the content as a parameter
      search: undefined,
      hash: undefined,
      text: text,
    };
  }
  
  // 🤖 Early kidlisp detection - check BEFORE space-to-tilde conversion
  // This catches inline kidlisp including RGB strings like "255 0 0"
  const kidlispCheck = isKidlispSource(text);
  const hasSpecialChars = text.includes("§") ||
    text.includes("~") ||
    text.includes("_") ||
    text.includes(",") || // Comma-separated kidlisp syntax
    text.includes("\n") ||
    text.startsWith("(") ||
    text.startsWith(";");
  
  // Also check for kidlisp AFTER converting underscores to spaces (for URL-encoded RGB)
  const textWithSpaces = text.replace(/_/g, " ");
  const isKidlispAfterDecode = isKidlispSource(textWithSpaces);
  
  if ((kidlispCheck && hasSpecialChars) || isKidlispAfterDecode) {
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
  // But preserve painting codes like #k3d (which aren't valid hex colors).
  // Only convert to 0x if it looks like a valid hex color (3 or 6 hex digits).
  text = text.replace(/~#([0-9a-fA-F]{3}(?:[0-9a-fA-F]{3})?)(?=~|$)/g, "~0x$1");
  [text, hash] = text.split("#");

  // 1. Pull off any "search" from `text`, ignoring any question mark
  //    characters that were part of the piece slug.

  if (text[0] === "?") {
    // Special case for empty path with a search param.
    search = text.slice(1);
    text = window?.acSTARTING_PIECE || "prompt";
  } else {
  // Smarter handling: keep literal question marks in piece slugs (e.g. line:5~?~?~?)
    let questionIndex = text.indexOf("?");
    while (questionIndex >= 0) {
      if (shouldTreatAsQuery(text, questionIndex)) {
        search = text.slice(questionIndex + 1);
        text = text.slice(0, questionIndex);
        break;
      }
      questionIndex = text.indexOf("?", questionIndex + 1);
    }
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

// List of legitimate query parameters that should be preserved
// Get clean path without legitimate query parameters
function getCleanPath(fullUrl) {
  // console.log("🐛 getCleanPath() input:", fullUrl);
  let cleanPath = fullUrl;
  
  // Remove legitimate parameters from the end
  for (const paramName of LEGITIMATE_PARAMS) {
    const regexWithValue = new RegExp(`[?&]${paramName}=([^&]*)`, 'g');
    const regexBool = new RegExp(`[?&]${paramName}(?=[&]|$)`, 'g');
    const beforeClean = cleanPath;
    cleanPath = cleanPath.replace(regexWithValue, '');
    cleanPath = cleanPath.replace(regexBool, '');
    if (beforeClean !== cleanPath) {
      // console.log(`🐛 getCleanPath() removed '${paramName}':`, beforeClean, "->", cleanPath);
    }
  }
  
  // Remove Auth0 parameters from the end (these should always be stripped)
  // Only remove 'code' if 'state' is also present (indicating Auth0 callback)
  const hasState = /[?&]state=/.test(cleanPath);
  const auth0ParamsToRemove = hasState 
    ? AUTH0_PARAMS_TO_STRIP 
    : AUTH0_PARAMS_TO_STRIP.filter(param => param !== 'code');
    
  for (const paramName of auth0ParamsToRemove) {
    const regexWithValue = new RegExp(`[?&]${paramName}=([^&]*)`, 'g');
    const regexBool = new RegExp(`[?&]${paramName}(?=[&]|$)`, 'g');
    cleanPath = cleanPath.replace(regexWithValue, '');
    cleanPath = cleanPath.replace(regexBool, '');
  }
  
  // Clean up any remaining ? or & sequences
  cleanPath = cleanPath.replace(/[?&]+$/, ''); // Remove trailing ? or &
  cleanPath = cleanPath.replace(/\?&+/, '?'); // Fix ?& sequences
  cleanPath = cleanPath.replace(/&+/g, '&'); // Collapse multiple &
  cleanPath = cleanPath.replace(/\?$/, ''); // Remove trailing ?

  // Only cut off ? and & if they look like query parameters
  // Don't cut them off if this is KidLisp code (which can contain ? as a special character)
  const questionIdx = cleanPath.indexOf('?');
  const ampIdx = cleanPath.indexOf('&');
  const cutoffCandidates = [questionIdx, ampIdx].filter((idx) => idx >= 0);
  
  if (cutoffCandidates.length > 0) {
    const cutoff = Math.min(...cutoffCandidates);
    const pathBeforeCutoff = cleanPath.slice(0, cutoff);
    const remainingPart = cleanPath.slice(cutoff);
    
    // Check if the remaining part looks like a query string with legitimate parameters
    // If it doesn't match any known patterns, it's likely KidLisp code and should be kept
    let looksLikeQuery = false;
    
    // Check if any legitimate or auth0 parameters appear after the ? or &
    for (const paramName of [...LEGITIMATE_PARAMS, ...AUTH0_PARAMS_TO_STRIP]) {
      if (remainingPart.match(new RegExp(`[?&]${paramName}(=|&|$)`))) {
        looksLikeQuery = true;
        break;
      }
    }
    
    // Also check for generic key=value patterns
    if (remainingPart.match(/[?&][a-zA-Z0-9_-]+=/)) {
      looksLikeQuery = true;
    }
    
    // Only cut off if it looks like a query string
    if (looksLikeQuery) {
      cleanPath = pathBeforeCutoff;
    }
    // Otherwise keep the full path including ? or & characters (KidLisp code)
  }
  
  // console.log("🐛 getCleanPath() final result:", cleanPath);
  return cleanPath;
}

// Cleans a url for feeding into `parse` as the text parameter.
function slug(url) {
  //console.log("🐛 slug() input:", url);
  
  // Remove http protocol and host from current url before feeding it to parser.
  let cleanedUrl = url
    .replace(/^http(s?):\/\//i, "")
    .replace(window.location.hostname + ":" + window.location.port + "/", "")
    .replace(window.location.hostname + "/", "")
    .split("#")[0]; // Remove any hash.

  //console.log("🐛 slug() after host removal:", cleanedUrl);

  // Use safe parameter removal instead of .split("?")[0] 
  cleanedUrl = getCleanPath(cleanedUrl);
  
  //console.log("🐛 slug() after getCleanPath:", cleanedUrl);

  // Decode URL-encoded characters first
  cleanedUrl = decodeURIComponent(cleanedUrl);
  
  //console.log("🐛 slug() after decodeURIComponent:", cleanedUrl);

  // Only apply kidlisp URL decoding if this actually looks like kidlisp code
  if (isKidlispSource(cleanedUrl)) {
    //console.log("🐛 slug() detected as KidLisp, decoding...");
    return decodeKidlispFromUrl(cleanedUrl);
  }
  
  //console.log("🐛 slug() final result:", cleanedUrl);
  return cleanedUrl;
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
function metadata(host, slug, pieceMetadata, protocol = "https:", objktContext = null) {
  // Use a default title if there is no override.
  const notAesthetic =
    host.indexOf("sotce") > -1 ||
    host.indexOf("botce") > -1 ||
    host.indexOf("wipppps.world") > -1;
  
  // Check if this is a standalone title from Lisp comment (skip " · Aesthetic Computer")
  const isStandaloneTitle = pieceMetadata?.standaloneTitle === true;
  
  let title;
  
  // Check for PACK mode and generate custom title format
  if (checkPackMode()) {
    try {
      const colophon = (typeof window !== 'undefined' && window.acPACK_COLOPHON) || 
                      (typeof globalThis !== 'undefined' && globalThis.acPACK_COLOPHON);
      
      if (colophon?.piece?.name && colophon?.build?.author) {
        // Extract timestamp starting with 2025 from zipFilename (format: @author-$piece-2025.09.25.04.22.12.938.zip)
        let timestamp = new Date().getFullYear(); // fallback
        if (colophon.build.zipFilename) {
          const timestampMatch = colophon.build.zipFilename.match(/(2025\.\d{2}\.\d{2}\.\d{2}\.\d{2}\.\d{2}\.\d{3})/);
          if (timestampMatch) {
            timestamp = timestampMatch[1];
          }
        } else if (colophon.build.packTime) {
          // Format packTime to match zip filename format
          const date = new Date(colophon.build.packTime);
          timestamp = `${date.getFullYear()}.${String(date.getMonth() + 1).padStart(2, '0')}.${String(date.getDate()).padStart(2, '0')}.${String(date.getHours()).padStart(2, '0')}.${String(date.getMinutes()).padStart(2, '0')}.${String(date.getSeconds()).padStart(2, '0')}.${String(date.getMilliseconds()).padStart(3, '0')}`;
        }
        title = `${colophon.piece.name} by ${colophon.build.author}, ${timestamp}`;
      } else if (objktContext?.author) {
        // Use objktContext if provided (for pack pipeline)  
        const year = new Date().getFullYear();
        title = `${slug} by ${objktContext.author}, ${year}`;
      } else {
        // Fallback for OBJKT mode without colophon
        title = slug;
      }
    } catch (e) {
      // Fallback if there's any issue accessing OBJKT data
      if (objktContext?.author) {
        const year = new Date().getFullYear();
        title = `${slug} by ${objktContext.author}, ${year}`;
      } else {
        title = slug;
      }
    }
  } else if (pieceMetadata?.title) {
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
    // Use Cloudflare Workers grab service for screenshot generation
    ogImage = `https://grab.aesthetic.computer/preview/1200x630/${slug}.png`;
    twitterImage = `https://grab.aesthetic.computer/preview/1800x900/${slug}.png`;
  }

  // Extract just the piece name (before ~) for icon URL
  const pieceName = slug.split('~')[0];
  icon = pieceMetadata?.icon_url || `https://grab.aesthetic.computer/icon/128x128/${pieceName}.png`;

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
