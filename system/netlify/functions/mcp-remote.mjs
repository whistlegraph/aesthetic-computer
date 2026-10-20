// Remote MCP Server, 2026.02.13
// SSE-based MCP server for aesthetic.computer
// Enables Claude Pro/Team users to connect without local installation

import { respond } from "../../backend/http.mjs";

const SERVER_INFO = {
  name: "aesthetic-computer",
  version: "1.1.0",
};

const TOOLS = [
  {
    name: "publish_piece",
    description: "Publish a JavaScript piece to aesthetic.computer",
    inputSchema: {
      type: "object",
      properties: {
        source: {
          type: "string",
          description: "JavaScript piece source code (max 100,000 characters)",
        },
        name: {
          type: "string",
          description: "Optional name for the piece",
        },
      },
      required: ["source"],
    },
  },
  {
    name: "publish_kidlisp",
    description:
      "Publish KidLisp code to aesthetic.computer. KidLisp is a creative coding Lisp dialect for generative art.",
    inputSchema: {
      type: "object",
      properties: {
        source: {
          type: "string",
          description: "KidLisp source code (max 50,000 characters)",
        },
      },
      required: ["source"],
    },
  },
  {
    name: "publish_clock",
    description:
      "Publish a clock melody to aesthetic.computer. Returns a pronounceable short code.",
    inputSchema: {
      type: "object",
      properties: {
        source: {
          type: "string",
          description: "Clock melody string (max 10,000 characters)",
        },
      },
      required: ["source"],
    },
  },
  {
    name: "get_api_info",
    description:
      "Fetch the full API documentation from aesthetic.computer",
    inputSchema: {
      type: "object",
      properties: {},
    },
  },
  {
    name: "preview_kidlisp",
    description:
      "Validate KidLisp source code without publishing. Checks syntax (balanced parens, strings), identifies functions used, and flags common mistakes. Use this before publish_kidlisp to catch errors.",
    inputSchema: {
      type: "object",
      properties: {
        source: {
          type: "string",
          description: "KidLisp source code to validate",
        },
      },
      required: ["source"],
    },
  },
  {
    name: "notepaint_brains",
    description:
      "List the brains (image models) Note Paint can paint with on notepaint.art, each with its price in Braincells and US dollars. When signed in, also returns the painter's Braincell balance and today's free allowance. Read-only; charges nothing.",
    inputSchema: {
      type: "object",
      properties: {
        size: {
          type: "number",
          enum: [256, 512, 1024],
          description: "Painting size in pixels (square). Defaults to 256.",
        },
      },
    },
  },
  {
    name: "notepaint_paint",
    description:
      "Paint with Note Paint (notepaint.art): one move turns a prompt into a square painting, or repaints an earlier painting from this conversation. Each move spends the painter's AC Braincells (today's free allowance first) — call notepaint_brains to show the price before painting. Requires signing in with an AC account that has a handle.",
    inputSchema: {
      type: "object",
      properties: {
        prompt: {
          type: "string",
          description: "What to paint or change, under 200 characters",
        },
        from: {
          type: "string",
          description:
            "Optional id of a painting returned earlier by notepaint_paint, to keep painting on it",
        },
        brain: {
          type: "string",
          description:
            "Optional brain id from notepaint_brains. Defaults to the cheapest available brain.",
        },
        strength: {
          type: "number",
          enum: [0.25, 0.5, 0.75],
          description:
            "How much the move may change the painting. Defaults to 0.75 for a new painting and 0.5 when repainting.",
        },
        size: {
          type: "number",
          enum: [256, 512, 1024],
          description:
            "Painting size for a new painting. Defaults to 256. A repaint keeps its painting's size.",
        },
      },
      required: ["prompt"],
    },
  },
];

const RESOURCES = [
  {
    uri: "aesthetic-computer://piece-template",
    name: "Piece Template",
    description:
      "Starter template for a new aesthetic.computer JavaScript piece",
    mimeType: "text/plain",
  },
  {
    uri: "aesthetic-computer://kidlisp-reference",
    name: "KidLisp Language Reference",
    description:
      "Complete reference for KidLisp: 118 functions across 12 categories for generative art",
    mimeType: "text/plain",
  },
  {
    uri: "aesthetic-computer://piece-examples",
    name: "KidLisp Top Hit Examples",
    description:
      "Real KidLisp pieces from aesthetic.computer with 500+ views — proven patterns",
    mimeType: "text/plain",
  },
];

const PROMPTS = [
  {
    name: "create-piece",
    description: "Guided prompt for creating an aesthetic.computer JavaScript piece",
    arguments: [
      { name: "name", description: "Name of the piece", required: true },
      {
        name: "description",
        description: "What the piece should do",
        required: true,
      },
    ],
  },
  {
    name: "create-kidlisp",
    description:
      "Guide for creating a KidLisp piece — generative art on aesthetic.computer",
    arguments: [
      {
        name: "description",
        description: "What the piece should do or look like",
        required: true,
      },
    ],
  },
];

// ── KidLisp validator (shared logic with stdio server) ──

function validateKidLisp(source) {
  const errors = [];
  const warnings = [];
  const functionsUsed = new Set();
  let expressionCount = 0;
  let hasAnimation = false;
  let hasInteraction = false;
  let hasWipe = false;

  if (!source.trim()) {
    errors.push("Source code is empty");
    return {
      valid: false,
      errors,
      warnings,
      stats: {
        expressions: 0,
        functions_used: [],
        has_animation: false,
        has_interaction: false,
        has_wipe: false,
      },
    };
  }

  let depth = 0;
  let inString = false;
  let stringChar = "";
  let inComment = false;

  for (let i = 0; i < source.length; i++) {
    const ch = source[i];
    if (inComment) {
      if (ch === "\n") inComment = false;
      continue;
    }
    if (inString) {
      if (ch === stringChar && source[i - 1] !== "\\") inString = false;
      continue;
    }
    if (ch === ";") {
      inComment = true;
      continue;
    }
    if (ch === '"' || ch === "'") {
      inString = true;
      stringChar = ch;
      continue;
    }
    if (ch === "(") {
      depth++;
      expressionCount++;
    } else if (ch === ")") {
      depth--;
      if (depth < 0) errors.push(`Unmatched closing parenthesis at position ${i}`);
    }
  }

  if (depth > 0)
    errors.push(
      `${depth} unclosed parenthes${depth === 1 ? "is" : "es"} — missing closing )`,
    );
  if (inString) errors.push("Unterminated string literal");

  const funcPattern = /\(\s*([a-zA-Z_+\-*/%?][a-zA-Z0-9_]*)/g;
  let match;
  while ((match = funcPattern.exec(source)) !== null) {
    const fname = match[1];
    functionsUsed.add(fname);
    if (fname === "wipe") hasWipe = true;
    if (fname === "tap" || fname === "draw") hasInteraction = true;
  }

  const firstLine = source.trim().split("\n")[0].trim();
  const bareColors = [
    "black", "white", "red", "blue", "green", "purple", "navy", "brown",
    "salmon", "beige", "cyan",
  ];
  if (bareColors.some((c) => firstLine.startsWith(c))) hasWipe = true;

  if (/\d+\.?\d*s[.!]?/.test(source)) hasAnimation = true;
  if (
    functionsUsed.has("wiggle") ||
    functionsUsed.has("spin") ||
    functionsUsed.has("scroll") ||
    functionsUsed.has("zoom")
  )
    hasAnimation = true;

  if (!hasWipe)
    warnings.push(
      "No background color found — consider starting with (wipe color) or a bare color name",
    );

  if (/\(def\s+[a-zA-Z][a-zA-Z0-9]*-[a-zA-Z]/.test(source))
    errors.push(
      "Identifier contains a dash (-) which is parsed as subtraction. Use underscores: my_var not my-var",
    );

  return {
    valid: errors.length === 0,
    errors,
    warnings,
    stats: {
      expressions: expressionCount,
      functions_used: [...functionsUsed].sort(),
      has_animation: hasAnimation,
      has_interaction: hasInteraction,
      has_wipe: hasWipe,
    },
  };
}

// ── Handle MCP protocol messages ──

async function handleMCPMessage(message, authToken, notePaintOnly = false) {
  const { method, params } = message;

  // notepaint.art/mcp is Note Paint's own connector: its tools only, and an
  // AC sign-in on every request (see handler).
  if (notePaintOnly) {
    switch (method) {
      case "initialize":
        return {
          protocolVersion: "2024-11-05",
          capabilities: { tools: {} },
          serverInfo: NOTEPAINT_SERVER_INFO,
        };
      case "tools/list":
        return { tools: NOTEPAINT_TOOLS };
      case "tools/call":
        if (!NOTEPAINT_TOOLS.some((tool) => tool.name === params?.name))
          return {
            content: [{ type: "text", text: `Error: Unknown tool: ${params?.name}` }],
            isError: true,
          };
        return await handleToolCall(params, authToken);
      default:
        throw new Error(`Unknown method: ${method}`);
    }
  }

  switch (method) {
    case "initialize":
      return {
        protocolVersion: "2024-11-05",
        capabilities: {
          tools: {},
          resources: {},
          prompts: {},
        },
        serverInfo: SERVER_INFO,
      };

    case "tools/list":
      return { tools: TOOLS };

    case "tools/call":
      return await handleToolCall(params, authToken);

    case "resources/list":
      return { resources: RESOURCES };

    case "resources/read":
      return await handleResourceRead(params);

    case "prompts/list":
      return { prompts: PROMPTS };

    case "prompts/get":
      return await handlePromptGet(params);

    default:
      throw new Error(`Unknown method: ${method}`);
  }
}

// ── Tool handlers ──

async function handleToolCall(params, authToken) {
  const { name, arguments: args } = params;

  try {
    switch (name) {
      case "publish_piece":
        return await callPublishPiece(args, authToken);
      case "publish_kidlisp":
        return await callPublishKidLisp(args, authToken);
      case "publish_clock":
        return await callPublishClock(args, authToken);
      case "get_api_info":
        return await callGetApiInfo();
      case "preview_kidlisp":
        return callPreviewKidLisp(args);
      case "notepaint_brains":
        return await callNotePaintBrains(args, authToken);
      case "notepaint_paint":
        return await callNotePaintPaint(args, authToken);
      default:
        throw new Error(`Unknown tool: ${name}`);
    }
  } catch (error) {
    return {
      content: [{ type: "text", text: `Error: ${error.message}` }],
      isError: true,
    };
  }
}

async function callPublishPiece(args, authToken) {
  const headers = { "Content-Type": "application/json" };
  if (authToken) headers["Authorization"] = `Bearer ${authToken}`;

  const response = await fetch("https://aesthetic.computer/api/store-piece", {
    method: "POST",
    headers,
    body: JSON.stringify(args),
  });

  const result = await response.json();

  return {
    content: [
      {
        type: "text",
        text: JSON.stringify(
          {
            code: result.code,
            url: result.url,
            cached: result.cached,
          },
          null,
          2,
        ),
      },
    ],
  };
}

async function callPublishKidLisp(args, authToken) {
  const headers = { "Content-Type": "application/json" };
  if (authToken) headers["Authorization"] = `Bearer ${authToken}`;

  const response = await fetch(
    "https://aesthetic.computer/api/store-kidlisp",
    { method: "POST", headers, body: JSON.stringify(args) },
  );

  const result = await response.json();

  return {
    content: [
      {
        type: "text",
        text: JSON.stringify(
          {
            code: result.code,
            url: `https://aesthetic.computer/${result.code}`,
            cached: result.cached,
          },
          null,
          2,
        ),
      },
    ],
  };
}

async function callPublishClock(args, authToken) {
  const headers = { "Content-Type": "application/json" };
  if (authToken) headers["Authorization"] = `Bearer ${authToken}`;

  const response = await fetch("https://aesthetic.computer/api/store-clock", {
    method: "POST",
    headers,
    body: JSON.stringify(args),
  });

  const result = await response.json();

  return {
    content: [
      {
        type: "text",
        text: JSON.stringify(
          {
            code: result.code,
            url: `https://aesthetic.computer/clock~${result.code}`,
            cached: result.cached,
          },
          null,
          2,
        ),
      },
    ],
  };
}

async function callGetApiInfo() {
  const response = await fetch("https://aesthetic.computer/api", {
    headers: { Accept: "application/json" },
  });

  const docs = await response.json();

  return {
    content: [{ type: "text", text: JSON.stringify(docs, null, 2) }],
  };
}

function callPreviewKidLisp(args) {
  const result = validateKidLisp(args.source || "");
  return {
    content: [{ type: "text", text: JSON.stringify(result, null, 2) }],
  };
}

// ── Note Paint (notepaint.art) ──
// The tools call the same endpoints as the studio, so pricing, the free
// allowance, billing and refunds stay in nopaint-inference.mjs.

const NOTEPAINT_API = "https://aesthetic.computer/api";

const NOTEPAINT_HOSTS = new Set(["notepaint.art", "www.notepaint.art"]);
const NOTEPAINT_SERVER_INFO = { name: "notepaint", version: "1.0.0" };
const NOTEPAINT_TOOLS = TOOLS.filter((tool) => tool.name.startsWith("notepaint_"));

// OAuth 2.0 Protected Resource Metadata (RFC 9728), the discovery step of
// MCP authorization. aesthetic.computer is the authorization server for the
// whole network (backend/ac-oauth.mjs): its link page signs painters in with
// AC, and the act_ token it issues is good for Note Paint alone.
const AC_AUTHORIZATION_SERVER = "https://aesthetic.computer";

function notePaintResourceMetadata() {
  return {
    resource: "https://notepaint.art/mcp",
    authorization_servers: [AC_AUTHORIZATION_SERVER],
    scopes_supported: ["openid", "profile", "email", "offline_access"],
    bearer_methods_supported: ["header"],
    resource_name: "Note Paint",
    resource_documentation: "https://notepaint.art",
  };
}

// Paintings from this conversation, so a repaint can start from the last one
// without the client sending the PNG back. Keyed by a hash of the caller's
// token; held in memory for an hour.
const notePaintings = new Map();
const NOTEPAINT_KEEP_MS = 60 * 60 * 1000;

async function notePaintOwner(authToken) {
  const { createHash } = await import("node:crypto");
  return createHash("sha256").update(authToken).digest("hex").slice(0, 16);
}

function keepNotePainting(owner, painting) {
  const now = Date.now();
  for (const [key, value] of notePaintings)
    if (value.until <= now) notePaintings.delete(key);
  if (notePaintings.size >= 64)
    notePaintings.delete(notePaintings.keys().next().value);
  const id = crypto.randomUUID().slice(0, 8);
  notePaintings.set(`${owner}:${id}`, {
    ...painting,
    until: now + NOTEPAINT_KEEP_MS,
  });
  return id;
}

async function notePaintRequest(path, authToken, body, timeout = 20000) {
  const headers = { "Content-Type": "application/json" };
  if (authToken) headers["Authorization"] = `Bearer ${authToken}`;
  const response = await fetch(`${NOTEPAINT_API}/${path}`, {
    method: body ? "POST" : "GET",
    headers,
    ...(body ? { body: JSON.stringify(body) } : {}),
    signal: AbortSignal.timeout(timeout),
  });
  const result = await response.json().catch(() => ({}));
  if (!response.ok)
    throw new Error(result.error || `Note Paint request failed (${response.status})`);
  return result;
}

function notePaintOffers(catalog, size) {
  return (catalog.models || [])
    .filter((model) => model.available && model.size?.[0] === size)
    .sort((a, b) => a.braincells - b.braincells);
}

function notePaintUSD(catalog, braincells) {
  return Number((braincells * catalog.creditValueUSD).toFixed(4));
}

async function notePaintWallet(authToken) {
  if (!authToken) return null;
  try {
    const wallet = await notePaintRequest("nopaint-wallet", authToken);
    return {
      handle: wallet.handle,
      free_remaining_today: wallet.remaining,
      free_limit_today: wallet.limit,
      purchased: wallet.purchased,
    };
  } catch (error) {
    return { error: error.message };
  }
}

async function callNotePaintBrains(args, authToken) {
  const size = args?.size || 256;
  const [catalog, wallet] = await Promise.all([
    notePaintRequest("nopaint-inference"),
    notePaintWallet(authToken),
  ]);
  const brains = notePaintOffers(catalog, size).map((model) => ({
    id: model.id,
    name: model.name,
    braincells: model.braincells,
    usd: notePaintUSD(catalog, model.braincells),
    ...(model.timing ? { seconds: model.timing.averageSeconds } : {}),
  }));
  return {
    content: [
      {
        type: "text",
        text: JSON.stringify(
          {
            size,
            brains,
            ...(wallet ? { wallet } : { signed_in: false }),
            ...(catalog.service?.available === false
              ? { unavailable: catalog.service.detail }
              : {}),
          },
          null,
          2,
        ),
      },
    ],
  };
}

async function blankNotePainting(size) {
  const { default: sharp } = await import("sharp");
  const png = await sharp({
    create: { width: size, height: size, channels: 3, background: "#ffffff" },
  })
    .png()
    .toBuffer();
  return png.toString("base64");
}

async function callNotePaintPaint(args, authToken) {
  if (!authToken) throw new Error("Sign in with AC to paint.");
  const prompt = (args.prompt || "").replace(/\s+/g, " ").trim();
  if (!prompt || prompt.length > 200)
    throw new Error("The prompt must be text under 200 characters.");

  const owner = await notePaintOwner(authToken);
  let base = null;
  if (args.from) {
    base = notePaintings.get(`${owner}:${args.from}`);
    if (!base || base.until <= Date.now())
      throw new Error(`No painting ${args.from} from the last hour. Paint a new one.`);
  }
  const size = base?.size || args.size || 256;
  const strength = args.strength ?? (base ? 0.5 : 0.75);

  const catalog = await notePaintRequest("nopaint-inference");
  const offers = notePaintOffers(catalog, size);
  const offer = args.brain
    ? offers.find((model) => model.id === args.brain || model.id === `${args.brain}@${size}`)
    : offers[0];
  if (!offer) throw new Error(`No available brain ${args.brain || ""} paints at ${size}px.`);

  const result = await notePaintRequest(
    "nopaint-inference",
    authToken,
    {
      requestId: crypto.randomUUID(),
      seed: Math.floor(Math.random() * 2 ** 31),
      strength,
      quote: offer.quote,
      model: offer.id,
      image: base?.image || (await blankNotePainting(size)),
      hint: prompt,
    },
    180000,
  );

  const id = keepNotePainting(owner, { image: result.image, size });
  return {
    content: [
      { type: "image", data: result.image, mimeType: "image/png" },
      {
        type: "text",
        text: JSON.stringify(
          {
            id,
            brain: offer.name,
            size,
            braincells: result.billing?.braincells,
            free: result.billing?.free,
            paid: result.billing?.paid,
            usd: notePaintUSD(catalog, result.billing?.braincells || 0),
            painter: result.handle,
            next: `Pass from: "${id}" to keep painting on it. Studio: https://notepaint.art`,
          },
          null,
          2,
        ),
      },
    ],
  };
}

// ── Resource handlers ──

async function handleResourceRead(params) {
  const { uri } = params;

  if (uri === "aesthetic-computer://piece-template") {
    return {
      contents: [
        {
          uri,
          mimeType: "text/plain",
          text: `// aesthetic.computer piece template

export function boot($) {
  // Called once when the piece loads
  $.wipe("blue");
}

export function paint($) {
  // Called every frame (60fps)
  $.ink("white");
  $.circle($.screen.width / 2, $.screen.height / 2, 50);
}

export function act($) {
  // Handle user input events
  if ($.event.is("touch")) {
    console.log("Touch at:", $.event.x, $.event.y);
  }
}

export function sim($) {
  // Update state / physics
}`,
        },
      ],
    };
  }

  if (uri === "aesthetic-computer://kidlisp-reference") {
    return {
      contents: [
        {
          uri,
          mimeType: "text/plain",
          text: KIDLISP_REFERENCE,
        },
      ],
    };
  }

  if (uri === "aesthetic-computer://piece-examples") {
    return {
      contents: [
        {
          uri,
          mimeType: "text/plain",
          text: PIECE_EXAMPLES,
        },
      ],
    };
  }

  throw new Error(`Unknown resource: ${uri}`);
}

// ── Prompt handlers ──

async function handlePromptGet(params) {
  const { name, arguments: args } = params;

  if (name === "create-piece") {
    return {
      description: "Create an aesthetic.computer JavaScript piece",
      messages: [
        {
          role: "user",
          content: {
            type: "text",
            text: `Create a new aesthetic.computer piece called "${args.name}" that ${args.description}. Use the aesthetic-computer://piece-template resource as a starting point. After writing the piece, publish it using the publish_piece tool.`,
          },
        },
      ],
    };
  }

  if (name === "create-kidlisp") {
    return {
      description: "Create a KidLisp piece for aesthetic.computer",
      messages: [
        {
          role: "user",
          content: {
            type: "text",
            text: `Create a KidLisp piece for aesthetic.computer.

Description: ${args.description}

Read the aesthetic-computer://kidlisp-reference resource for the full language reference, and aesthetic-computer://piece-examples for real top-hit examples with 500+ views.

Key tips:
- Start with a background color (bare word like \`black\` or \`(wipe color)\`)
- Use \`(ink color)\` before drawing, \`(? a b c)\` for random, \`(... a b c)\` for cycling
- Combine transforms (scroll + zoom + blur) for feedback loops
- Use timing expressions: \`0.1s\`, \`1s...\`, \`2s!\`
- Keep it minimal — many top hits are under 5 lines

After writing the piece, publish it using the publish_kidlisp tool.`,
          },
        },
      ],
    };
  }

  throw new Error(`Unknown prompt: ${name}`);
}

// ── Inline content (matches stdio server) ──

const KIDLISP_REFERENCE = `# KidLisp Language Reference

KidLisp is a minimal Lisp dialect for creating generative art and interactive experiences on aesthetic.computer.
It uses S-expressions where the first element is a function name followed by arguments.

## Syntax Basics

\`\`\`lisp
; Comments start with semicolon
(function-name arg1 arg2 ...)

; Shorthand comma syntax (no parens needed for simple commands)
purple, ink, line, blur 5

; Variables & Functions
(def name value)
(later name param1 param2 body...)
\`\`\`

Shorthand: \`w\` = width, \`h\` = height, \`f\` = frame. \`?\` = random, \`...\` = cycle.
Identifiers: no dashes (parsed as subtraction). Use \`my_var\` not \`my-var\`.

## Drawing

| Function | Usage | Description |
|----------|-------|-------------|
| \`wipe\` | \`(wipe color)\` | Clear screen |
| \`ink\` | \`(ink color [alpha])\` | Set draw color |
| \`line\` | \`(line x1 y1 x2 y2)\` or \`(line)\` | Line (random if no args) |
| \`box\` | \`(box x y w h)\` | Rectangle |
| \`circle\` | \`(circle x y radius)\` | Circle |
| \`tri\` | \`(tri x1 y1 x2 y2 x3 y3)\` | Triangle |
| \`plot\` | \`(plot x y)\` | Single pixel |
| \`flood\` | \`(flood x y)\` | Flood fill |
| \`fill\`/\`outline\` | \`(fill)\` / \`(outline)\` | Toggle shape mode |

## Colors

- Named: \`red\`, \`blue\`, \`lime\`, \`navy\`, etc. (all CSS colors, no quotes needed)
- RGB: \`(ink 255 0 0)\` — With alpha: \`(ink red 128)\`
- Special: \`rainbow\`, \`zebra\`, \`erase\`
- Gradients: \`fade:red-blue-black\`, \`fade:zebra-rainbow-zebra:direction\`

## Math

\`+\`, \`-\`, \`*\`, \`/\`, \`%\`, \`sin\`, \`cos\`, \`random\`, \`wiggle\`, \`min\`, \`max\`, \`abs\`, \`sqrt\`, \`pow\`, \`floor\`, \`ceil\`, \`round\`

## System: \`width\`/\`w\`, \`height\`/\`h\`, \`frame\`/\`f\`, \`clock\`, \`pi\`

## Control Flow

| Function | Usage | Description |
|----------|-------|-------------|
| \`def\` | \`(def x 10)\` | Variable |
| \`later\` | \`(later fn params body)\` | Function |
| \`if\` | \`(if cond then else)\` | Conditional |
| \`once\` | \`(once expr)\` | Run once |
| \`repeat\`/\`bunch\` | \`(repeat n i expr)\` | Loop |
| \`?\`/\`choose\` | \`(? a b c)\` | Random pick |
| \`...\` | \`(... a b c)\` | Cycle over time |

## Timing

- \`1s\` — after 1 second
- \`2s...\` — cycle every 2 seconds
- \`0.5s!\` — once after 0.5s
- \`0.1s\` — every 0.1 seconds

## Transforms

| Function | Usage |
|----------|-------|
| \`scroll\` | \`(scroll dx dy)\` — translate with wrapping |
| \`zoom\` | \`(zoom factor)\` — scale from center |
| \`spin\` | \`(spin angle)\` — rotate |
| \`suck\` | \`(suck strength)\` — vortex |
| \`blur\` | \`(blur amount)\` — gaussian blur |
| \`contrast\` | \`(contrast amount)\` |
| \`sort\` | \`(sort)\` — sort pixels |
| \`bake\` | \`(bake)\` — commit to background |

## Media & Text

\`paste\`, \`stamp\`, \`write\`, \`tape\`, \`painting\`, \`steal\`, \`putback\`

## Audio

\`mic\`, \`amplitude\`, \`melody\`, \`overtone\`, \`noise\`, \`speaker\`

## 3D

\`cube\`, \`form\`, \`trans\`, \`cubespin\`, \`cubepos\`, \`cubescale\`, \`camrot\`, \`camspin\`

## Embedding

\`($codeId)\` or \`($codeId x y w h alpha)\` — embed another piece
\`(jump "piece")\` or \`(jump $id)\` — navigate

## Best Practices

1. Start with a background — bare color or \`(wipe color)\`
2. Use \`?\` for randomness, \`...\` for cycling
3. Combine transforms for feedback loops (scroll + zoom + blur)
4. Use \`once\` for setup, \`bake\` for layering
5. Keep it minimal — many top hits are under 5 lines
`;

const PIECE_EXAMPLES = `# KidLisp Top Hit Examples

Real pieces from aesthetic.computer with 500+ views.

## "bop" — 7,388 views
purple, ink, line, blur 5

Pattern: Bare color background, random ink, random line, blur. Builds dreamy lines.

## "pie" — 4,702 views
(fps 24)
(0.25s (wipe (... red yellow blue)))
(ink green)
(line 0 height/2 width height/2)
(ink red)
(line width/2 0 width/2 height)
(scroll frame frame)

Pattern: ... cycles colors. scroll frame frame = diagonal motion.

## "roz" — 3,251 views (@jeffrey)
fade:red-blue-black-blue-red
ink (? rainbow white 0) (1s... 24 64)
line w/2 0 w/2 h
(spin (2s... -1.125 1.125)) (zoom 1.1)
(0.5s (contrast 1.05))
(scroll (? -0.1 0 0.1) (? -0.1 0 0.1))
ink (? cyan yellow magenta) 8
circle w/2 h/2 (? 2 4 8)

Pattern: Fade gradient, cycling spin, random ink alpha, center circle. Transforms accumulate.

## "ceo" — 3,087 views
(1s (coat fade:black-red-rainbow-red-black:frame 64)) (0.3s (zoom 0.5)) (scroll 1)

Pattern: coat overlay with animated fade. Periodic zoom-out = tunnel. Scroll = drift.

## "39i" — 2,049 views (@jeffrey)
black
(0.1s (ink (? black white) 32) (circle ? ? 32))
(ink (? white black gray) (? 32 96))
flood ? ?
scroll (2s... 1 0 -1) (3s... -1 0 1)
(1.5s (zoom (? 0.5 4)))
(1s (blur 0.5))
(2s (contrast 2))
(0.3s (ink (? white black)) (repeat 30 point))
(1s... (spin (0.5s... -0.1 0.1))

Pattern: Random circles + floods + aggressive transforms. Multi-speed timers.

## "cow" — 2,320 views (@jeffrey)
($39i 0 0 w h 128)
($r2f 0 0 w h 128)
(contrast 1.5)

Pattern: Embedded composition — layers two pieces with alpha transparency.

## "4bb" — 1,611 views
black, ink (? yellow black) 48, line, scroll 1
bake, ink (? magenta erase) 64, line, scroll -1
bake, ink (? lime erase) 16, line, scroll 0 1
bake, ink (? cyan erase) 48, line, scroll 0 -1
burn, blur 8, contrast 1.25

Pattern: Multi-layer bake. Four scrolling directions. erase ink = transparency holes.

## "reeb" — 1,337 views
red, ink erase, line, bake

Pattern: Red surface eroded by transparent random lines. Minimal.

## "otoc" — 1,181 views
(once noise) (spin (0.5s... 1 -1)) (suck 1) (contrast 1.01)

Pattern: Noise + spin + suck vortex + slow contrast. Minimal code, maximum effect.

## "inz" — 1,293 views (@jeffrey)
(beige)
(ink (0.25s... 127 0 rainbow))
(write X 3 3)
(scroll 18 3)
(blur 0.1)
(1.25s (zoom (? 0.25 1.5)))

Pattern: Text animation with scroll/blur/zoom jumps.

## Common Patterns

1. Feedback loops — scroll + zoom + blur = self-evolving trails
2. Weighted randomness — (? value value value rare) makes common values likely
3. Multi-speed timers — different intervals for layered rhythm
4. Bare word shortcuts — black, line, ink work without parens
5. erase ink — creates transparency holes
6. Embedded composition — $codeId layers pieces with alpha
7. Fade gradients — fade:color1-color2-color3
8. Minimal code — many top hits are under 5 lines
`;

// ── Main handler ──

export async function handler(event, context) {
  console.log(`📡 Remote MCP request: ${event.httpMethod} ${event.path}`);

  const corsHeaders = {
    "Access-Control-Allow-Origin": "*",
    "Access-Control-Allow-Methods": "GET, POST, OPTIONS",
    "Access-Control-Allow-Headers": "Content-Type, Authorization",
    "Access-Control-Expose-Headers": "WWW-Authenticate",
  };

  if (event.httpMethod === "OPTIONS") {
    return { statusCode: 204, headers: corsHeaders, body: "" };
  }

  if (event.path?.startsWith("/.well-known/oauth-protected-resource")) {
    const host = (event.headers.host || "").toLowerCase().split(":")[0];
    if (!NOTEPAINT_HOSTS.has(host))
      return respond(404, { error: "Not found" }, corsHeaders);
    return respond(200, notePaintResourceMetadata(), {
      ...corsHeaders,
      "Cache-Control": "public, max-age=3600",
    });
  }

  try {
    const authToken = event.headers.authorization?.replace("Bearer ", "");
    const host = (event.headers.host || "").toLowerCase().split(":")[0];
    const notePaintOnly = NOTEPAINT_HOSTS.has(host);

    // An unauthenticated request starts the client's OAuth flow; an expired
    // link token tells it to refresh.
    if (notePaintOnly) {
      const challenge = (error) => respond(401, { error: "Sign in with AC to paint." }, {
        ...corsHeaders,
        "WWW-Authenticate":
          `Bearer resource_metadata="https://notepaint.art/.well-known/oauth-protected-resource", scope="openid profile email offline_access"${error ? `, error="${error}"` : ""}`,
      });
      if (!authToken) return challenge();
      if (authToken.startsWith("act_")) {
        const { acOAuth } = await import("../../backend/ac-oauth.mjs");
        const linked = await (await acOAuth()).resolve(authToken);
        if (!linked || linked.resource !== "https://notepaint.art/mcp") return challenge("invalid_token");
      }
    }

    if (event.httpMethod === "GET") {
      const headers = {
        ...corsHeaders,
        "Content-Type": "text/event-stream",
        "Cache-Control": "no-cache",
        Connection: "keep-alive",
      };

      const initMessage = JSON.stringify({
        jsonrpc: "2.0",
        method: "server/info",
        params: SERVER_INFO,
      });

      return { statusCode: 200, headers, body: `data: ${initMessage}\n\n` };
    }

    if (event.httpMethod === "POST") {
      const message = JSON.parse(event.body || "{}");
      console.log(`🔧 MCP method: ${message.method}`);

      // Initialization is a one-way notification, not a method call.
      if (message.method === "notifications/initialized") {
        if (message.jsonrpc !== "2.0" || Object.hasOwn(message, "id") ||
            (message.params !== undefined && (!message.params ||
              typeof message.params !== "object" || Array.isArray(message.params)))) {
          return respond(400, { error: "Invalid initialized notification" }, corsHeaders);
        }
        return { statusCode: 202, headers: corsHeaders, body: "" };
      }

      const result = await handleMCPMessage(message, authToken, notePaintOnly);
      const response = { jsonrpc: "2.0", id: message.id, result };

      return respond(200, response, corsHeaders);
    }

    return respond(405, { error: "Method not allowed" }, corsHeaders);
  } catch (error) {
    console.error("❌ Remote MCP error:", error);
    return respond(
      500,
      { jsonrpc: "2.0", error: { code: -32603, message: error.message } },
      corsHeaders,
    );
  }
}
