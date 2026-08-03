// API Documentation
// Returns LLM-friendly documentation of public aesthetic.computer APIs

import { respond } from "../../backend/http.mjs";

export async function handler(event, context) {
  const apiDocs = {
    title: "aesthetic.computer Public API",
    version: "1.1.0",
    updated: "2026-08-03",
    description: "Supported public APIs for publishing creative work and reading public Aesthetic Computer data",
    baseURL: "https://aesthetic.computer",

    mcp: {
      title: "MCP Server",
      description: "Model Context Protocol server for compatible assistants to create, validate, and publish Aesthetic Computer pieces",
      package: "@aesthetic.computer/mcp",
      install: "npx @aesthetic.computer/mcp",
      repository: "https://tangled.org/aesthetic.computer/core/tree/main/mcp-server",

      tools: [
        {
          name: "publish_piece",
          description: "Publish a JavaScript piece to aesthetic.computer",
          input: { source: "string", name: "string (optional)" },
          output: { code: "string", url: "string", cached: "boolean" }
        },
        {
          name: "publish_kidlisp",
          description: "Publish KidLisp code to aesthetic.computer",
          input: { source: "string" },
          output: { code: "string", url: "string", cached: "boolean" }
        },
        {
          name: "publish_clock",
          description: "Publish a clock melody to aesthetic.computer",
          input: { source: "string" },
          output: { code: "string", url: "string", cached: "boolean" }
        },
        {
          name: "get_api_info",
          description: "Fetch the full API documentation",
          input: {},
          output: "API documentation object"
        },
        {
          name: "preview_kidlisp",
          description: "Validate KidLisp syntax without publishing",
          input: { source: "string" },
          output: { valid: "boolean", errors: "string[]", warnings: "string[]", stats: "object" }
        }
      ],

      resources: [
        {
          uri: "aesthetic-computer://piece-template",
          description: "Starter template for a new aesthetic.computer piece with all lifecycle functions"
        },
        {
          uri: "aesthetic-computer://kidlisp-reference",
          description: "Quick reference guide for KidLisp syntax and common functions"
        },
        {
          uri: "aesthetic-computer://piece-examples",
          description: "Examples drawn from popular published pieces"
        }
      ],

      prompts: [
        {
          name: "create-piece",
          description: "Guided prompt for creating an aesthetic.computer piece",
          arguments: ["name (required)", "description (required)"]
        },
        {
          name: "create-kidlisp",
          description: "Guided prompt for creating a KidLisp piece",
          arguments: ["description (required)"]
        }
      ],

      configuration: {
        "Claude Desktop": `{
  "mcpServers": {
    "aesthetic-computer": {
      "command": "npx",
      "args": ["-y", "@aesthetic.computer/mcp"],
      "env": {
        "AC_TOKEN": "optional-bearer-token"
      }
    }
  }
}`,
        "Claude Code": `{
  "mcpServers": {
    "aesthetic-computer": {
      "command": "npx",
      "args": ["-y", "@aesthetic.computer/mcp"]
    }
  }
}`,
        "Cursor": `{
  "mcpServers": {
    "aesthetic-computer": {
      "command": "npx",
      "args": ["-y", "@aesthetic.computer/mcp"]
    }
  }
}`
      }
    },

    endpoints: [
      {
        name: "Store KidLisp Code",
        method: "POST",
        path: "/api/store-kidlisp",
        description: "Publish KidLisp code anonymously and get a short URL for sharing",
        authentication: "Optional (Bearer token for authenticated users)",
        requestBody: {
          contentType: "application/json",
          schema: {
            source: {
              type: "string",
              required: true,
              description: "KidLisp source code (max 50,000 characters)",
              example: "(wipe blue)\n(ink red)\n(box 10 10 50 50)"
            }
          }
        },
        responseBody: {
          schema: {
            code: {
              type: "string",
              description: "Short code for accessing the piece (e.g. 'abc123')"
            },
            cached: {
              type: "boolean",
              description: "True if code already existed (deduplication)"
            }
          }
        },
        examples: [
          {
            title: "Publish a KidLisp Piece",
            description: "Create a simple animated piece with KidLisp",
            curl: `curl -X POST https://aesthetic.computer/api/store-kidlisp \\
  -H "Content-Type: application/json" \\
  -d '{
    "source": "(wipe blue)\\n(ink yellow)\\n(circle (/ w 2) (/ h 2) 100)"
  }'`,
            javascript: `const response = await fetch("https://aesthetic.computer/api/store-kidlisp", {
  method: "POST",
  headers: { "Content-Type": "application/json" },
  body: JSON.stringify({
    source: "(wipe blue)\\n(ink yellow)\\n(circle (/ w 2) (/ h 2) 100)"
  })
});

const { code, cached } = await response.json();
console.log(\`View at: https://aesthetic.computer/\${code}\`);`,
            python: `import requests

response = requests.post(
    "https://aesthetic.computer/api/store-kidlisp",
    json={
        "source": "(wipe blue)\\n(ink yellow)\\n(circle (/ w 2) (/ h 2) 100)"
    }
)

data = response.json()
print(f"View at: https://aesthetic.computer/{data['code']}")`,
            response: {
              status: 201,
              body: {
                code: "xyz789",
                cached: false
              }
            }
          }
        ]
      },

      {
        name: "Store Clock Melody",
        method: "POST",
        path: "/api/store-clock",
        description: "Publish a clock melody string and get a pronounceable short code",
        authentication: "Optional (Bearer token for authenticated users)",
        requestBody: {
          contentType: "application/json",
          schema: {
            source: {
              type: "string",
              required: true,
              description: "Clock melody string (max 10,000 characters)",
              example: "c4 d4 e4 f4 g4"
            },
            melody: {
              type: "string",
              required: false,
              description: "Legacy field name (use 'source' instead)"
            }
          }
        },
        responseBody: {
          schema: {
            code: {
              type: "string",
              description: "Pronounceable short code (e.g. 'bako', 'milu')"
            },
            cached: {
              type: "boolean",
              description: "True if melody already existed (deduplication)"
            }
          }
        },
        examples: [
          {
            title: "Publish a Clock Melody",
            description: "Store a musical sequence for the clock piece",
            curl: `curl -X POST https://aesthetic.computer/api/store-clock \\
  -H "Content-Type: application/json" \\
  -d '{
    "source": "c4 e4 g4 c5 g4 e4 c4"
  }'`,
            javascript: `const response = await fetch("https://aesthetic.computer/api/store-clock", {
  method: "POST",
  headers: { "Content-Type": "application/json" },
  body: JSON.stringify({
    source: "c4 e4 g4 c5 g4 e4 c4"
  })
});

const { code, cached } = await response.json();
console.log(\`Listen at: https://aesthetic.computer/clock~\${code}\`);`,
            python: `import requests

response = requests.post(
    "https://aesthetic.computer/api/store-clock",
    json={
        "source": "c4 e4 g4 c5 g4 e4 c4"
    }
)

data = response.json()
print(f"Listen at: https://aesthetic.computer/clock~{data['code']}")`,
            response: {
              status: 201,
              body: {
                code: "bako",
                cached: false
              }
            }
          }
        ]
      },

      {
        name: "List Chat Messages",
        method: "GET",
        path: "/api/chat-messages",
        description: "Read recent messages from a chat channel. `system` backs the main `/chat` piece; `clock` backs `laer-klokken` (r8Dio). Results are chronological (oldest → newest) within each page. Paginate further back with `before`.",
        authentication: "None (public read)",
        queryParameters: {
          instance: {
            type: "string",
            enum: ["system", "clock", "all"],
            default: "system",
            description: "Chat channel to read. `all` spans both channels when `search` or `q` is present."
          },
          limit: {
            type: "number",
            default: 50,
            max: 100,
            description: "How many messages to return. Values over 100 return HTTP 400."
          },
          before: {
            type: "string",
            required: false,
            description: "ISO-8601 timestamp. Returns messages strictly older than this — pass the `nextBefore` from the previous response to page back."
          },
          search: {
            type: "string",
            required: false,
            description: "Case-insensitive substring search over message text. Search results are newest first."
          },
          q: {
            type: "string",
            required: false,
            description: "Alias for `search`."
          }
        },
        responseBody: {
          schema: {
            instance: { type: "string", description: "Echoes the queried channel." },
            search: { type: "string", description: "Echoes the search term when supplied; omitted otherwise." },
            count: { type: "number", description: "Number of messages in this page." },
            messages: {
              type: "array",
              description: "Chronological (oldest → newest).",
              items: {
                id: { type: "string", description: "Mongo ObjectId string." },
                from: { type: "string", description: "`@handle` of the sender, or `anon` if unresolved." },
                text: { type: "string", description: "Message body as posted." },
                when: { type: "string", description: "ISO timestamp." },
                instance: { type: "string", description: "Source channel for this message." },
                hearts: { type: "number", description: "Heart-reaction count from the shared `hearts` collection." }
              }
            },
            nextBefore: {
              type: "string",
              description: "ISO timestamp of the oldest message in this page — pass as `before=` to fetch the previous page. `null` when the page is empty."
            }
          }
        },
        examples: [
          {
            title: "Latest 50 messages from the main chat",
            description: "Default channel is `system`.",
            curl: `curl "https://aesthetic.computer/api/chat-messages"`,
            javascript: `const res = await fetch("https://aesthetic.computer/api/chat-messages");
const { messages } = await res.json();
for (const m of messages) console.log(m.when, m.from, m.text);`,
            python: `import requests

data = requests.get("https://aesthetic.computer/api/chat-messages").json()
for m in data["messages"]:
    print(m["when"], m["from"], m["text"])`
          },
          {
            title: "Latest 100 from the laer-klokken (clock) channel",
            curl: `curl "https://aesthetic.computer/api/chat-messages?instance=clock&limit=100"`,
            javascript: `const res = await fetch(
  "https://aesthetic.computer/api/chat-messages?instance=clock&limit=100"
);
const { messages, nextBefore } = await res.json();
console.log(messages.length, "messages; older page cursor:", nextBefore);`,
            python: `import requests

data = requests.get(
    "https://aesthetic.computer/api/chat-messages",
    params={"instance": "clock", "limit": 100},
).json()
print(len(data["messages"]), "messages; older page cursor:", data["nextBefore"])`
          },
          {
            title: "Paginate back through older messages",
            description: "Use `nextBefore` from each response to walk back in time.",
            curl: `curl "https://aesthetic.computer/api/chat-messages?instance=clock&limit=100&before=2026-04-20T00%3A00%3A00Z"`,
            javascript: `async function* allClockMessages() {
  let before;
  while (true) {
    const url = new URL("https://aesthetic.computer/api/chat-messages");
    url.searchParams.set("instance", "clock");
    url.searchParams.set("limit", "100");
    if (before) url.searchParams.set("before", before);
    const res = await fetch(url);
    const page = await res.json();
    if (page.count === 0) break;
    yield page.messages;
    before = page.nextBefore;
  }
}`,
            python: `import requests

def all_clock_messages():
    before = None
    while True:
        params = {"instance": "clock", "limit": 100}
        if before:
            params["before"] = before
        page = requests.get("https://aesthetic.computer/api/chat-messages",
                            params=params).json()
        if page["count"] == 0:
            break
        yield page["messages"]
        before = page["nextBefore"]`
          },
          {
            title: "Search both public chat channels",
            curl: `curl "https://aesthetic.computer/api/chat-messages?instance=all&search=music&limit=25"`,
            javascript: `const url = new URL("https://aesthetic.computer/api/chat-messages");
url.search = new URLSearchParams({ instance: "all", search: "music", limit: "25" });
const { messages } = await fetch(url).then((res) => res.json());
console.log(messages);`,
            python: `import requests

data = requests.get(
    "https://aesthetic.computer/api/chat-messages",
    params={"instance": "all", "search": "music", "limit": 25},
).json()
print(data["messages"])`
          }
        ],
        notes: [
          "Responses are cached in Redis for 2 minutes, keyed on instance, limit, before, and search.",
          "Pages without a search term are chronological; search results are newest first.",
          "`from` falls back to `anon` when a message's author has no resolved `@handle`.",
          "`hearts` comes from the shared `hearts` collection (`type: chat-<instance>`)."
        ]
      },

      {
        name: "Store JavaScript Piece",
        method: "POST",
        path: "/api/store-piece",
        description: "Publish a JavaScript piece (.mjs) anonymously by providing source code as a string. No S3 credentials needed - the server handles storage automatically.",
        authentication: "Optional (Bearer token for authenticated users)",
        requestBody: {
          contentType: "application/json",
          schema: {
            source: {
              type: "string",
              required: true,
              description: "JavaScript piece source code (max 100,000 characters). Must contain a supported lifecycle export or a default export.",
              example: "export function boot($) { $.wipe('blue'); }\nexport function paint($) { $.ink('red'); $.box(10, 10, 50, 50); }"
            },
            name: {
              type: "string",
              required: false,
              description: "Optional name for the piece (used for code generation)"
            }
          }
        },
        responseBody: {
          schema: {
            code: {
              type: "string",
              description: "Short code for accessing the piece (e.g. 'drift', 'wave')"
            },
            cached: {
              type: "boolean",
              description: "True if code already existed (deduplication)"
            },
            url: {
              type: "string",
              description: "Full URL to view the piece"
            }
          }
        },
        examples: [
          {
            title: "Publish a Simple Piece",
            description: "Create a piece with basic drawing",
            curl: `curl -X POST https://aesthetic.computer/api/store-piece \\
  -H "Content-Type: application/json" \\
  -d '{
    "source": "export function boot($) {\\n  $.wipe(\"blue\");\\n}\\n\\nexport function paint($) {\\n  $.ink(\"red\");\\n  $.box(10, 10, 50, 50);\\n}",
    "name": "red-box"
  }'`,
            javascript: `const source = \`export function boot($) {
  $.wipe("blue");
}

export function paint($) {
  $.ink("red");
  $.box(10, 10, 50, 50);
}\`;

const response = await fetch("https://aesthetic.computer/api/store-piece", {
  method: "POST",
  headers: { "Content-Type": "application/json" },
  body: JSON.stringify({
    source,
    name: "red-box"
  })
});

const { code, url, cached } = await response.json();
console.log(\`View at: \${url}\`);`,
            python: `import requests

source = """export function boot($) {
  $.wipe("blue");
}

export function paint($) {
  $.ink("red");
  $.box(10, 10, 50, 50);
}"""

response = requests.post(
    "https://aesthetic.computer/api/store-piece",
    json={
        "source": source,
        "name": "red-box"
    }
)

data = response.json()
print(f"View at: {data['url']}")`,
            response: {
              status: 201,
              body: {
                code: "red-box",
                cached: false,
                url: "https://aesthetic.computer/red-box"
              }
            }
          },
          {
            title: "Publish Interactive Piece",
            description: "Create a piece with user interaction",
            curl: `curl -X POST https://aesthetic.computer/api/store-piece \\
  -H "Content-Type: application/json" \\
  -d '{
    "source": "let x = 0;\\n\\nexport function boot($) {\\n  x = $.screen.width / 2;\\n}\\n\\nexport function paint($) {\\n  $.wipe(\"black\");\\n  $.ink(\"yellow\");\\n  $.circle(x, $.screen.height / 2, 20);\\n}\\n\\nexport function act($) {\\n  if ($.event.is(\"touch\")) x = $.event.x;\\n}"
  }'`,
            javascript: `const source = \`let x = 0;

export function boot($) {
  x = $.screen.width / 2;
}

export function paint($) {
  $.wipe("black");
  $.ink("yellow");
  $.circle(x, $.screen.height / 2, 20);
}

export function act($) {
  if ($.event.is("touch")) x = $.event.x;
}\`;

const response = await fetch("https://aesthetic.computer/api/store-piece", {
  method: "POST",
  headers: { "Content-Type": "application/json" },
  body: JSON.stringify({ source })
});

const { code, url } = await response.json();
console.log(\`View at: \${url}\`);`,
            python: `import requests

source = """let x = 0;

export function boot($) {
  x = $.screen.width / 2;
}

export function paint($) {
  $.wipe("black");
  $.ink("yellow");
  $.circle(x, $.screen.height / 2, 20);
}

export function act($) {
  if ($.event.is("touch")) x = $.event.x;
}"""

response = requests.post(
    "https://aesthetic.computer/api/store-piece",
    json={"source": source}
)

data = response.json()
print(f"View at: {data['url']}")`,
            response: {
              status: 201,
              body: {
                code: "touch",
                cached: false,
                url: "https://aesthetic.computer/touch"
              }
            }
          }
        ]
      },

      {
        name: "Track Media (Publish Artwork)",
        method: "POST",
        path: "/api/track-media",
        description: "Register an uploaded painting, piece, or tape and receive a short code. The file must already exist in Aesthetic Computer storage.",
        authentication: "Optional (Bearer token for authenticated users)",
        requestBody: {
          contentType: "application/json",
          schema: {
            slug: {
              type: "string",
              required: true,
              description: "S3/storage path where the file was uploaded"
            },
            ext: {
              type: "string",
              required: true,
              enum: ["png", "mjs", "lisp", "lua", "zip", "mp4"],
              description: "Media type: PNG painting; MJS, Lisp, or Lua piece; ZIP web tape; or finished MP4 native tape"
            },
            metadata: {
              type: "object",
              required: false,
              description: "Optional metadata (for tapes: totalDuration in seconds, max 30s)",
              properties: {
                totalDuration: {
                  type: "number",
                  description: "Duration in seconds (tapes only, max 30)"
                }
              }
            }
          }
        },
        responseBody: {
          schema: {
            code: {
              type: "string",
              description: "Short code for accessing the media"
            }
          }
        },
        examples: [
          {
            title: "Publish a JavaScript Piece",
            description: "After uploading .mjs file to S3, register it in the database",
            curl: `curl -X POST https://aesthetic.computer/api/track-media \\
  -H "Content-Type: application/json" \\
  -d '{
    "slug": "2026/02/12/my-piece.mjs",
    "ext": "mjs"
  }'`,
            javascript: `// Step 1: Upload your .mjs file to S3 (requires credentials)
// Step 2: Register the uploaded file
const response = await fetch("https://aesthetic.computer/api/track-media", {
  method: "POST",
  headers: { "Content-Type": "application/json" },
  body: JSON.stringify({
    slug: "2026/02/12/my-piece.mjs",
    ext: "mjs"
  })
});

const { code } = await response.json();
console.log(\`View at: https://aesthetic.computer/\${code}\`);`,
            python: `import requests

# After uploading your .mjs file to S3
response = requests.post(
    "https://aesthetic.computer/api/track-media",
    json={
        "slug": "2026/02/12/my-piece.mjs",
        "ext": "mjs"
    }
)

data = response.json()
print(f"View at: https://aesthetic.computer/{data['code']}")`,
            response: {
              status: 200,
              body: {
                code: "abc456"
              }
            }
          },
          {
            title: "Publish a Painting (PNG)",
            description: "Register a painting image after uploading to S3",
            curl: `curl -X POST https://aesthetic.computer/api/track-media \\
  -H "Content-Type: application/json" \\
  -d '{
    "slug": "2026/02/12/my-painting.png",
    "ext": "png"
  }'`,
            javascript: `const response = await fetch("https://aesthetic.computer/api/track-media", {
  method: "POST",
  headers: { "Content-Type": "application/json" },
  body: JSON.stringify({
    slug: "2026/02/12/my-painting.png",
    ext: "png"
  })
});

const { code } = await response.json();
console.log(\`View at: https://aesthetic.computer/\${code}\`);`,
            python: `import requests

response = requests.post(
    "https://aesthetic.computer/api/track-media",
    json={
        "slug": "2026/02/12/my-painting.png",
        "ext": "png"
    }
)

data = response.json()
print(f"View at: https://aesthetic.computer/{data['code']}")`,
            response: {
              status: 200,
              body: {
                code: "def789"
              }
            }
          },
          {
            title: "Publish a Recording Tape (ZIP)",
            description: "Register a recording after uploading ZIP to S3",
            curl: `curl -X POST https://aesthetic.computer/api/track-media \\
  -H "Content-Type: application/json" \\
  -d '{
    "slug": "2026/02/12/my-recording.zip",
    "ext": "zip",
    "metadata": {
      "totalDuration": 15.5
    }
  }'`,
            javascript: `const response = await fetch("https://aesthetic.computer/api/track-media", {
  method: "POST",
  headers: { "Content-Type": "application/json" },
  body: JSON.stringify({
    slug: "2026/02/12/my-recording.zip",
    ext: "zip",
    metadata: {
      totalDuration: 15.5  // seconds (max 30)
    }
  })
});

const { code } = await response.json();
console.log(\`Watch at: https://aesthetic.computer/\${code}\`);`,
            python: `import requests

response = requests.post(
    "https://aesthetic.computer/api/track-media",
    json={
        "slug": "2026/02/12/my-recording.zip",
        "ext": "zip",
        "metadata": {
            "totalDuration": 15.5  # seconds (max 30)
        }
    }
)

data = response.json()
print(f"Watch at: https://aesthetic.computer/{data['code']}")`,
            response: {
              status: 200,
              body: {
                code: "ghi012"
              }
            }
          }
        ]
      }
    ],

    notes: [
      "Publishing works anonymously. Add an Authorization: Bearer header to associate new work with an account.",
      "/api/store-piece stores source directly; /api/track-media registers a file that is already in Aesthetic Computer storage.",
      "Source limits: KidLisp 50,000 characters, JavaScript 100,000 characters, clock melodies 10,000 characters.",
      "Tape duration is limited to 30 seconds.",
      "Store endpoints deduplicate identical source.",
      "JavaScript pieces must export boot, paint, sim, act, or a default function."
    ],

    relatedResources: [
      {
        name: "KidLisp Documentation",
        url: "https://kidlisp.com"
      },
      {
        name: "Piece API Documentation",
        url: "https://aesthetic.computer/docs"
      },
      {
        name: "Sitemap",
        url: "https://sitemap.aesthetic.computer"
      },
      {
        name: "aesthetic.computer Main Site",
        url: "https://aesthetic.computer"
      }
    ]
  };

  // Content negotiation: HTML for browsers, JSON for APIs/LLMs
  const acceptHeader = event.headers?.accept || "";
  const format = event.queryStringParameters?.format;
  const jsonPath = event.path?.endsWith(".json");

  // Explicit format parameter takes precedence
  const wantsHTML = !jsonPath && (format === "html" ||
                    (!format && acceptHeader.includes("text/html")));

  if (wantsHTML) {
    // Serve HTML documentation for browsers
    const html = generateHTML(apiDocs);
    return respond(200, html, {
      "Content-Type": "text/html; charset=UTF-8",
      "Access-Control-Allow-Origin": "*"
    });
  }

  // Default: Return as pretty-printed JSON for LLMs/APIs
  return respond(200, apiDocs, {
    "Content-Type": "application/json",
    "Access-Control-Allow-Origin": "*"
  });
}

function generateHTML(docs) {
  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>${docs.title}</title>
  <link rel="icon" href="https://aesthetic.computer/favicon.ico" type="image/x-icon">
  <style>
    /* CSS Variables for theming */
    :root {
      --bg: #f7f7f7;
      --bg-alt: #fff;
      --text: #111;
      --text-dim: #666;
      --border: #ddd;
      --accent: rgb(205, 92, 155);
      --accent-hover: rgb(240, 180, 215);
      --code-bg: #e8e8e8;
      --code-text: #a31515;
      --pre-bg: #2a2520;
      --pre-text: #fffacd;
      --link: rgb(0, 80, 180);
      --link-hover: rgb(205, 92, 155);
    }

    @media (prefers-color-scheme: dark) {
      :root {
        --bg: #1e1e1e;
        --bg-alt: #252526;
        --text: #d4d4d4;
        --text-dim: #858585;
        --border: #3e3e42;
        --accent: rgb(205, 92, 155);
        --accent-hover: rgb(240, 180, 215);
        --code-bg: #252526;
        --code-text: #ce9178;
        --pre-bg: #1a1a1a;
        --pre-text: #e2e8f0;
        --link: rgb(150, 180, 255);
        --link-hover: rgb(205, 92, 155);
      }
    }

    * {
      box-sizing: border-box;
      margin: 0;
      padding: 0;
    }

    body {
      font-family: 'Berkeley Mono Variable', 'Noto Sans Mono', 'SF Mono', Monaco, Consolas, monospace;
      background: var(--bg);
      color: var(--text);
      line-height: 1.6;
      padding: 1em;
      max-width: 960px;
      margin: 0 auto;
      font-size: 14px;
    }

    .header {
      border: 2px solid var(--border);
      padding: 1.5em;
      margin-bottom: 1em;
      background: var(--bg-alt);
    }

    h1 {
      font-size: 1.5em;
      font-weight: bold;
      color: var(--text);
      margin-bottom: 0.5em;
    }

    .tagline {
      color: var(--text-dim);
      margin-bottom: 1em;
    }

    .version {
      display: inline-block;
      border: 1px solid var(--border);
      color: var(--text-dim);
      padding: 0.2em 0.5em;
      font-size: 0.85em;
      margin-bottom: 1em;
    }

    .quick-links {
      display: flex;
      gap: 0.5em;
      flex-wrap: wrap;
      margin-top: 1em;
    }

    .btn {
      display: inline-block;
      padding: 0.5em 1em;
      background: var(--accent);
      color: white;
      text-decoration: none;
      border: 2px solid var(--accent);
      transition: all 0.2s;
    }

    .btn:hover {
      background: var(--accent-hover);
      border-color: var(--accent-hover);
      color: var(--text);
    }

    .btn-secondary {
      background: transparent;
      color: var(--accent);
    }

    .section {
      border: 2px solid var(--border);
      padding: 1.5em;
      margin-bottom: 1em;
      background: var(--bg-alt);
    }

    h2 {
      font-size: 1.3em;
      font-weight: bold;
      color: var(--text);
      margin-bottom: 1em;
      border-bottom: 2px solid var(--border);
      padding-bottom: 0.5em;
    }

    h3 {
      font-size: 1.1em;
      font-weight: bold;
      margin-top: 1.5em;
      margin-bottom: 0.5em;
    }

    h4 {
      font-size: 1em;
      font-weight: bold;
      margin-top: 1em;
      margin-bottom: 0.5em;
      color: var(--text-dim);
    }

    p {
      margin-bottom: 0.8em;
    }

    code {
      font-family: 'Berkeley Mono Variable', 'Noto Sans Mono', Monaco, Consolas, monospace;
      background: var(--code-bg);
      padding: 0.2em 0.4em;
      font-size: 0.95em;
      color: var(--code-text);
    }

    pre {
      background: var(--pre-bg);
      color: var(--pre-text);
      padding: 1em;
      overflow-x: auto;
      margin: 1em 0;
      border: 1px solid var(--border);
      line-height: 1.4;
    }

    pre code {
      background: transparent;
      padding: 0;
      color: var(--pre-text);
    }

    .method {
      display: inline-block;
      background: var(--accent);
      color: white;
      padding: 0.2em 0.5em;
      font-weight: bold;
      font-size: 0.85em;
      margin-right: 0.5em;
    }

    .path {
      font-weight: bold;
    }

    .tabs {
      display: flex;
      gap: 0;
      margin: 1em 0 0 0;
      border-bottom: 2px solid var(--border);
    }

    .tab {
      padding: 0.5em 1em;
      background: transparent;
      border: none;
      cursor: pointer;
      font-family: inherit;
      font-size: 0.95em;
      color: var(--text-dim);
      border-bottom: 2px solid transparent;
      margin-bottom: -2px;
    }

    .tab:hover {
      color: var(--accent);
      background: var(--code-bg);
    }

    .tab.active {
      color: var(--accent);
      border-bottom-color: var(--accent);
    }

    .tab-content {
      display: none;
    }

    .tab-content.active {
      display: block;
    }

    .notes {
      border: 2px solid var(--border);
      padding: 1em;
      margin: 1em 0;
      background: var(--bg-alt);
    }

    .notes h3 {
      color: var(--text);
      margin-bottom: 0.5em;
    }

    .notes ul {
      margin-left: 1.5em;
      list-style: square;
    }

    .notes li {
      margin-bottom: 0.5em;
    }

    .footer {
      margin-top: 2em;
      padding-top: 1em;
      border-top: 1px solid var(--border);
      color: var(--text-dim);
      font-size: 0.9em;
    }

    .footer a {
      color: var(--link);
      text-decoration: none;
    }

    .footer a:hover {
      color: var(--link-hover);
      text-decoration: underline;
    }

    a {
      color: var(--link);
      text-decoration: none;
    }

    a:hover {
      color: var(--link-hover);
      text-decoration: underline;
    }

    .endpoint-card {
      border-left: 3px solid var(--border);
      padding-left: 1em;
      margin: 2em 0;
    }

    .contract {
      background: var(--code-bg);
      border: 1px solid var(--border);
      padding: 0.75em;
      margin: 0.75em 0;
    }

    .contract dt { font-weight: bold; }
    .contract dd { margin: 0 0 0.65em 1em; }

    @media (max-width: 768px) {
      body { padding: 0.5em; font-size: 13px; }
      .header { padding: 1em; }
      .section { padding: 1em; }
    }
  </style>
</head>
<body>
  <div class="header">
    <h1>${docs.title}</h1>
    <p class="tagline">${docs.description}</p>
    <div class="version">version ${docs.version} · updated ${docs.updated}</div>
    <div class="quick-links">
      <a href="?format=json" class="btn btn-secondary">[ view json ]</a>
      <a href="https://aesthetic.computer" class="btn">[ home ]</a>
      <a href="https://tangled.org/aesthetic.computer/core" class="btn">[ tangled ]</a>
    </div>
  </div>

  <div class="section">
    <h2>MCP Server</h2>
    <p>${docs.mcp.description}</p>
    <p><strong>package:</strong> <code>${docs.mcp.package}</code></p>
    <p><strong>install:</strong> <code>${docs.mcp.install}</code></p>
    <p><a href="${docs.mcp.repository}">view on tangled &rarr;</a></p>

    <h3>tools</h3>
    ${docs.mcp.tools.map(tool => `
      <p><strong><code>${tool.name}</code></strong> &mdash; ${tool.description}</p>
      <p>input: <code>${JSON.stringify(tool.input)}</code><br>
      output: <code>${typeof tool.output === 'string' ? tool.output : JSON.stringify(tool.output)}</code></p>
    `).join('')}

    <h3>resources</h3>
    ${docs.mcp.resources.map(resource => `
      <p><strong><code>${resource.uri}</code></strong> &mdash; ${resource.description}</p>
    `).join('')}

    <h3>prompts</h3>
    ${docs.mcp.prompts.map(prompt => `
      <p><strong><code>${prompt.name}</code></strong> &mdash; ${prompt.description}</p>
      <p>args: ${prompt.arguments.join(', ')}</p>
    `).join('')}

    <h3>configuration examples</h3>
    ${Object.entries(docs.mcp.configuration).map(([client, config]) => `
      <h4>${client}</h4>
      <pre><code>${escapeHTML(config)}</code></pre>
    `).join('')}
  </div>

  <div class="section">
    <h2>HTTP endpoints</h2>

    ${docs.endpoints.map((endpoint, idx) => `
      <div class="endpoint-card">
        <h3><span class="method">${endpoint.method}</span> ${endpoint.name}</h3>
        <p><code class="path">${docs.baseURL}${endpoint.path}</code></p>
        <p>${endpoint.description}</p>
        <p><strong>Authentication:</strong> ${endpoint.authentication}</p>

        ${endpoint.queryParameters ? `
          <h4>Query parameters</h4>
          <dl class="contract">
            ${Object.entries(endpoint.queryParameters).map(([name, field]) => `
              <dt><code>${name}</code> · ${field.type}${field.required ? ' · required' : ''}</dt>
              <dd>${field.description}${field.default !== undefined ? ` Default: <code>${field.default}</code>.` : ''}${field.enum ? ` Values: <code>${field.enum.join(', ')}</code>.` : ''}</dd>
            `).join('')}
          </dl>
        ` : ''}

        ${endpoint.requestBody ? `
          <h4>Request body · ${endpoint.requestBody.contentType}</h4>
          <pre><code>${escapeHTML(JSON.stringify(endpoint.requestBody.schema, null, 2))}</code></pre>
        ` : ''}

        ${endpoint.responseBody ? `
          <h4>Response</h4>
          <pre><code>${escapeHTML(JSON.stringify(endpoint.responseBody.schema, null, 2))}</code></pre>
        ` : ''}

        ${(endpoint.examples || []).map((example, exIdx) => `
          <h3>${example.title || ''}</h3>
          <p>${example.description || ''}</p>

          <div class="tabs" id="tabs-${idx}-${exIdx}">
            <button class="tab active" onclick="showTab(${idx}, ${exIdx}, 'curl')">curl</button>
            <button class="tab" onclick="showTab(${idx}, ${exIdx}, 'js')">javascript</button>
            <button class="tab" onclick="showTab(${idx}, ${exIdx}, 'py')">python</button>
          </div>

          <div class="tab-content active" id="content-${idx}-${exIdx}-curl">
            <pre>${escapeHTML(example.curl || '')}</pre>
          </div>
          <div class="tab-content" id="content-${idx}-${exIdx}-js">
            <pre>${escapeHTML(example.javascript || '')}</pre>
          </div>
          <div class="tab-content" id="content-${idx}-${exIdx}-py">
            <pre>${escapeHTML(example.python || '')}</pre>
          </div>

          ${example.response ? `<h4>response:</h4>
          <pre><code>${escapeHTML(JSON.stringify(example.response.body ?? example.response, null, 2))}</code></pre>` : ''}
        `).join('')}

        ${endpoint.notes ? `
          <h4>Notes</h4>
          <ul>${endpoint.notes.map(note => `<li>${note}</li>`).join('')}</ul>
        ` : ''}
      </div>
    `).join('')}
  </div>

  <div class="notes">
    <h3>Limits and behavior</h3>
    <ul>
      ${docs.notes.map(note => `<li>${note}</li>`).join('')}
    </ul>
  </div>

  <div class="footer">
    ${docs.relatedResources.map(r =>
      `<a href="${r.url}" target="_blank">${r.name}</a>`
    ).join(' • ')}
  </div>

  <script>
    function showTab(endpointIdx, exampleIdx, lang) {
      const tabsContainer = document.getElementById(\`tabs-\${endpointIdx}-\${exampleIdx}\`);
      const tabs = tabsContainer.querySelectorAll('.tab');
      const contents = ['curl', 'js', 'py'];

      tabs.forEach((tab, i) => {
        tab.classList.remove('active');
        const contentId = \`content-\${endpointIdx}-\${exampleIdx}-\${contents[i]}\`;
        const content = document.getElementById(contentId);
        if (content) content.classList.remove('active');
      });

      const activeTab = Array.from(tabs).find(t =>
        t.textContent.toLowerCase().includes(lang === 'js' ? 'javascript' : lang)
      );
      if (activeTab) activeTab.classList.add('active');

      const activeContent = document.getElementById(\`content-\${endpointIdx}-\${exampleIdx}-\${lang}\`);
      if (activeContent) activeContent.classList.add('active');
    }
  </script>
</body>
</html>`;
}

function escapeHTML(str) {
  return str
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#039;');
}
