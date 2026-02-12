// Remote MCP Server, 2026.02.12
// SSE-based MCP server for aesthetic.computer
// Enables Claude Pro/Team users to connect without local installation

import { respond } from "../../backend/http.mjs";

// Import the MCP server implementation
// (We'll share this with the stdio version)
const SERVER_INFO = {
  name: "aesthetic-computer",
  version: "1.0.0",
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
    description: "Publish KidLisp code to aesthetic.computer",
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
    description: "Publish a clock melody to aesthetic.computer",
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
    description: "Fetch the full API documentation from aesthetic.computer",
    inputSchema: {
      type: "object",
      properties: {},
    },
  },
];

const RESOURCES = [
  {
    uri: "aesthetic-computer://piece-template",
    name: "Piece Template",
    description: "Starter template for a new aesthetic.computer piece",
    mimeType: "text/plain",
  },
  {
    uri: "aesthetic-computer://kidlisp-reference",
    name: "KidLisp Reference",
    description: "Quick reference guide for KidLisp syntax",
    mimeType: "text/plain",
  },
];

const PROMPTS = [
  {
    name: "create-piece",
    description: "Guided prompt for creating an aesthetic.computer piece",
    arguments: [
      {
        name: "name",
        description: "Name of the piece",
        required: true,
      },
      {
        name: "description",
        description: "What the piece should do",
        required: true,
      },
    ],
  },
];

// Handle MCP protocol messages
async function handleMCPMessage(message, authToken) {
  const { method, params } = message;

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

// Handle tool calls
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

      default:
        throw new Error(`Unknown tool: ${name}`);
    }
  } catch (error) {
    return {
      content: [
        {
          type: "text",
          text: `Error: ${error.message}`,
        },
      ],
      isError: true,
    };
  }
}

// Tool implementations (call existing APIs)
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
        text: `✅ Published piece: ${result.url}\nCode: ${result.code}${
          result.cached ? " (cached - already existed)" : ""
        }`,
      },
    ],
  };
}

async function callPublishKidLisp(args, authToken) {
  const headers = { "Content-Type": "application/json" };
  if (authToken) headers["Authorization"] = `Bearer ${authToken}`;

  const response = await fetch(
    "https://aesthetic.computer/api/store-kidlisp",
    {
      method: "POST",
      headers,
      body: JSON.stringify(args),
    }
  );

  const result = await response.json();
  const url = `https://aesthetic.computer/${result.code}`;

  return {
    content: [
      {
        type: "text",
        text: `✅ Published KidLisp piece: ${url}\nCode: ${result.code}${
          result.cached ? " (cached - already existed)" : ""
        }`,
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
  const url = `https://aesthetic.computer/clock~${result.code}`;

  return {
    content: [
      {
        type: "text",
        text: `✅ Published clock melody: ${url}\nCode: ${result.code}${
          result.cached ? " (cached - already existed)" : ""
        }`,
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
    content: [
      {
        type: "text",
        text: JSON.stringify(docs, null, 2),
      },
    ],
  };
}

// Handle resource reads
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
          text: `KidLisp Quick Reference

Basic Drawing:
  (wipe color)          - Clear screen with color
  (ink color)           - Set drawing color
  (circle x y radius)   - Draw circle
  (box x y w h)         - Draw rectangle
  (line x1 y1 x2 y2)    - Draw line

Variables:
  w, h                  - Screen width/height
  t                     - Time in frames

Math:
  (+, -, *, /)          - Basic arithmetic
  (random min max)      - Random number
  (sin x), (cos x)      - Trigonometry

More: https://kidlisp.com`,
        },
      ],
    };
  }

  throw new Error(`Unknown resource: ${uri}`);
}

// Handle prompt get
async function handlePromptGet(params) {
  const { name, arguments: args } = params;

  if (name === "create-piece") {
    return {
      description: "Create an aesthetic.computer piece",
      messages: [
        {
          role: "user",
          content: {
            type: "text",
            text: `Create a new aesthetic.computer piece called "${args.name}" that ${args.description}. Use the publish_piece tool to publish it.`,
          },
        },
      ],
    };
  }

  throw new Error(`Unknown prompt: ${name}`);
}

// Main handler
export async function handler(event, context) {
  console.log(`📡 Remote MCP request: ${event.httpMethod} ${event.path}`);

  // CORS headers
  const corsHeaders = {
    "Access-Control-Allow-Origin": "*",
    "Access-Control-Allow-Methods": "GET, POST, OPTIONS",
    "Access-Control-Allow-Headers": "Content-Type, Authorization",
  };

  if (event.httpMethod === "OPTIONS") {
    return {
      statusCode: 204,
      headers: corsHeaders,
      body: "",
    };
  }

  try {
    // Extract auth token from headers
    const authToken = event.headers.authorization?.replace("Bearer ", "");

    if (event.httpMethod === "GET") {
      // SSE endpoint
      const headers = {
        ...corsHeaders,
        "Content-Type": "text/event-stream",
        "Cache-Control": "no-cache",
        Connection: "keep-alive",
      };

      // Initial connection message
      const initMessage = JSON.stringify({
        jsonrpc: "2.0",
        method: "server/info",
        params: SERVER_INFO,
      });

      return {
        statusCode: 200,
        headers,
        body: `data: ${initMessage}\n\n`,
      };
    }

    if (event.httpMethod === "POST") {
      // Handle MCP message
      const message = JSON.parse(event.body || "{}");
      console.log(`🔧 MCP method: ${message.method}`);

      const result = await handleMCPMessage(message, authToken);

      const response = {
        jsonrpc: "2.0",
        id: message.id,
        result,
      };

      return respond(200, response, corsHeaders);
    }

    return respond(405, { error: "Method not allowed" }, corsHeaders);
  } catch (error) {
    console.error("❌ Remote MCP error:", error);
    return respond(
      500,
      {
        jsonrpc: "2.0",
        error: {
          code: -32603,
          message: error.message,
        },
      },
      corsHeaders
    );
  }
}
