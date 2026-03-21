// MCP Server for aesthetic.computer
// Exposes tools, resources, and prompts for publishing and exploring pieces

import { Server } from "@modelcontextprotocol/sdk/server/index.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import {
  CallToolRequestSchema,
  ListResourcesRequestSchema,
  ListToolsRequestSchema,
  ReadResourceRequestSchema,
  ListPromptsRequestSchema,
  GetPromptRequestSchema,
} from "@modelcontextprotocol/sdk/types.js";

// Import tools
import { publishPieceTool, publishPiece } from "./tools/publish-piece.js";
import { publishKidLispTool, publishKidLisp } from "./tools/publish-kidlisp.js";
import { publishClockTool, publishClock } from "./tools/publish-clock.js";
import { getAPIInfoTool, getAPIInfo } from "./tools/get-api-info.js";
import {
  previewKidLispTool,
  previewKidLisp,
} from "./tools/preview-kidlisp.js";

// Import resources
import {
  pieceTemplateResource,
  getPieceTemplate,
} from "./resources/piece-template.js";
import {
  kidlispReferenceResource,
  getKidLispReference,
} from "./resources/kidlisp-reference.js";
import {
  pieceExamplesResource,
  getPieceExamples,
} from "./resources/piece-examples.js";

// Import prompts
import {
  createPiecePrompt,
  getCreatePiecePrompt,
} from "./prompts/create-piece.js";
import {
  createKidLispPrompt,
  getCreateKidLispPrompt,
} from "./prompts/create-kidlisp.js";

export class AestheticComputerServer {
  private server: Server;
  private token?: string;

  constructor(token?: string) {
    this.token = token;
    this.server = new Server(
      {
        name: "aesthetic-computer",
        version: "1.1.0",
      },
      {
        capabilities: {
          tools: {},
          resources: {},
          prompts: {},
        },
      }
    );

    this.setupHandlers();
  }

  private setupHandlers() {
    // List available tools
    this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
      tools: [
        publishPieceTool,
        publishKidLispTool,
        publishClockTool,
        getAPIInfoTool,
        previewKidLispTool,
      ],
    }));

    // Handle tool calls
    this.server.setRequestHandler(CallToolRequestSchema, async (request) => {
      const { name, arguments: args } = request.params;

      try {
        switch (name) {
          case "publish_piece":
            return {
              content: [
                {
                  type: "text",
                  text: JSON.stringify(
                    await publishPiece(
                      args as { source: string; name?: string },
                      this.token
                    ),
                    null,
                    2
                  ),
                },
              ],
            };

          case "publish_kidlisp":
            return {
              content: [
                {
                  type: "text",
                  text: JSON.stringify(
                    await publishKidLisp(args as { source: string }, this.token),
                    null,
                    2
                  ),
                },
              ],
            };

          case "publish_clock":
            return {
              content: [
                {
                  type: "text",
                  text: JSON.stringify(
                    await publishClock(args as { source: string }, this.token),
                    null,
                    2
                  ),
                },
              ],
            };

          case "get_api_info":
            return {
              content: [
                {
                  type: "text",
                  text: JSON.stringify(await getAPIInfo(), null, 2),
                },
              ],
            };

          case "preview_kidlisp":
            return {
              content: [
                {
                  type: "text",
                  text: JSON.stringify(
                    await previewKidLisp(args as { source: string }),
                    null,
                    2
                  ),
                },
              ],
            };

          default:
            throw new Error(`Unknown tool: ${name}`);
        }
      } catch (error) {
        return {
          content: [
            {
              type: "text",
              text: `Error: ${error instanceof Error ? error.message : String(error)}`,
            },
          ],
          isError: true,
        };
      }
    });

    // List available resources
    this.server.setRequestHandler(ListResourcesRequestSchema, async () => ({
      resources: [
        pieceTemplateResource,
        kidlispReferenceResource,
        pieceExamplesResource,
      ],
    }));

    // Read resource content
    this.server.setRequestHandler(ReadResourceRequestSchema, async (request) => {
      const { uri } = request.params;

      switch (uri) {
        case "aesthetic-computer://piece-template":
          return {
            contents: [
              {
                uri,
                mimeType: "text/javascript",
                text: getPieceTemplate(),
              },
            ],
          };

        case "aesthetic-computer://kidlisp-reference":
          return {
            contents: [
              {
                uri,
                mimeType: "text/markdown",
                text: getKidLispReference(),
              },
            ],
          };

        case "aesthetic-computer://piece-examples":
          return {
            contents: [
              {
                uri,
                mimeType: "text/markdown",
                text: getPieceExamples(),
              },
            ],
          };

        default:
          throw new Error(`Unknown resource: ${uri}`);
      }
    });

    // List available prompts
    this.server.setRequestHandler(ListPromptsRequestSchema, async () => ({
      prompts: [createPiecePrompt, createKidLispPrompt],
    }));

    // Get prompt content
    this.server.setRequestHandler(GetPromptRequestSchema, async (request) => {
      const { name, arguments: args } = request.params;

      switch (name) {
        case "create-piece":
          return {
            messages: [
              {
                role: "user",
                content: {
                  type: "text",
                  text: getCreatePiecePrompt(
                    args as { name: string; description: string }
                  ),
                },
              },
            ],
          };

        case "create-kidlisp":
          return {
            messages: [
              {
                role: "user",
                content: {
                  type: "text",
                  text: getCreateKidLispPrompt(
                    args as { description: string }
                  ),
                },
              },
            ],
          };

        default:
          throw new Error(`Unknown prompt: ${name}`);
      }
    });
  }

  async run() {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    console.error("Aesthetic Computer MCP server running on stdio");
  }
}
