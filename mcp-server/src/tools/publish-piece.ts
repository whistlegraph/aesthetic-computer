// Tool: publish_piece
// Publish a JavaScript piece to aesthetic.computer

export const publishPieceTool = {
  name: "publish_piece",
  description: "Publish a JavaScript piece to aesthetic.computer. The piece will be stored and given a short code URL.",
  inputSchema: {
    type: "object" as const,
    properties: {
      source: {
        type: "string",
        description: "JavaScript source code for the piece. Must export lifecycle functions like boot, paint, sim, or act.",
      },
      name: {
        type: "string",
        description: "Optional name for the piece (used for code generation)",
      },
    },
    required: ["source"],
  },
};

export async function publishPiece(args: { source: string; name?: string }, token?: string) {
  const headers: Record<string, string> = {
    "Content-Type": "application/json",
  };

  if (token) {
    headers["Authorization"] = `Bearer ${token}`;
  }

  const response = await fetch("https://aesthetic.computer/api/store-piece", {
    method: "POST",
    headers,
    body: JSON.stringify({
      source: args.source,
      name: args.name,
    }),
  });

  if (!response.ok) {
    const error: any = await response.json().catch(() => ({ error: response.statusText }));
    throw new Error(`Failed to publish piece: ${error.error || response.statusText}`);
  }

  const result: any = await response.json();
  return {
    code: result.code,
    url: result.url,
    cached: result.cached,
  };
}
