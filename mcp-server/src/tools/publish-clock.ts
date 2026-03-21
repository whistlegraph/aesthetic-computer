// Tool: publish_clock
// Publish a clock melody to aesthetic.computer

export const publishClockTool = {
  name: "publish_clock",
  description: "Publish a clock melody to aesthetic.computer. Returns a pronounceable short code.",
  inputSchema: {
    type: "object" as const,
    properties: {
      source: {
        type: "string",
        description: "Clock melody string (e.g., 'c4 e4 g4 c5'). Max 10,000 characters.",
      },
    },
    required: ["source"],
  },
};

export async function publishClock(args: { source: string }, token?: string) {
  const headers: Record<string, string> = {
    "Content-Type": "application/json",
  };

  if (token) {
    headers["Authorization"] = `Bearer ${token}`;
  }

  const response = await fetch("https://aesthetic.computer/api/store-clock", {
    method: "POST",
    headers,
    body: JSON.stringify({
      source: args.source,
    }),
  });

  if (!response.ok) {
    const error: any = await response.json().catch(() => ({ error: response.statusText }));
    throw new Error(`Failed to publish clock: ${error.error || response.statusText}`);
  }

  const result: any = await response.json();
  return {
    code: result.code,
    url: `https://aesthetic.computer/clock~${result.code}`,
    cached: result.cached,
  };
}
