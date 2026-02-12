// Tool: publish_kidlisp
// Publish KidLisp code to aesthetic.computer

export const publishKidLispTool = {
  name: "publish_kidlisp",
  description: "Publish KidLisp code to aesthetic.computer. KidLisp is a creative coding Lisp dialect.",
  inputSchema: {
    type: "object" as const,
    properties: {
      source: {
        type: "string",
        description: "KidLisp source code (max 50,000 characters)",
      },
    },
    required: ["source"],
  },
};

export async function publishKidLisp(args: { source: string }, token?: string) {
  const headers: Record<string, string> = {
    "Content-Type": "application/json",
  };

  if (token) {
    headers["Authorization"] = `Bearer ${token}`;
  }

  const response = await fetch("https://aesthetic.computer/api/store-kidlisp", {
    method: "POST",
    headers,
    body: JSON.stringify({
      source: args.source,
    }),
  });

  if (!response.ok) {
    const error: any = await response.json().catch(() => ({ error: response.statusText }));
    throw new Error(`Failed to publish KidLisp: ${error.error || response.statusText}`);
  }

  const result: any = await response.json();
  return {
    code: result.code,
    url: `https://aesthetic.computer/${result.code}`,
    cached: result.cached,
  };
}
