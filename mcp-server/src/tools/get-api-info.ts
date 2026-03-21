// Tool: get_api_info
// Fetch the full API documentation from aesthetic.computer

export const getAPIInfoTool = {
  name: "get_api_info",
  description: "Get the full API documentation for aesthetic.computer, including all available endpoints and examples.",
  inputSchema: {
    type: "object" as const,
    properties: {},
  },
};

export async function getAPIInfo() {
  const response = await fetch("https://aesthetic.computer/api/api-docs?format=json");

  if (!response.ok) {
    throw new Error(`Failed to fetch API info: ${response.statusText}`);
  }

  const apiDocs = await response.json();
  return apiDocs;
}
