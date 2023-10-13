// Slash, 23.10.13.00.08
// A helpful Discord webhook for auto-linking AC pieces.

export const config = { path: "/slash" };

export default async function handleRequest(request) {
  if (request.method === "POST") {
    const body = await request.json();
    console.log("✏️ Received:", body);

    let out = "";
    switch (body?.input) {
      case "hello":
        out = "Hi there!";
        break;
      case "bye":
        out = "Goodbye!";
        break;
      default:
        out = `I'm not sure how to respond to '${input}'.`;
        break;
    }

    // Respond to Discord
    return new Response(JSON.stringify({ type: 4, data: { content: out } }), {
      headers: { "Content-Type": "application/json" },
    });
  }
}
