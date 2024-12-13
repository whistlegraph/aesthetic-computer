const { stream } = require("@netlify/functions");

const dev = process.env.CONTEXT === "dev";

const allowedOrigins = [
  "https://aesthetic.computer",
  "https://botce.ac",
  "https://chat-system.aesthetic.computer",
];

exports.handler = stream(async (event) => {
  const origin = event.headers.origin;

  // CORS handling
  if (!dev && !allowedOrigins.includes(origin)) {
    return {
      statusCode: 403,
      headers: { "Content-Type": "text/plain" },
      body: "Access denied.",
    };
  }

  // Handle OPTIONS preflight requests
  if (event.httpMethod === "OPTIONS") {
    return {
      statusCode: 200,
      headers: {
        "Content-Type": "text/plain",
        "Access-Control-Allow-Origin": origin,
        "Access-Control-Allow-Methods": "POST, OPTIONS",
        "Access-Control-Allow-Headers": "Content-Type",
      },
      body: "Success!",
    };
  }

  if (event.httpMethod !== "POST") {
    return {
      statusCode: 405,
      headers: { "Content-Type": "text/plain" },
      body: "Wrong method.",
    };
  }

  const body = JSON.parse(event.body);
  let { messages, hint } = body;
  console.log("🧠 Hint:", hint);

  try {
    messages = messages?.map((message) => {
      return { role: message.by, content: message.text };
    });

    // Defaults
    let temperature = 1;
    let top_p = 1;
    let max_tokens = 256;

    if (hint.startsWith("character")) {
      temperature = 1;
      top_p = 0.5;
      max_tokens = 256;
    }

    let model = hint.split(":")[1] || "gpt-4o-mini";

    if (hint.startsWith("code")) {
      model = "gpt-4o";
      max_tokens = 512;
    }

    const payload = {
      model,
      messages,
      temperature,
      top_p,
      frequency_penalty: 0,
      presence_penalty: 0,
      max_tokens,
      stream: true,
      n: 1,
    };

    const res = await fetch("https://api.openai.com/v1/chat/completions", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        Authorization: `Bearer ${process.env.OPENAI_API_KEY}`,
      },
      body: JSON.stringify(payload),
    });

    if (!res.ok) {
      const errorData = await res.json();
      console.error("Request failed:", errorData);
      return {
        statusCode: 500,
        headers: { "Content-Type": "text/plain" },
        body: "Error",
      };
    }

    const stream = new ReadableStream({
      async start(controller) {
        let buffer = "";

        for await (const chunk of res.body) {
          buffer += new TextDecoder().decode(chunk);

          let boundary = buffer.indexOf("\n\n");
          while (boundary !== -1) {
            const part = buffer.slice(0, boundary).trim();
            buffer = buffer.slice(boundary + 2);
            boundary = buffer.indexOf("\n\n");

            if (part.startsWith("data: ")) {
              const jsonString = part.slice(6).trim();
              if (jsonString === "[DONE]") {
                controller.close();
                return;
              }
              try {
                const json = JSON.parse(jsonString);
                const text = json.choices[0]?.delta?.content || "";
                if (text) {
                  controller.enqueue(new TextEncoder().encode(text));
                }
              } catch (e) {
                console.error("Error parsing part:", e);
                controller.error(e);
              }
            }
          }
        }
      },
    });

    return {
      headers: {
        "Content-Type": "text/event-stream",
        "Access-Control-Allow-Origin": origin,
      },
      statusCode: 200,
      body: stream,
    };
  } catch (error) {
    console.error("Failed to process the request:", error);
    return {
      statusCode: 500,
      headers: { "Content-Type": "text/plain" },
      body: "Error",
    };
  }
});
