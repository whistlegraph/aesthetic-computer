const { stream } = require("@netlify/functions");

const dev = process.env.CONTEXT === "dev";

const allowedOrigins = [
  "https://aesthetic.computer",
  "https://botce.ac",
  "https://chat-system.aesthetic.computer",
];

// Supports both OpenAI and Anthropic Claude models
// 
// Model detection: Any model name containing "claude" routes to Anthropic API
//
// CLAUDE MODELS (Anthropic):
// - claude-opus-4-20250514 (Most capable, $15/MTok input)
// - claude-sonnet-4-20250514 (High-performance, $3/MTok input) 
// - claude-3-7-sonnet-20250219 (Extended thinking, $3/MTok input)
// - claude-3-5-sonnet-20241022 (Previous flagship, $3/MTok input)
// - claude-3-5-haiku-20241022 (Fastest, $0.80/MTok input)
//
// OPENAI MODELS:
// - gpt-4o (Default for code hints)
// - gpt-4o-mini (Default model)
// - gpt-3.5-turbo
//
// USAGE EXAMPLES:
// "character:claude-3-5-sonnet-20241022" - Character roleplay with Claude
// "code:claude-sonnet-4-20250514" - Code generation with Claude Sonnet 4
// "code:gpt-4o" - Code generation with OpenAI (default)
// "character:gpt-4o-mini" - Character interactions with OpenAI
//
// ENVIRONMENT VARIABLES REQUIRED:
// - ANTHROPIC_API_KEY (for Claude models)
// - OPENAI_API_KEY (for OpenAI models)

async function handleClaudeRequest(messages, model, temperature, top_p, max_tokens, origin) {
  if (!process.env.ANTHROPIC_API_KEY) {
    console.error("ANTHROPIC_API_KEY not configured");
    return {
      statusCode: 500,
      headers: { "Content-Type": "text/plain" },
      body: "Claude API not configured",
    };
  }

  // Extract system messages for Anthropic API format
  const systemMessages = messages.filter(msg => msg.role === "system");
  const nonSystemMessages = messages.filter(msg => msg.role !== "system");
  
  // Combine system messages into a single system parameter
  const systemContent = systemMessages.map(msg => msg.content).join("\n\n");
  
  const payload = {
    model,
    messages: nonSystemMessages,
    temperature,
    top_p,
    max_tokens,
    stream: true,
  };

  // Add system parameter if we have system content
  if (systemContent) {
    payload.system = systemContent;
  }

  const res = await fetch("https://api.anthropic.com/v1/messages", {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      "x-api-key": process.env.ANTHROPIC_API_KEY,
      "anthropic-version": "2023-06-01",
    },
    body: JSON.stringify(payload),
  });

  if (!res.ok) {
    const errorData = await res.json().catch(() => ({ error: "Failed to parse error response" }));
    console.error("Anthropic request failed:", JSON.stringify(errorData, null, 2));
    return {
      statusCode: 500,
      headers: { "Content-Type": "text/plain" },
      body: "Error",
    };
  }

  const stream = new ReadableStream({
    async start(controller) {
      let buffer = "";

      try {
        for await (const chunk of res.body) {
          const chunkText = new TextDecoder().decode(chunk);
          buffer += chunkText;

          // Process complete SSE events (separated by double newlines)
          let boundary = buffer.indexOf("\n\n");
          while (boundary !== -1) {
            const sseEvent = buffer.slice(0, boundary).trim();
            buffer = buffer.slice(boundary + 2);
            boundary = buffer.indexOf("\n\n");

            if (sseEvent) {
              // Parse SSE event (can have multiple lines: event: type, data: json)
              const lines = sseEvent.split('\n');
              let eventType = '';
              let eventData = '';
              
              for (const line of lines) {
                if (line.startsWith('event: ')) {
                  eventType = line.slice(7).trim();
                } else if (line.startsWith('data: ')) {
                  eventData = line.slice(6).trim();
                }
              }
              
              if (eventData) {
                try {
                  const json = JSON.parse(eventData);
                  
                  // Handle different Anthropic event types
                  if (json.type === "content_block_delta" && json.delta?.type === "text_delta") {
                    const text = json.delta.text || "";
                    if (text) {
                      controller.enqueue(new TextEncoder().encode(text));
                    }
                  } else if (json.type === "message_stop") {
                    controller.close();
                    return;
                  }
                  // Skip other event types (ping, message_start, content_block_start, etc.)
                } catch (e) {
                  console.error("Error parsing Anthropic response:", e);
                  // Don't error the whole stream for one bad event
                }
              }
            }
          }
        }
        controller.close();
      } catch (e) {
        console.error("Stream error:", e);
        controller.error(e);
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
}

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
    console.log("🔧 Initial model from hint:", model);

    if (hint.startsWith("code")) {
      // Only set default model if none was specified in the hint
      const hasModelInHint = hint.includes(":") && hint.split(":")[1] !== "";
      console.log("🔧 Code hint detected, hasModelInHint:", hasModelInHint);
      if (!hasModelInHint) {
        model = "gpt-4o";
        console.log("🔧 No model specified, using default:", model);
      } else {
        console.log("🔧 Using model from hint:", model);
      }
      max_tokens = 2048; // Increased for longer, more detailed code responses
    }

    // Check if it's a Claude model
    const isClaudeModel = model.includes("claude");
    
    // Extract the last user message as the prompt for logging
    const lastUserMessage = messages.filter(msg => msg.role === "user").pop();
    const prompt = lastUserMessage?.content || "No user message found";
    
    // Log what's being asked and which model will be used
    console.log(`🤖 Model: ${model} ${isClaudeModel ? '(Anthropic)' : '(OpenAI)'}`);
    console.log(`❓ Prompt: "${prompt.substring(0, 100)}${prompt.length > 100 ? '...' : ''}"`);
    
    if (isClaudeModel) {
      return handleClaudeRequest(messages, model, temperature, top_p, max_tokens, origin);
    } else {
      // OpenAI request
      if (!process.env.OPENAI_API_KEY) {
        console.error("OPENAI_API_KEY not configured");
        return {
          statusCode: 500,
          headers: { "Content-Type": "text/plain" },
          body: "OpenAI API not configured",
        };
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
    }
  } catch (error) {
    console.error("Failed to process the request:", error);
    return {
      statusCode: 500,
      headers: { "Content-Type": "text/plain" },
      body: "Error",
    };
  }
});
