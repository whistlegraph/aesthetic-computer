// Ask, 23.05.16.13.49
// A vercel edge function to handle OpenAI text prediction APIs.

import { corsHeaders } from "../help.mjs";
// import { count } from "openai-gpt-token-counter";
// ^ Would require moving to a different runtime. 23.05.29.18.02

const dev = process.env.NODE_ENV !== "development";

export default async function handler(req) {
  const headers = corsHeaders(req);
  const allowedOrigins = ["https://aesthetic.computer", "https://botce.ac"];
  const origin = req.headers.get("Origin");

  // Allow requests in development environment or if the origin is in the allowed list
  if (dev && !allowedOrigins.includes(origin)) {
    return new Response("Access denied.", {
      status: 403,
      headers: { "Content-Type": "text/plain", ...headers },
    });
  }

  if (req.method === "GET") {
    return new Response("Wrong method.", {
      headers: { "Content-Type": "text/plain", ...headers },
    });
  }

  if (req.method === "OPTIONS") {
    return new Response("Success!", {
      headers: { "Content-Type": "text/plain", ...headers },
    });
  }

  if (req.method === "POST") {
    const body = await req.json();
    let { messages, hint } = body;

    try {
      messages = messages?.map((message) => {
        return { role: message.by, content: message.text };
      });

      // ❤️‍🔥 TODO: Measure max token size for conversation history.

      // Defaults
      let temperature = 1;
      let top_p = 1; // Maximum: 1
      let max_tokens = 64;

      // Tweak for "code" based formal output.
      if (hint.startsWith("code")) {
        temperature = 0.4;
        top_p = 1;
        max_tokens = 256;
      }

      // Tweak for "character" dialogical output.
      if (hint.startsWith("character")) {
        temperature = 1;
        top_p = 0.5;
        max_tokens = 256;
      }

      // const model = hint.split(":")[1] || "gpt-3.5-turbo";
      const model = hint.split(":")[1] || "gpt-4o"; // "gpt-4-0125-preview";

      if (model.startsWith("gpt-4")) {
        top_p = 1;
        max_tokens = 350;
      }

      // Request streaming response
      const payload: OpenAIStreamPayload = {
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

      const stream = await OpenAIStream(payload);

      if (!stream) {
        return new Response("Error", {
          status: 500,
          headers: { "Content-Type": "text/plain", ...headers },
        });
      } else {
        return new Response(stream, { headers });
      }
    } catch (error) {
      console.error("Failed to process the request:", error);
      return new Response("Error", {
        status: 500,
        headers: { "Content-Type": "text/plain", ...headers },
      });
    }
  } else {
    return new Response("Wrong method.");
  }
}

export const config = { runtime: "edge" };

// Extracted from: https://github.com/Nutlope/twitterbio/blob/main/utils/OpenAIStream.ts

import {
  createParser,
  ParsedEvent,
  ReconnectInterval,
} from "eventsource-parser";

type ChatGPTAgent = "user" | "system";

interface ChatGPTMessage {
  role: ChatGPTAgent;
  content: string;
}

interface OpenAIStreamPayload {
  model: string;
  messages: ChatGPTMessage[];
  temperature: number;
  top_p: number;
  frequency_penalty: number;
  presence_penalty: number;
  max_tokens: number;
  stream: boolean;
  n: number;
}

async function OpenAIStream(payload: OpenAIStreamPayload) {
  const encoder = new TextEncoder();
  const decoder = new TextDecoder();

  let counter = 0;
  let res;

  try {
    res = await fetch("https://api.openai.com/v1/chat/completions", {
      headers: {
        "Content-Type": "application/json",
        Authorization: `Bearer ${process.env.OPENAI_API_KEY ?? ""}`,
      },
      method: "POST",
      body: JSON.stringify(payload),
    });

    if (!res.ok) {
      const errorData = await res.json();
      console.error("Request failed:", errorData);
    }
  } catch (err) {
    console.error("Request failed:", err);
  }

  if (!res || !res.ok) return; // Return early if an error was caught.

  const stream = new ReadableStream({
    async start(controller) {
      // callback
      function parse(event: ParsedEvent | ReconnectInterval) {
        if (event.type === "event") {
          const data = event.data;
          // https://beta.openai.com/docs/api-reference/completions/create#completions/create-stream
          if (data === "[DONE]") {
            controller.close();
            return;
          }
          try {
            const json = JSON.parse(data);
            const text = json.choices[0].delta?.content || "";
            // prefix character (i.e., "\n\n"), do nothing
            if (counter < 2 && (text.match(/\n/) || []).length) return;
            const queue = encoder.encode(text);
            controller.enqueue(queue);
            counter += 1;
          } catch (e) {
            controller.error(e);
          }
        }
      }

      // The stream response (SSE) from OpenAI may be fragmented into multiple
      // chunks this ensures we properly read chunks and invoke events for each.
      const parser = createParser(parse);
      // https://web.dev/streams/#asynchronous-iteration
      for await (const chunk of res.body as any) {
        parser.feed(decoder.decode(chunk));
      }
    },
  });

  return stream;
}
