// Query, 23.05.16.13.49
// A vercel edge function to handle OpenAI text prediction APIs.

import { OpenAI } from "openai-streams";

export default async function handler(request, context) {
  const origin = request.headers.get("Origin");
  const production = origin === "https://aesthetic.computer";
  const allowedOrigin = production ? "https://aesthetic.computer" : "*";

  const headers = {
    "Access-Control-Allow-Origin": allowedOrigin,
    "Access-Control-Allow-Headers": "Content-Type",
  }; // Define CORS headers.

  if (request.method === "OPTIONS")
    return new Response("Success!", { headers });

  if (request.method === "POST") {
    const body = await request.json();
    let { prompt, program } = body;

    try {
      // A. Completions
      if (program.before.length > 0)
        prompt = `${body.program.before} - ${prompt}`;
      if (program.after.length > 0) prompt += ` - ${program.after}`;

      // const controller = new AbortController();

      // const stream = await OpenAI(
      //   "completions",
      //   {
      //     model: "text-davinci-003",
      //     prompt,
      //     max_tokens: 1024,
      //     temperature: 1.2,
      //   },
      //   {
      //     mode: "raw",
      //     // controller,
      //   }
      // );

      // TODO: How can I cancel this readable stream?
      // This is not currently working: 23.05.16.13.49
      // request.signal.addEventListener("abort", () => {
        // console.log("Cancelled stream!");
        // controller.abort(); // Send to a new abort controller.
      // });

       const payload: OpenAIStreamPayload = {
         model: "gpt-3.5-turbo",
         messages: [{ role: "user", content: prompt }],
         temperature: 0.7,
         top_p: 1,
         frequency_penalty: 0,
         presence_penalty: 0,
         max_tokens: 200,
         stream: true,
         n: 1,
       };

       const stream = await OpenAIStream(payload);
       return new Response(stream, { headers });

      // return new Response(stream, { headers });
      // return new Response("hello", { headers });
    } catch (error) {
      console.error("Failed to process the request:", error);
      return new Response("Error", {
        status: 500,
        headers: { "Content-Type": "text/plain", ...headers },
      });
    }
  }
}

export const config = { runtime: "edge" };

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

  const res = await fetch("https://api.openai.com/v1/chat/completions", {
    headers: {
      "Content-Type": "application/json",
      Authorization: `Bearer ${process.env.OPENAI_API_KEY ?? ""}`,
    },
    method: "POST",
    body: JSON.stringify(payload),
  });

  const stream = new ReadableStream({
    async start(controller) {
      // callback
      function onParse(event: ParsedEvent | ReconnectInterval) {
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
            if (counter < 2 && (text.match(/\n/) || []).length) {
              // this is a prefix character (i.e., "\n\n"), do nothing
              return;
            }
            const queue = encoder.encode(text);
            controller.enqueue(queue);
            counter++;
          } catch (e) {
            // maybe parse error
            controller.error(e);
          }
        }
      }

      // stream response (SSE) from OpenAI may be fragmented into multiple chunks
      // this ensures we properly read chunks and invoke an event for each SSE event stream
      const parser = createParser(onParse);
      // https://web.dev/streams/#asynchronous-iteration
      for await (const chunk of res.body as any) {
        parser.feed(decoder.decode(chunk));
      }
    },
  });

  return stream;
}