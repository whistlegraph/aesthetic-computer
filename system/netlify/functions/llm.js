// LLM, 23.05.14.23.47
// An endpoint to interact with large language models.
// OpenAI w/ GPT-3

// TODO: Add a streaming response.

import { OpenAI } from "openai-streams";
import { respond } from "../../backend/http.mjs";

export async function handler(event) {
  if (event.httpMethod !== "POST")
    return respond(405, { body: "Method Not Allowed" });

  const stream = await OpenAI(
    "completions",
    {
      model: "text-davinci-003",
      prompt: "Write a sentence.\n\n",
      max_tokens: 100,
    },
    {
      // runtime: "edge",
      apiKey: process.env.OPENAI_API_KEY,
    }
  );

  //      return new Response(stream);
  console.log(stream);

  // const body = JSON.parse(event.body);
  // let prompt = body.prompt;

  // // Adding beforehand and afterwards prompts if necessary.
  // const messages = [];
  // if (body.program.before.length > 0) {
  //   prompt = `${body.program.before} - ${prompt}`;
  // }
  // // messages.push({ role: "system", content: body.program.before });
  // // messages.push({ role: "user", content: prompt });
  // if (body.program.after.length > 0) prompt += ` - ${body.program.after}`;
  // //messages.push({ role: "system", content: body.program.after });

  // // console.log(messages);
  // console.log(prompt);

  // messages.push({ role: "user", content: prompt });

  // try {

  //   const response = await openai.createCompletion({
  //     model: "text-davinci-003",
  //     prompt,
  //     temperature: 1.4,
  //     n: 2,
  //     max_tokens: 128,
  //   });

  //   return respond(200, { reply: response.data.choices[0].text });
  // } catch (error) {
  //   console.log(error);
  //   return respond(500, { body: error.toString() });
  // }
}
