// TODO: Make this a netlify function.

import { Configuration, OpenAIApi } from "openai";
import { corsHeaders } from "../help.mjs";

export default async function handler(req) {
  const headers = corsHeaders(req);

  const configuration = new Configuration({
    apiKey: process.env.OPENAI_API_KEY,
  });

  const openai = new OpenAIApi(configuration);
  const submission = await req.json();

  // Decode the base64-encoded image data
  const imageData = base64ToUint8Array(submission.image.split(",")[1]);

  console.log(imageData);

  try {
    console.log('hi');

    const response = await openai.createImageVariation(submission.image, 2, "256x256");
    // return new Response(JSON.stringify({ url: response.data.data[0].url }), {
    //   status: 200,
    //   headers: { "Content-Type": "application/json", ...headers },
    // });
  } catch (error) {
    return new Response(JSON.stringify({ error: "Error varying painting" }), {
      status: 500,
      headers: { "Content-Type": "application/json", ...headers },
    });
  }
}

function base64ToUint8Array(base64) {
  const binary_string = atob(base64);
  const len = binary_string.length;
  const bytes = new Uint8Array(len);
  for (let i = 0; i < len; i++) {
    bytes[i] = binary_string.charCodeAt(i);
  }
  return bytes;
}

export const config = { runtime: "edge" };
