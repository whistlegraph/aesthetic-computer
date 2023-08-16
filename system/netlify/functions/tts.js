// TTS, 23.08.16.14.04
// Generate speech from text using Google TTS, based on parameters.

/* #region 🏁 TODO 
  - [] Convert back to builder?
  - [] Get the sample in `wordfight` and play it back when necessary.
#endregion */

// const { builder } = require("@netlify/functions");
const textToSpeech = require("@google-cloud/text-to-speech");
import { respond } from "../../backend/http.mjs";
const dev = process.env.CONTEXT === "dev";
const gcpKey = process.env.GCP_TTS_KEY;
const gcpEmail = process.env.GCP_EMAIL;

export async function handler(event, context) {
  // A GET request to get a handle from a user `sub`.
  if (
    event.httpMethod === "GET" &&
    (event.headers["host"] === "aesthetic.computer" || dev)
  ) {
    const client = new textToSpeech.TextToSpeechClient({
      credentials: {
        private_key: gcpKey.replace(/\\n/g, "\n"),
        client_email: gcpEmail,
      },
    });

    const utterance = event.queryStringParameters.from || "aesthetic.computer";

    // console.log(decodeURIComponent(event.rawQuery.replace("from=")));

    const request = {
      input: { text: utterance },
      voice: {
        languageCode: "en-US",
        ssmlGender: "NEUTRAL",
      },
      audioConfig: {
        audioEncoding: "MP3",
      },
    };

    // Performs the text-to-speech request
    const [response] = await client.synthesizeSpeech(request);
    const audioContent = response.audioContent;

    // TODO: Properly return the MP3 response here...
    // Return the audio content as a binary response
    return {
      statusCode: 200,
      body: audioContent.toString("base64"), // Convert to base64 for proper transfer
      headers: {
        "Content-Type": "audio/mpeg",
        "Content-Disposition": `inline; filename="response.mp3"`, // Optional: This makes browsers prompt the user to download the file
      },
      isBase64Encoded: true, // Indicates that the body content is base64 encoded
    };
  } else {
    return respond(405, { message: "Method Not Allowed" });
  }
}

// exports.handler = builder(handler);
