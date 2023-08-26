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

    // const utterance = event.queryStringParameters.from || "aesthetic.computer";

    const params = new URLSearchParams(event.rawQuery);
    const utterance = params.get("from") || "aesthetic.computer";
    const set = parseInt(params.get("voice").split(":")[1]) || 0;
    const gender =
      params.get("voice").split(":")[0]?.toUpperCase() || "NEUTRAL";

    const voices = (
      await client.listVoices({
        languageCode: "en-US",
      })
    )[0].voices;

    const females = voices
      .filter((v) => v.ssmlGender === "FEMALE")
      .sort((a, b) => {
        return a.name.localeCompare(b.name);
      });

    const males = voices
      .filter((v) => v.ssmlGender === "MALE")
      .sort((a, b) => {
        return a.name.localeCompare(b.name);
      });

    // Print all male and female voices:

    // females.forEach((f, i) => {
    //   console.log("Index:", i, "Name:", f.name);
    // });

    // males.forEach((f, i) => {
    //   console.log("Index:", i, "Name:", f.name);
    // });

    console.log("Female voices:", females.length);
    console.log("Male voices:", males.length);

    let voice;

    if (gender === "MALE") {
      voice = males[set];
    } else if (gender === "FEMALE") {
      voice = females[set];
    } else {
      voice = males[1]; // Different male.
    }

    console.log("Chosen voice:", voice, "Set:", set, "Spoken:", utterance);

    const request = {
      voice: {
        languageCode: "en-US",
        ...voice,
      },
      audioConfig: {
        audioEncoding: "MP3",
      },
    };

    // Add `input` to the request after detecting whether it is `ssml` or not.
    let ssml = false;
    if (utterance.indexOf("<speak>") !== -1) ssml = true;
    request.input = ssml ? { ssml: utterance } : { text: utterance };

    // Performs the text-to-speech request
    const [response] = await client.synthesizeSpeech(request);
    const audioContent = response.audioContent;

    // Return the audio content as a binary mp3 file.
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
