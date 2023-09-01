// Say, 23.09.01.01.11
//      23.08.16.14.04 (as TTS)
// Generate speech from text using Google TTS, based on parameters.

/* #region 🏁 TODO 
  - [x] Get the basic function working on vercel. 
  - [] Move over the environment variables (remove from netlify)
  - [] Get everything working locally.
  - [] Try and run in production.
#endregion */

// export default async function handler(request, response) {
//   if (request.method === "GET") {
//     const client = new TextToSpeechClient({
//       credentials: {
//         private_key: process.env.GCP_TTS_KEY.replace(/\\n/g, "\n"),
//         client_email: process.env.GCP_EMAIL,
//       },
//     });

//     const urlParams = new URL(request.url).searchParams;
//     const utterance = urlParams.get("from") || "aesthetic.computer";
//     const set = parseInt(urlParams.get("voice").split(":")[1]) || 0;
//     const gender =
//       urlParams.get("voice").split(":")[0]?.toUpperCase() || "NEUTRAL";

//     const voices = (await client.listVoices({ languageCode: "en-US" }))[0]
//       .voices;

//     const females = voices
//       .filter((v) => v.ssmlGender === "FEMALE")
//       .sort((a, b) => a.name.localeCompare(b.name));
//     const males = voices
//       .filter((v) => v.ssmlGender === "MALE")
//       .sort((a, b) => a.name.localeCompare(b.name));

//     let voice;
//     if (gender === "MALE") {
//       voice = males[set];
//     } else if (gender === "FEMALE") {
//       voice = females[set];
//     } else {
//       voice = males[1]; // Different male.
//     }

//     const ttsRequest = {
//       voice: {
//         languageCode: "en-US",
//         ...voice,
//       },
//       audioConfig: {
//         audioEncoding: "MP3",
//       },
//     };

//     let ssml = false;
//     if (utterance.indexOf("<speak>") !== -1) ssml = true;
//     ttsRequest.input = ssml ? { ssml: utterance } : { text: utterance };

//     const [gcpResponse] = await client.synthesizeSpeech(ttsRequest);
//     const audioContent = gcpResponse.audioContent;

//     response.setHeader("Content-Type", "audio/mpeg");
//     response.setHeader(
//       "Content-Disposition",
//       `inline; filename="response.mp3"`
//     );
//     response.status(200).send(audioContent.toString("base64"));
//   } else {
//     response.status(405).json({ message: "Method Not Allowed" });
//   }
// }

import { TextToSpeechClient } from "@google-cloud/text-to-speech";
export const config = { runtime: "edge" };

export default async function handler(request) {
  const urlParams = new URL(request.url).searchParams;
  const query = Object.fromEntries(urlParams);
  const cookies = request.headers.get("cookie");
  let body;
  try {
    body = await request.json();
  } catch (e) {
    body = null;
  }

  return new Response(
    JSON.stringify({
      body,
      query,
      cookies,
    }),
    {
      status: 200,
      headers: {
        "content-type": "application/json",
      },
    }
  );
}

// export const config = { runtime: "edge" };

/*
// const { builder } = require("@netlify/functions");
const textToSpeech = require("@google-cloud/text-to-speech");
// import { respond } from "../../backend/http.mjs";
const dev = process.env.CONTEXT === "dev";
const gcpKey = process.env.GCP_TTS_KEY;
const gcpEmail = process.env.GCP_EMAIL;

export default async function handler(event, context) {
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
*/
