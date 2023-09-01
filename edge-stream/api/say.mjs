// Say, 23.09.01.01.11
//      23.08.16.14.04 (as TTS)
// Generate speech from text using Google TTS, based on parameters.

/* #region 🏁 TODO 
  - [🟡] Try and run in production.
  + Done
  - [x] Get the basic function working on vercel. 
    - https://vercel.com/docs/functions/edge-functions/quickstart
  - [x] Move over the environment variables (remove from netlify)
  - [x] Get everything working locally.
#endregion */

import { TextToSpeechClient } from "@google-cloud/text-to-speech";
import { corsHeaders } from "../help.mjs";

const dev = process.env.VERCEL_ENV === "development";
const gcpKey = process.env.GCP_TTS_KEY;
const gcpEmail = process.env.GCP_EMAIL;

export default async function handler(req, res) {
  const { method, query, headers } = req;

  if (method === "GET") {
    const client = new TextToSpeechClient({
      credentials: {
        private_key: gcpKey.replace(/\\n/g, "\n"),
        client_email: gcpEmail,
      },
    });

    let from = "aesthetic.computer";
    if (query.from) from = Buffer.from(query.from, 'base64').toString();
    const utterance = from || "aesthetic.computer";
    const set = parseInt(query.voice?.split(":")[1]) || 0;
    const gender = query.voice?.split(":")[0]?.toUpperCase() || "NEUTRAL";

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

    let voice;

    if (gender === "MALE") {
      voice = males[set];
    } else if (gender === "FEMALE") {
      voice = females[set];
    } else {
      voice = males[1]; // Different male.
    }

    const ttsRequest = {
      voice: {
        languageCode: "en-US",
        ...voice,
      },
      audioConfig: {
        audioEncoding: "MP3",
      },
    };

    let ssml = false;
    if (utterance.indexOf("<speak>") !== -1) ssml = true;
    ttsRequest.input = ssml ? { ssml: utterance } : { text: utterance };

    const [ttsResponse] = await client.synthesizeSpeech(ttsRequest);
    const audioContent = ttsResponse.audioContent;

    res.setHeader("Content-Type", "audio/mpeg");
    res.setHeader("Content-Disposition", 'inline; filename="response.mp3"');

    for (const [k, v] of Object.entries(corsHeaders(req))) {
       res.setHeader(k, v);
    } 

    res.status(200).send(audioContent);
  } else {
    res.status(405).json({ message: "Method Not Allowed" });
  }
}
