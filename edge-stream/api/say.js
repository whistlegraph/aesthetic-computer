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

const tts = require("@google-cloud/text-to-speech");
const gcpKey = process.env.GCP_TTS_KEY;
const gcpEmail = process.env.GCP_EMAIL;

export default async function handler(req, res) {
  const { method, query, headers } = req;

  if (method === "GET") {
    const client = new tts.TextToSpeechClient({
      credentials: {
        private_key: gcpKey.replace(/\\n/g, "\n"),
        client_email: gcpEmail,
      },
    });

    let from = "aesthetic.computer";
    if (query.from) from = Buffer.from(query.from, "base64").toString();
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
      voice: { languageCode: "en-US", ...voice },
      audioConfig: { audioEncoding: "MP3" },
    };

    let ssml = false;
    if (utterance.indexOf("<speak>") !== -1) ssml = true;
    ttsRequest.input = ssml ? { ssml: utterance } : { text: utterance };

    client
      .synthesizeSpeech(ttsRequest)
      .then((ttsResponse) => {
        const audioContent = ttsResponse[0].audioContent;
        res.setHeader("Content-Type", "audio/mpeg");
        res.setHeader("Content-Disposition", 'inline; filename="response.mp3"');
        for (const [k, v] of Object.entries(corsHeaders(req))) {
          res.setHeader(k, v);
        }
        res.status(200).send(audioContent);
      })
      .catch((err) => {
        res.status(500).json({ message: "An error has occured." });
      });
  } else {
    res.status(405).json({ message: "Method Not Allowed" });
  }
}

// See also: `help.mjs`
// TODO: Could not import like in `ask.ts` because of the JS environment.
//       23.09.01.19.11
function corsHeaders(request) {
  const dev = process.env.VERCEL_ENV === "development";
  const production = !dev;
  const allowedOrigin = production ? "https://aesthetic.computer" : "*";

  return {
    "Access-Control-Allow-Methods": "GET,OPTIONS,PATCH,DELETE,POST,PUT",
    "Access-Control-Allow-Origin": allowedOrigin,
    "Access-Control-Allow-Headers":
      "X-CSRF-Token, X-Requested-With, Accept, Accept-Version, Content-Length, Content-MD5, Content-Type, Date, X-Api-Version",
  }; // Define CORS headers.
}
