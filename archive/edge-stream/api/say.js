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
  const { method, body } = req;

  for (const [k, v] of Object.entries(corsHeaders(req))) res.setHeader(k, v);

  if (method === "OPTIONS") {
    res.status(200).json({ message: "Success!" });
  } else if (method === "POST") {
    const client = new tts.TextToSpeechClient({
      credentials: {
        private_key: gcpKey.replace(/\\n/g, "\n"),
        client_email: gcpEmail,
      },
    });

    const utterance = body.from || "aesthetic.computer";
    const set = parseInt(body.voice?.split(":")[1]) || 0;
    const gender = body.voice?.split(":")[0]?.toUpperCase() || "NEUTRAL";
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
        if (!audioContent) {
          res.status(500).json(ttsResponse);
        } else {
          res.setHeader(
            "Content-Disposition",
            'inline; filename="response.mp3"',
          );
          res.setHeader("Content-Type", "audio/mpeg");
          // console.log(
          //   "Sending audio content:",
          //   audioContent.length,
          //   utterance,
          //   JSON.stringify(ttsResponse[0])
          // );
          res.status(200).send(audioContent);
        }
      })
      .catch((err) => {
        res.status(500).json({ message: "An error has occurred." });
      });
  } else {
    res.status(405).json({ message: "Method Not Allowed" });
  }
}

// See also: `help.mjs`
// TODO: Could not import like in `ask.ts` because of the JS environment.
//       23.09.01.19.11
// ⚠️ This will not work on other *.ac domains.
function corsHeaders(request) {
  const dev = process.env.VERCEL_ENV === "development";
  const production = !dev;
  let allowedOrigin = production ? "https://aesthetic.computer" : "*";
  if (request.headers.origin === "null") allowedOrigin = "*";

  return {
    "Access-Control-Allow-Methods": "GET,OPTIONS,PATCH,DELETE,POST,PUT",
    "Access-Control-Allow-Origin": allowedOrigin,
    "Access-Control-Allow-Credentials": true,
    "Access-Control-Allow-Headers":
      "X-CSRF-Token, X-Requested-With, Accept, Accept-Version, Content-Length, Content-MD5, Content-Type, Date, X-Api-Version",
  }; // Define CORS headers.
}
