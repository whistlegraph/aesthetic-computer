// Say

const { stream } = require("@netlify/functions");
const tts = require("@google-cloud/text-to-speech");

const gcpEmail = process.env.GCP_EMAIL;

exports.handler = async (event) => {
  const method = event.httpMethod;
  const headers = corsHeaders(event);

  if (method === "OPTIONS") {
    return {
      statusCode: 200,
      headers,
      body: JSON.stringify({ message: "Success!" }),
    };
  } else if (method === "POST") {
    const body = JSON.parse(event.body);

    let gcpKey;
    try {
      const response = await fetch(process.env.GCP_TTS_KEY_URL);
      if (!response.ok) {
        throw new Error(`HTTP error! Status: ${response.status}`);
      }
      const json = await response.json();
      gcpKey = json.GCP_TTS_KEY;
    } catch (error) {
      console.error("Error fetching  account:", error);
      // Handle the error as needed
    }

    const client = new tts.TextToSpeechClient({
      credentials: {
        private_key: gcpKey.replace(/\\n/g, "\n"),
        client_email: gcpEmail,
      },
    });

    const utterance = body.from || "aesthetic.computer";
    const set = parseInt(body.voice?.split(":")[1]) || 0;
    const gender = body.voice?.split(":")[0]?.toUpperCase() || "NEUTRAL";

    try {
      const voices = (
        await client.listVoices({
          languageCode: "en-US",
        })
      )[0].voices;

      const females = voices
        .filter((v) => v.ssmlGender === "FEMALE")
        .sort((a, b) => a.name.localeCompare(b.name));

      const males = voices
        .filter((v) => v.ssmlGender === "MALE")
        .sort((a, b) => a.name.localeCompare(b.name));

      let voice;
      if (gender === "MALE") {
        voice = males[set];
      } else if (gender === "FEMALE") {
        voice = females[set];
      } else {
        voice = males[1];
      }

      const ttsRequest = {
        voice: { languageCode: "en-US", ...voice },
        audioConfig: { audioEncoding: "MP3" },
      };

      let ssml = false;
      if (utterance.indexOf("<speak>") !== -1) ssml = true;
      ttsRequest.input = ssml ? { ssml: utterance } : { text: utterance };

      const [ttsResponse] = await client.synthesizeSpeech(ttsRequest);

      const audioContent = ttsResponse.audioContent;
      if (!audioContent) {
        return {
          statusCode: 500,
          headers,
          body: JSON.stringify({ message: "Failed to generate audio." }),
        };
      }

      return {
        statusCode: 200,
        headers: {
          ...headers,
          "Content-Disposition": 'inline; filename="response.mp3"',
          "Content-Type": "audio/mpeg",
        },
        body: audioContent.toString("base64"),
        isBase64Encoded: true,
      };
    } catch (error) {
      console.error("TTS generation failed:", error);
      return {
        statusCode: 500,
        headers,
        body: JSON.stringify({ message: "An error has occurred." }),
      };
    }
  } else {
    return {
      statusCode: 405,
      headers,
      body: JSON.stringify({ message: "Method Not Allowed" }),
    };
  }
};

function corsHeaders(event) {
  const dev = process.env.CONTEXT === "dev";
  const production = !dev;
  let allowedOrigin = production ? "https://aesthetic.computer" : "*";
  if (event.headers.origin === "null") allowedOrigin = "*";

  return {
    "Access-Control-Allow-Methods": "GET,OPTIONS,PATCH,DELETE,POST,PUT",
    "Access-Control-Allow-Origin": allowedOrigin,
    "Access-Control-Allow-Credentials": true,
    "Access-Control-Allow-Headers":
      "X-CSRF-Token, X-Requested-With, Accept, Accept-Version, Content-Length, Content-MD5, Content-Type, Date, X-Api-Version",
  };
}
