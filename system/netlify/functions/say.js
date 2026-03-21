// Say - TTS API with OpenAI (default) and Google Cloud support
// Caches audio to Digital Ocean Spaces for efficiency

const OpenAI = require("openai");
const tts = require("@google-cloud/text-to-speech");
const crypto = require("crypto");
const { S3Client, HeadObjectCommand, PutObjectCommand } = require("@aws-sdk/client-s3");

// OpenAI voice mapping: alloy, echo, fable, onyx, nova, shimmer
const OPENAI_VOICES = {
  male: ["onyx", "echo", "alloy"],
  female: ["nova", "shimmer", "fable"],
  neutral: ["alloy", "fable", "echo"],
};

// Initialize S3 client for Digital Ocean Spaces
const s3 = new S3Client({
  endpoint: `https://${process.env.ART_ENDPOINT}`,
  region: "us-east-1", // DO Spaces requires a region, but it's ignored
  credentials: {
    accessKeyId: process.env.ART_KEY,
    secretAccessKey: process.env.ART_SECRET,
  },
});

const BUCKET = process.env.ART_SPACE_NAME;
const CDN_URL = "https://art.aesthetic.computer";
const CACHE_PREFIX = "tts-cache/";

// Generate cache key from provider + voice + text
function getCacheKey(provider, voiceId, text) {
  const hash = crypto.createHash("sha256").update(`${provider}:${voiceId}:${text}`).digest("hex");
  return `${CACHE_PREFIX}${hash}.mp3`;
}

// Check if cached audio exists, return CDN URL if so
async function checkCache(key) {
  try {
    await s3.send(new HeadObjectCommand({ Bucket: BUCKET, Key: key }));
    return `${CDN_URL}/${key}`;
  } catch (err) {
    if (err.name === "NotFound" || err.$metadata?.httpStatusCode === 404) {
      return null; // Not cached
    }
    console.error("Cache check error:", err);
    return null;
  }
}

// Save audio to cache
async function saveToCache(key, audioBuffer) {
  try {
    await s3.send(new PutObjectCommand({
      Bucket: BUCKET,
      Key: key,
      Body: audioBuffer,
      ContentType: "audio/mpeg",
      ACL: "public-read",
      CacheControl: "public, max-age=31536000", // 1 year (audio doesn't change)
    }));
    console.log(`✅ Cached TTS: ${CDN_URL}/${key}`);
    return `${CDN_URL}/${key}`;
  } catch (err) {
    console.error("Cache write error:", err);
    return null;
  }
}

// Generate audio with OpenAI TTS
async function generateOpenAI(text, gender, set) {
  const voiceList = OPENAI_VOICES[gender] || OPENAI_VOICES.neutral;
  const voice = voiceList[set % voiceList.length];
  
  const openai = new OpenAI({ apiKey: process.env.OPENAI_API_KEY });
  
  const mp3Response = await openai.audio.speech.create({
    model: "tts-1",
    voice: voice,
    input: text,
    response_format: "mp3",
  });

  return {
    buffer: Buffer.from(await mp3Response.arrayBuffer()),
    voiceId: `openai-${voice}`,
  };
}

// Generate audio with Google Cloud TTS
async function generateGoogle(text, gender, set, isSSML) {
  // Fetch GCP key from URL
  const response = await fetch(process.env.GCP_TTS_KEY_URL);
  if (!response.ok) {
    throw new Error(`Failed to fetch GCP key: ${response.status}`);
  }
  const json = await response.json();
  const gcpKey = json.GCP_TTS_KEY;

  const client = new tts.TextToSpeechClient({
    credentials: {
      private_key: gcpKey.replace(/\\n/g, "\n"),
      client_email: process.env.GCP_EMAIL,
    },
  });

  // Get available voices
  const voices = (await client.listVoices({ languageCode: "en-US" }))[0].voices;

  const females = voices
    .filter((v) => v.ssmlGender === "FEMALE")
    .sort((a, b) => a.name.localeCompare(b.name));

  const males = voices
    .filter((v) => v.ssmlGender === "MALE")
    .sort((a, b) => a.name.localeCompare(b.name));

  let voice;
  const genderUpper = gender.toUpperCase();
  if (genderUpper === "MALE") {
    voice = males[set % males.length];
  } else if (genderUpper === "FEMALE") {
    voice = females[set % females.length];
  } else {
    voice = males[1 % males.length];
  }

  const ttsRequest = {
    voice: { languageCode: "en-US", ...voice },
    audioConfig: { audioEncoding: "MP3" },
    input: isSSML ? { ssml: text } : { text },
  };

  const [ttsResponse] = await client.synthesizeSpeech(ttsRequest);

  return {
    buffer: ttsResponse.audioContent,
    voiceId: `google-${voice.name}`,
  };
}

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

    const utterance = body.from || "aesthetic.computer";
    const set = parseInt(body.voice?.split(":")[1]) || 0;
    const gender = body.voice?.split(":")[0]?.toLowerCase() || "neutral";
    
    // Provider: "openai" (default), "google"
    // Can be set via body.provider or defaults to openai
    const provider = body.provider || "openai";
    
    // Cache bust: if true, skip cache lookup and regenerate
    const bustCache = body.bust === true;

    // Check for SSML (only Google supports it)
    const isSSML = utterance.indexOf("<speak>") !== -1;
    
    // Strip SSML tags for OpenAI (it doesn't support them)
    let text = utterance;
    if (isSSML && provider === "openai") {
      text = text.replace(/<[^>]*>/g, "").trim();
    }

    // Build voice identifier for cache key
    const voiceSpec = `${provider}-${gender}-${set}`;
    const cacheKey = getCacheKey(provider, voiceSpec, text);

    try {
      // Check cache first - return redirect to CDN if cached (unless bust=true)
      if (!bustCache) {
        const cachedUrl = await checkCache(cacheKey);
        
        if (cachedUrl) {
          console.log(`🎯 TTS cache hit: ${cachedUrl}`);
          return {
            statusCode: 302,
            headers: {
              ...headers,
              Location: cachedUrl,
              "Cache-Control": "public, max-age=86400",
            },
            body: "",
          };
        }
      } else {
        console.log(`🧹 Cache bust requested for: ${text.substring(0, 50)}...`);
      }

      // Cache miss (or bust) - generate with selected provider
      console.log(`🔄 TTS ${bustCache ? 'regenerating' : 'cache miss'} (${provider}): ${text.substring(0, 50)}...`);
      
      let result;
      if (provider === "google") {
        result = await generateGoogle(text, gender, set, isSSML);
      } else {
        result = await generateOpenAI(text, gender, set);
      }

      const { buffer: audioBuffer, voiceId } = result;

      if (!audioBuffer || audioBuffer.length === 0) {
        return {
          statusCode: 500,
          headers,
          body: JSON.stringify({ message: "Failed to generate audio." }),
        };
      }

      console.log(`🗣️ Generated with ${provider}: ${voiceId}`);

      // Cache for next time
      const cdnUrl = await saveToCache(cacheKey, audioBuffer);
      
      if (cdnUrl) {
        return {
          statusCode: 302,
          headers: {
            ...headers,
            Location: cdnUrl,
          },
          body: "",
        };
      }

      // Fallback: return audio directly if caching failed
      return {
        statusCode: 200,
        headers: {
          ...headers,
          "Content-Disposition": 'inline; filename="response.mp3"',
          "Content-Type": "audio/mpeg",
        },
        body: audioBuffer.toString("base64"),
        isBase64Encoded: true,
      };
    } catch (error) {
      console.error("TTS generation failed:", error);
      return {
        statusCode: 500,
        headers,
        body: JSON.stringify({ message: "An error has occurred.", error: error.message }),
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
