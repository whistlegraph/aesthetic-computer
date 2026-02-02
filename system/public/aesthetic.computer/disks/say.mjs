// Say, 2026.02.02
// A simple test piece for the TTS API.
// Type a word or phrase after `say` to hear it spoken.

/* #region 📚 README 
  Usage: say hello
         say how are you today
         say:male hi there
         say:female good morning
         say:google hello (use Google TTS)
         say:google:female hi there
#endregion */

let text = "";
let lastSpoken = "";
let status = "idle"; // idle, speaking, error
let provider = "openai"; // "openai" or "google"
let gender = "neutral";

// 🥾 Boot
function boot({ params, colon }) {
  // Get text from URL params (e.g., `say hello world`)
  if (params.length > 0) {
    text = params.join(" ");
  }
  
  // Parse colon options (e.g., `say:google:male hello`)
  // colon can be a string or array depending on format
  if (colon) {
    const parts = Array.isArray(colon) ? colon : [colon];
    for (const part of parts) {
      if (part === "google") provider = "google";
      else if (part === "openai") provider = "openai";
      else if (part === "male") gender = "male";
      else if (part === "female") gender = "female";
    }
    console.log(`Provider: ${provider}, Gender: ${gender}`);
  }
}

// 🎨 Paint
function paint({ wipe, ink, write, screen }) {
  wipe(32, 16, 64); // Dark purple background
  
  // Note: Top-left corner is reserved for prompt HUD label
  
  // Provider indicator (below HUD area)
  const providerColor = provider === "google" ? "cyan" : "lime";
  ink(providerColor).write(`[${provider}]`, { x: 6, y: 18 });
  
  // Instructions
  ink("gray").write("say <words>", { x: 6, y: 32 });
  ink("gray").write("say:google or say:male", { x: 6, y: 44 });
  
  // Current text
  if (text) {
    ink("yellow").write(`"${text}"`, { center: "xy" });
  } else {
    ink("gray").write("(no text)", { center: "xy" });
  }
  
  // Status
  const statusY = screen.height - 14;
  if (status === "speaking") {
    ink("lime").write("Speaking...", { x: 6, y: statusY });
  } else if (status === "error") {
    ink("red").write("Error!", { x: 6, y: statusY });
  } else if (lastSpoken) {
    ink("green").write(`✓ "${lastSpoken}"`, { x: 6, y: statusY });
  } else {
    ink("gray").write("Tap to speak", { x: 6, y: statusY });
  }
}

// 🎪 Act
function act({ event: e, speak }) {
  // Speak on every tap (sampler style - can overlap!)
  if (e.is("touch") && text) {
    status = "speaking";
    
    const voice = `${gender}:0`;
    
    console.log(`🗣️ Speaking: "${text}" with ${provider}, voice: ${voice}`);
    speak(text, voice, "cloud", {
      volume: 1,
      provider: provider,
    });
  }
  
  // Handle speech completion (just for status display)
  if (e.is("speech:completed")) {
    status = "idle";
    lastSpoken = text;
  }
}

// 📰 Meta
function meta() {
  return {
    title: "Say",
    desc: "Test the text-to-speech API.",
  };
}

export { boot, paint, act, meta };
