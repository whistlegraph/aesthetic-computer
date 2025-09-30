// Amp, 2025.9.30
// A simple microphone amplifier with monitor - plug in and play!

/* 📝 Notes
  - Connect your microphone or instrument and hear it back through the speakers
  - Visual feedback shows your audio signal in real-time
  - Adjust gain and monitoring settings
*/

let mic,
  connectBtn,
  connected = false,
  connecting = false,
  latencyInfo = null;

const { max } = Math;

// 🥾 Boot
function boot({ ui, screen, sound: { microphone } }) {
  connectBtn = new ui.TextButton("Connect & Monitor", { center: "xy", screen });
  mic = microphone;
}

// 🎨 Paint
function paint({
  api,
  wipe,
  ink,
  screen: { width, height },
  num,
  help: { choose },
}) {
  // Background changes based on state
  wipe(
    connected
      ? [0, 100, 50] // Green when connected (monitoring)
      : connecting
      ? "gray"
      : "black"
  );

  // Draw waveform when connected
  if (connected && mic?.waveform.length > 0 && mic?.amplitude !== undefined) {
    const xStep = width / (mic.waveform.length - 1);
    const yMid = height / 2;
    const yMax = height / 2;

    // Draw waveform
    ink(0, 255, 100).poly(
      mic.waveform.map((v, i) => [i * xStep, yMid + v * yMax])
    );

    // Draw amplitude indicator at top
    const ampHeight = mic.amplitude * 50;
    ink(255, 0, 0, 128).box(0, 0, width, ampHeight);

    // Draw center line
    ink(255, 255, 0, 32).line(0, yMid, width, yMid);
    
    // Show amplitude and status text
    const ampPercent = Math.round(mic.amplitude * 100);
    ink("white").write("MONITORING", { center: "x", y: 20, size: 1 });
    ink("cyan").write(`Level: ${ampPercent}%`, { center: "x", y: 40 });
    
    // Show latency info at the bottom if available
    if (latencyInfo) {
      const y = height - 40;
      ink("yellow").write(`Latency: ${latencyInfo.total}ms`, { center: "x", y });
      ink("gray").write(`(in:${latencyInfo.input}ms proc:${latencyInfo.processing}ms out:${latencyInfo.output}ms)`, 
        { center: "x", y: y + 12 });
    }
  }

  // Button - only show when not connected
  if (!connected) {
    connectBtn.paint({ ink }, ["green", "yellow", "yellow", "green"], ["yellow", "green", "green", "yellow"]);
  }
}

// 🧮 Sim
function sim({ sound: { microphone } }) {
  mic?.poll(); // Query for updated amplitude and waveform data
}

// 🎪 Act
function act({ event: e, screen, sound: { microphone }, delay }) {
  if (e.is("reframed")) {
    connectBtn.reposition({ center: "xy", screen });
  }

  // Handle button press
  if (e.is("touch") && !connected && !connecting) {
    connectBtn.down = true;
  }

  if (e.is("lift") && connectBtn.down && !connected && !connecting) {
    // Start connection with monitoring enabled and minimal latency
    connecting = true;
    connectBtn.txt = "Connecting...";
    connectBtn.disabled = true;
    connectBtn.down = false;
    microphone.connect({ 
      monitor: true,
      echoCancellation: false, // Clean signal, no processing
      noiseSuppression: false, // Keep clean audio
      autoGainControl: false, // No automatic volume changes
      latency: 0, // Request shortest possible latency
      enablePitch: false // Disable pitch detection for lower CPU usage and latency
    });
    mic = microphone;
  }

  connectBtn.act(e); // Pass events through to the button

  // Handle microphone connection success
  if (e.is("microphone-connect:success")) {
    delay(() => {
      connecting = false;
      connected = true;
    }, 30);
  }

  // Capture audio latency info
  if (e.is("audio:latency-info")) {
    latencyInfo = e.content;
  }

  // Handle microphone connection failure
  if (e.is("microphone-connect:failure")) {
    connecting = false;
    connected = false;
    connectBtn.txt = "Connect & Monitor";
    connectBtn.disabled = false;
  }
}

// 📰 Meta
function meta() {
  return {
    title: "Amp",
    desc: "Plug in your instrument and play!",
  };
}

export { boot, paint, sim, act, meta };
