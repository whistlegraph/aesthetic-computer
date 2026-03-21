// demoplay, 2026.3.04.17.00
// A conductor for automated AC performances — "Test Suite for Aesthetic Computer"

let dp; // Reference to system.demoplay (set in boot, used in closures)

async function boot({
  system,
  jump,
  send,
  speak,
  preloadPieces,
  params,
  colon,
  sound,
  net,
}) {
  // Load score module (default or from params)
  const scoreName = params?.[0] || "ars-2026";
  let scoreModule;
  try {
    scoreModule = await import(`./demoplay-scores/${scoreName}.mjs`);
  } catch (e) {
    console.error("🎬 Demoplay: Failed to load score:", scoreName, e);
    jump("prompt");
    return;
  }

  const { score, meta } = scoreModule;
  if (!score || score.length === 0) {
    console.error("🎬 Demoplay: Empty score");
    jump("prompt");
    return;
  }

  // Extract all piece names and speech texts from the score for preloading
  const pieceNames = new Set();
  const speechTexts = [];

  for (const movement of score) {
    for (const action of movement.actions) {
      if (action.action === "jump") pieceNames.add(action.to);
      if (action.action === "speak") {
        speechTexts.push({
          text: action.text,
          voice: action.voice || meta?.voices?.[0] || "female:18",
        });
      }
    }
  }

  // Initialize conductor state on the shared system object
  system.demoplay = {
    score,
    meta,
    running: true,
    movementIndex: 0,
    actionIndex: 0,
    card: null, // { text, opacity, fadeIn, fadeOut, startTime }
    syntheticKeyboard: [],
    eventCallback: null, // Set by wait-for actions
  };

  dp = system.demoplay;

  // Stop function (called on Escape)
  system.stopDemoplay = () => {
    console.log("🎬 Demoplay: Stopped");
    if (system.demoplay) {
      system.demoplay.running = false;
      delete system.demoplay;
    }
    delete system.stopDemoplay;
    send({ type: "url-freeze", content: { freeze: false } });
    // Reset leaving flag so jump works
    jump("prompt");
  };

  // Freeze URL during performance
  send({ type: "url-freeze", content: { freeze: true } });

  // Preload pieces
  if (pieceNames.size > 0) {
    console.log("🎬 Demoplay: Preloading pieces:", [...pieceNames]);
    await preloadPieces([...pieceNames]);
  }

  // Preload TTS
  if (speechTexts.length > 0) {
    console.log("🎬 Demoplay: Preloading", speechTexts.length, "speech samples");
    const promises = speechTexts.map(({ text, voice }) =>
      speak(text, voice, "cloud", { preloadOnly: true }),
    );
    await Promise.all(promises);
  }

  console.log(
    "🎬 Demoplay: Starting score:",
    meta?.title || scoreName,
    `(${score.length} movements)`,
  );

  // Start the performance
  advanceAction(system, jump, send, speak, sound, 0, 0);
}

function advanceAction(system, jump, send, speak, sound, mvtIdx, actIdx) {
  const dp = system.demoplay;
  if (!dp || !dp.running) return;

  const { score } = dp;

  // Movement boundary check
  if (mvtIdx >= score.length) {
    // Score complete
    console.log("🎬 Demoplay: Score complete");
    system.stopDemoplay?.();
    return;
  }

  const movement = score[mvtIdx];

  // If we've exhausted this movement's actions, show card for next movement and advance
  if (actIdx >= movement.actions.length) {
    advanceAction(system, jump, send, speak, sound, mvtIdx + 1, 0);
    return;
  }

  // Show movement card at start of each movement
  if (actIdx === 0 && movement.card) {
    const card = movement.card;
    dp.movementIndex = mvtIdx;
    dp.card = {
      text: card.text,
      opacity: 0,
      fadeIn: card.fade || 500,
      fadeOut: card.fade || 500,
      duration: card.duration || 3000,
      startTime: Date.now(),
    };
    console.log(`🎬 Movement ${mvtIdx}: "${movement.movement}" — card for ${card.duration}ms`);

    // Wait for card duration, then start actions
    setTimeout(() => {
      if (!dp.running) return;
      dp.card = null;
      executeAction(system, jump, send, speak, sound, mvtIdx, actIdx);
    }, card.duration);
    return;
  }

  executeAction(system, jump, send, speak, sound, mvtIdx, actIdx);
}

function executeAction(system, jump, send, speak, sound, mvtIdx, actIdx) {
  const dp = system.demoplay;
  if (!dp || !dp.running) return;

  const action = dp.score[mvtIdx].actions[actIdx];
  dp.movementIndex = mvtIdx;
  dp.actionIndex = actIdx;

  const next = () =>
    advanceAction(system, jump, send, speak, sound, mvtIdx, actIdx + 1);

  console.log(
    `🎬 [${mvtIdx}:${actIdx}] ${action.action}`,
    action.text || action.to || action.ms || action.key || "",
  );

  switch (action.action) {
    case "wait":
      setTimeout(next, action.ms || 1000);
      break;

    case "wait-for": {
      const timeout = action.timeout || 10000;
      const startTime = Date.now();
      dp.eventCallback = (eventName) => {
        if (eventName === action.event) {
          dp.eventCallback = null;
          next();
        }
      };
      // Timeout fallback
      setTimeout(() => {
        if (dp.eventCallback) {
          console.warn(
            `🎬 wait-for "${action.event}" timed out after ${timeout}ms`,
          );
          dp.eventCallback = null;
          next();
        }
      }, timeout);
      break;
    }

    case "jump":
      jump(action.to);
      // Small delay to let piece load
      setTimeout(next, action.delay || 300);
      break;

    case "speak":
      speak(
        action.text,
        action.voice || dp.meta?.voices?.[0] || "female:18",
        "cloud",
        { skipCompleted: false, ...action.opts },
      );
      next(); // Advance immediately — use wait-for speech:completed to sync
      break;

    case "type": {
      // Type text character by character into the prompt
      const text = action.text;
      const speed = action.speed || 80;
      let i = 0;
      const typeChar = () => {
        if (!dp.running) return;
        i++;
        send({
          type: "keyboard:text:replace",
          content: { text: text.slice(0, i), cursor: i },
        });
        if (i < text.length) {
          setTimeout(typeChar, speed);
        } else {
          next();
        }
      };
      typeChar();
      break;
    }

    case "key":
      // Inject a synthetic keyboard event
      dp.syntheticKeyboard.push({
        name: "keyboard:down:" + action.key.toLowerCase(),
        key: action.key,
        repeat: false,
        shift: action.shift || false,
        ctrl: action.ctrl || false,
        alt: action.alt || false,
      });
      setTimeout(next, action.delay || 100);
      break;

    case "keys": {
      // Sequence of timed key presses
      const seq = action.sequence || [];
      let idx = 0;
      const playKey = () => {
        if (!dp.running || idx >= seq.length) {
          next();
          return;
        }
        const k = seq[idx];
        dp.syntheticKeyboard.push({
          name: "keyboard:down:" + k.key.toLowerCase(),
          key: k.key,
          repeat: false,
          shift: k.shift || false,
          ctrl: k.ctrl || false,
          alt: k.alt || false,
        });
        idx++;
        setTimeout(playKey, k.hold || 200);
      };
      playKey();
      break;
    }

    case "robo": {
      // Play a sequence of robo pen events
      const path = action.path || [];
      let idx = 0;
      const playStep = () => {
        if (!dp.running || idx >= path.length) {
          next();
          return;
        }
        const step = path[idx];
        // Robo steps are dispatched via syntheticRobo (processed in makeFrame)
        dp.syntheticRobo = dp.syntheticRobo || [];
        dp.syntheticRobo.push(step);
        idx++;
        setTimeout(playStep, step.delay || 16); // ~60fps default
      };
      playStep();
      break;
    }

    case "card": {
      dp.card = {
        text: action.text,
        opacity: 0,
        fadeIn: action.fade || 500,
        fadeOut: action.fade || 500,
        duration: action.duration || 3000,
        startTime: Date.now(),
      };
      setTimeout(() => {
        if (dp.running) dp.card = null;
        next();
      }, action.duration || 3000);
      break;
    }

    default:
      console.warn("🎬 Unknown action:", action.action);
      next();
  }
}

export { boot };
