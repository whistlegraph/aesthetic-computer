import { Box } from "./geo.mjs";
import { radians, p2 } from "./num.mjs";
const { round } = Math;

const spinnerDelayMax = 8; // Skip a few frames of the spinner.
let spinnerDelay = 0;

let TYPEFACE_UI; // A default `Typeface` instance carried over from `disk`.

function spinnerReset() {
  spinnerDelay = 0;
}

// Loading icon.
function spinner(ctx, timePassed) {
  if (spinnerDelay < spinnerDelayMax) {
    spinnerDelay += 1;
    return;
  }
  const gap = 20,
    s = 8;
  ctx.save();
  const scaledWidth = ctx.canvas.width / window.devicePixelRatio;
  ctx.translate(scaledWidth - (s + gap), s + gap);
  ctx.rotate(radians(timePassed % 360) * 1);
  ctx.beginPath();
  // \ of the X
  ctx.moveTo(-s, -s); // top left
  ctx.lineTo(s, s); // bottom right
  // / of the X
  //ctx.moveTo(-s, s); // bottom left
  //ctx.lineTo(s, -s); // top right
  ctx.strokeStyle = "rgb(255, 255, 0)";
  ctx.lineWidth = 4;
  ctx.lineCap = "round";
  ctx.stroke();
  ctx.restore();
}

// Paused icon.
function cached(ctx) {
  const gap = 4,
    s = 20;

  ctx.save();
  ctx.translate(round(gap / 2) + 6, round(gap / 2) + 4);

  // Bottom Left option.
  ctx.translate(
    0,
    round(ctx.canvas.height / window.devicePixelRatio - s * 2 + 2),
  );

  ctx.beginPath();

  ctx.moveTo(gap, gap); // left
  ctx.lineTo(gap, s);
  ctx.moveTo(gap * 3.5, gap); // right
  ctx.lineTo(gap * 3.5, s);

  ctx.strokeStyle = "rgb(0, 255, 255)";
  ctx.lineWidth = 4;
  ctx.lineCap = "round";
  ctx.stroke();

  ctx.restore();
}

// Global tracking for drag-between-buttons behavior
const activeButtons = new Set(); // Track which buttons are currently active

// Debug: Track recent rollout events to detect retap-after-rollout issues
const recentRollouts = new Map(); // buttonId -> timestamp
const ROLLOUT_MEMORY_TIME = 2000; // Remember rollouts for 2 seconds

function trackRollout(buttonId) {
  recentRollouts.set(buttonId, performance.now());
  // Clean up old entries
  for (const [id, timestamp] of recentRollouts.entries()) {
    if (performance.now() - timestamp > ROLLOUT_MEMORY_TIME) {
      recentRollouts.delete(id);
    }
  }
}

function wasRecentlyRolledOut(buttonId) {
  const rolloutTime = recentRollouts.get(buttonId);
  return rolloutTime && (performance.now() - rolloutTime < ROLLOUT_MEMORY_TIME);
}
function addActiveButton(btn, reason = "unknown", netLog = null) {
  activeButtons.add(btn);
  const logData = {
    buttonId: btn.id || "unnamed",
    reason,
    totalActive: activeButtons.size,
    down: btn.down,
    downPointer: btn.downPointer
  };
  
  console.log("➕ Button added to activeButtons:", logData);
  
  // Send to remote debugging if available
  if (netLog) {
    netLog.info("🔘 Button activated:", logData);
  }
  
  // Emergency cleanup if we have too many active buttons
  if (activeButtons.size > 5) {
    if (netLog) netLog.warn("🚨 Emergency cleanup: too many active buttons:", activeButtons.size);
    emergencyButtonCleanup("too many active buttons detected: " + activeButtons.size, netLog);
  }
}

function removeActiveButton(btn, reason = "unknown", netLog = null) {
  const wasPresent = activeButtons.has(btn);
  activeButtons.delete(btn);
  if (wasPresent) {
    const logData = {
      buttonId: btn.id || "unnamed",
      reason,
      totalActive: activeButtons.size,
      down: btn.down,
      downPointer: btn.downPointer
    };
    console.log("➖ Button removed from activeButtons:", logData);
    if (netLog) {
      netLog.info("🔘 Button deactivated:", logData);
    }
  } else {
    const logData = {
      buttonId: btn.id || "unnamed",
      reason
    };
    console.warn("⚠️ Tried to remove button not in activeButtons:", logData);
    if (netLog) {
      netLog.warn("⚠️ Button removal attempted but not found:", logData);
    }
  }
}

// Debug: Track button state inconsistencies
let debugButtonStateChecks = 0;
function debugActiveButtonsState() {
  debugButtonStateChecks++;
  if (debugButtonStateChecks % 60 === 0) { // Log every 60 calls (roughly once per second at 60fps)
    const activeButtonsArray = Array.from(activeButtons);
    const stuckButtons = activeButtonsArray.filter(btn => !btn.down);
    
    if (stuckButtons.length > 0) {
      console.warn("🐛 INCONSISTENT BUTTON STATE DETECTED:", {
        totalActive: activeButtonsArray.length,
        stuckButtons: stuckButtons.map(btn => ({
          id: btn.id || "unnamed",
          down: btn.down,
          over: btn.over,
          downPointer: btn.downPointer
        })),
        allButtons: activeButtonsArray.map(btn => ({
          id: btn.id || "unnamed",
          down: btn.down,
          over: btn.over,
          downPointer: btn.downPointer
        }))
      });
      
      // Clean up stuck buttons
      stuckButtons.forEach(btn => {
        console.log("🧹 Cleaning up stuck button:", btn.id || "unnamed");
        activeButtons.delete(btn);
      });
    }
  }
}

// Manual button state reset function (for emergency use)
function resetAllButtons(netLog = null) {
  console.log("🔄 MANUAL BUTTON RESET - clearing all active buttons");
  if (netLog) {
    netLog.info("🔄 Manual button reset initiated");
  }
  
  const buttonsToReset = Array.from(activeButtons);
  buttonsToReset.forEach(btn => {
    console.log("🔄 Resetting button:", btn.id || "unnamed");
    btn.down = false;
    btn.over = false;
    btn.downPointer = undefined;
    removeActiveButton(btn, "manual reset", netLog);
  });
}

// Emergency cleanup function for when we detect stuck buttons
function emergencyButtonCleanup(reason = "unknown", netLog = null) {
  console.warn("🚨 EMERGENCY BUTTON CLEANUP:", reason);
  if (netLog) {
    netLog.error("🚨 Emergency button cleanup triggered:", { reason });
  }
  
  const activeButtonsArray = Array.from(activeButtons);
  
  // Log the state before cleanup
  const buttonStates = activeButtonsArray.map(btn => ({
    id: btn.id || "unnamed",
    down: btn.down,
    over: btn.over,
    downPointer: btn.downPointer
  }));
  
  console.log("📊 Active buttons before cleanup:", buttonStates);
  if (netLog) {
    netLog.info("📊 Emergency cleanup button states:", buttonStates);
  }
  
  // Reset all buttons
  activeButtonsArray.forEach(btn => {
    btn.down = false;
    btn.over = false;
    btn.downPointer = undefined;
    activeButtons.delete(btn);
  });
  
  console.log("🧹 Emergency cleanup complete - all buttons reset");
  if (netLog) {
    netLog.info("🧹 Emergency cleanup completed:", { buttonsReset: activeButtonsArray.length });
  }
}

// Expose functions globally for debugging
if (typeof window !== 'undefined') {
  window.resetAllButtons = resetAllButtons;
  window.emergencyButtonCleanup = emergencyButtonCleanup;
}

// An interactive button model.
class Button {
  btn;
  box;
  down = false;
  disabled = false;
  icon;
  dom = false;
  over = false; // Keep track of rollover state.
  multitouch = true; // Toggle to false to make a single touch button2.
  downPointer; // Keep track of what original pointer downed the button.
  actions; // A held list of callbacks for virtually triggering events.
  noEdgeDetection = false; // Set to true to opt out of global edge detection cancellation
  noRolloverActivation = false; // Set to true to prevent activation via rollover from other buttons
  stickyScrubbing = false; // Set to true to prevent rollover activation when scrubbing from another button
  offScreenScrubbing = false; // Set to true to allow scrubbing to continue off-screen but still allow horizontal rollover

  get up() {
    return !this.down;
  }

  set up(value) {
    this.down = !value;
  }

  // (x, y, width, height) or Box
  constructor() {
    if (arguments.length === 1) {
      // Assume we are passing in a box {x,y,w,h} object.
      this.box = Box.from(arguments[0]);
    } else this.box = new Box(...arguments); // Otherwise: x, y, w, h for a box.
    this.btn = this;
  }

  publishToDom({ send }, label, message) {
    // The only use case for this right now is the Clipboard API. 23.06.16.15.40
    // 📓 Where `message` is used as text to be copied.
    send({
      type: "button:hitbox:add",
      content: { box: this.box, label, message },
    });
  }

  removeFromDom({ send }, label) {
    send({ type: "button:hitbox:remove", content: label });
  }

  // For using in a piece's `act` function. Contains callbacks for
  // events that take place inside the button.
  // Usage:  act(e, () => {}); // For 'push' callback only.
  //         act(e, {push: () => {}, down: () => {}, cancel: () => {}, draw() => {}});
  // You can optionally pass in an array of `pens` {x, y} for multi-touch support.
  act(e, callbacks = () => {}, pens = []) {
    const btn = this.btn;
    if (btn.disabled) {
      return;
    }

    // Extract net.log for remote debugging
    const netLog = e.net?.log;

    // Debug: Check for inconsistent button states
    debugActiveButtonsState();

    // If only a single function is sent, then assume it's a button push callback.
    if (typeof callbacks === "function") callbacks = { push: callbacks };
    btn.actions = callbacks;

    // Handle global edge detection cancellation
    if (e.is("ui:cancel-interactions") && !btn.noEdgeDetection) {
      // Only cancel if the button is still down (not already cancelled by other means)
      // For offScreenScrubbing buttons, don't cancel if cursor is horizontally within bounds
      // For stickyScrubbing buttons, always cancel when mouse leaves screen to prevent sounds getting stuck
      const shouldCancel = btn.down && (!btn.offScreenScrubbing || 
        (btn.offScreenScrubbing && (e.x < btn.box.x || e.x >= btn.box.x + btn.box.w)));
      
      if (shouldCancel) {
        console.log("🚫 Global edge detection - button cancelled:", {
          buttonId: btn.id || "unnamed",
          timestamp: performance.now(),
          reason: "ui:cancel-interactions event",
          wasDown: btn.down,
          offScreenScrubbing: btn.offScreenScrubbing,
          cursorX: e.x,
          buttonBounds: { x: btn.box.x, w: btn.box.w }
        });
        
        btn.down = false;
        btn.over = false;
        btn.downPointer = undefined;
  removeActiveButton(btn, "global edge detection", netLog); // Remove from activeButtons Set
        callbacks.cancel?.(btn);
      }
      
      // ADDITIONAL SAFETY: Even if shouldCancel is false, if this button is in activeButtons
      // but shouldn't be, clean it up to prevent corruption
      if (!shouldCancel && activeButtons.has(btn) && !btn.down) {
        console.warn("🧹 Edge detection cleanup - removing orphaned button:", {
          buttonId: btn.id || "unnamed",
          reason: "button in activeButtons but not down during edge detection"
        });
  removeActiveButton(btn, "edge detection orphan cleanup", netLog);
        btn.over = false;
        btn.downPointer = undefined;
      }
      
      return;
    }

    const t = this.multitouch ? "any" : "1";

    // 1. Down: Enable the button if we touched over it. (Repeatable)
    if (e.is(`touch:${t}`) && btn.box.contains(e) && !btn.down) {
      const wasRecentRollout = wasRecentlyRolledOut(btn.id || "unnamed");
      
      console.log("🟢 Button touch down - IMMEDIATE:", {
        buttonId: btn.id || "unnamed",
        timestamp: performance.now(),
        pointer: e.pointer,
        eventType: "touch",
        wasInActiveButtons: activeButtons.has(btn),
        currentDownPointer: btn.downPointer,
        over: btn.over,
        wasRecentlyRolledOut: wasRecentRollout,
        suspiciousRetap: wasRecentRollout && activeButtons.has(btn)
      });
      
      // SAFETY: If this button was somehow still in activeButtons but not down, clean it up first
      if (activeButtons.has(btn) && !btn.down) {
        console.warn("⚠️ CORRUPTED STATE DETECTED - button in activeButtons but not down:", {
          buttonId: btn.id || "unnamed",
          reason: "cleaning up before new touch",
          wasRecentlyRolledOut: wasRecentRollout
        });
  removeActiveButton(btn, "corrupted state cleanup before touch", netLog);
        btn.over = false;
        btn.downPointer = undefined;
      }
      
      // EXTRA SAFETY: If this is a retap after recent rollout, ensure clean state
      if (wasRecentRollout) {
        console.log("🔄 Retap after recent rollout - ensuring clean state:", {
          buttonId: btn.id || "unnamed"
        });
        btn.over = false;
        btn.downPointer = undefined;
        if (activeButtons.has(btn)) {
          removeActiveButton(btn, "retap after rollout cleanup", netLog);
        }
      }
      
      const downed = callbacks.down?.(btn);
      btn.down = downed || downed === undefined ? true : false;
      
      console.log("🔍 downPointer state before assignment:", {
        buttonId: btn.id || "unnamed",
        eventPointer: e.pointer,
        currentDownPointer: btn.downPointer,
        btnDown: btn.down,
        willSetDownPointer: btn.down && btn.downPointer === undefined
      });
      
      if (btn.down && btn.downPointer === undefined) {
        console.log("🔧 Setting downPointer on initial touch:", {
          buttonId: btn.id || "unnamed",
          eventPointer: e.pointer,
          beforeDownPointer: btn.downPointer,
          afterDownPointer: e.pointer || 0
        });
        btn.downPointer = e.pointer || 0;
      } else if (btn.down && btn.downPointer !== undefined) {
        console.warn("⚠️ Button already has downPointer - NOT updating:", {
          buttonId: btn.id || "unnamed",
          eventPointer: e.pointer,
          existingDownPointer: btn.downPointer,
          reason: "downPointer was not cleaned up properly"
        });
      }
      btn.over = btn.down;
      // Add to active buttons set
      if (btn.down) {
  addActiveButton(btn, "touch down", netLog);
      }
    }

    // 3. Push: Trigger the button if we push it.
    // Only process lift events from the controlling pointer
    const isControllingLiftPointer = !this.multitouch || btn.downPointer === e.pointer || btn.downPointer === undefined;
    if (e.is(`lift:${t}`) && btn.down && isControllingLiftPointer) {
      console.log("📤 Button lift event received:", {
        buttonId: btn.id || "unnamed",
        timestamp: performance.now(),
        pointer: e.pointer,
        downPointer: btn.downPointer,
        eventType: "lift"
      });
      
      function up() {
        const up = callbacks.up?.(btn);
        if (up === false) {
          btn.down = true;
          btn.over = true;
          console.log("🔄 Button kept down by callback:", {
            buttonId: btn.id || "unnamed",
            reason: "up() returned false"
          });
        } else {
          btn.down = false;
          btn.over = false;
          btn.downPointer = undefined;
          // Only remove if still in activeButtons (avoid double removal)
          if (activeButtons.has(btn)) {
            removeActiveButton(btn, "button up callback", netLog);
          }
          console.log("🏁 Button fully released:", {
            buttonId: btn.id || "unnamed",
            activeButtonsCount: activeButtons.size
          });
        }
      }

      // In multitouch mode, only respond to lift events from the controlling pointer
      const isControllingPointer = !this.multitouch || btn.downPointer === e.pointer || btn.downPointer === undefined;

      console.log("🔍 Pointer control analysis:", {
        buttonId: btn.id || "unnamed",
        multitouch: this.multitouch,
        eventPointer: e.pointer,
        buttonDownPointer: btn.downPointer,
        isControllingPointer,
        pensCount: pens?.length || 0
      });

      // Check if this is a valid button push or should be cancelled
      const isValidPush = isControllingPointer && (
        // Multi-touch case: all pens are outside box but lift event is inside
        (pens.length > 1 &&
          btn.box.containsNone(pens) &&
          btn.box.contains(e)) ||
        // Single touch case: lift event is inside box
        ((!pens || pens.length <= 1) && btn.box.contains(e))
      );

      console.log("📤 Button lift condition check:", {
        buttonId: btn.id || "unnamed",
        isControllingPointer,
        isValidPush,
        multitouch: this.multitouch,
        pensLength: pens?.length || 0,
        containsE: btn.box.contains(e),
        containsNonePens: btn.box.containsNone(pens || []),
        pointerMatch: btn.downPointer === e.pointer
      });

      if (isValidPush) {
        console.log("🔴 Button lift - IMMEDIATE (valid push):", {
          buttonId: btn.id || "unnamed",
          timestamp: performance.now(),
          pointer: e.pointer
        });
        
        btn.down = false;
        btn.over = false;
        // Only remove if still in activeButtons (avoid double removal from force cleanup)
        if (activeButtons.has(btn)) {
          removeActiveButton(btn, "valid push", netLog);
        }
        callbacks.push?.(btn);
        
        up();
      } else if (
        isControllingPointer && (
          btn.box.containsNone(pens) ||
          ((!pens || pens.length === 0) && !btn.box.contains(e))
        )
      ) {
        console.log("❌ Button cancelled (lift outside):", {
          buttonId: btn.id || "unnamed",
          timestamp: performance.now(),
          pointer: e.pointer,
          containsE: btn.box.contains(e),
          containsNonePens: btn.box.containsNone(pens || []),
          reason: "lifted outside button bounds"
        });
        
        btn.down = false;
        btn.over = false;
        // Only remove if still in activeButtons (avoid double removal from force cleanup)
        if (activeButtons.has(btn)) {
          removeActiveButton(btn, "cancelled - lift outside", netLog);
        }
        callbacks.cancel?.(btn);
        up();
        //console.log("Button up (cancel):", btn, pens);
      } else {
        // Check for stuck button scenario: button is down but controlling pointer doesn't match
        // Force cleanup if pointer mismatch - phantom pointers need to be cleaned up aggressively
        const isStuckButton = btn.down && !isControllingPointer;
        
        if (isStuckButton) {
          console.warn("🧹 FORCE CLEANUP - Stuck button detected:", {
            buttonId: btn.id || "unnamed",
            timestamp: performance.now(),
            eventPointer: e.pointer,
            buttonDownPointer: btn.downPointer,
            reason: "Force releasing stuck button with phantom pointer"
          });
          
          // Force cleanup the stuck button
          btn.down = false;
          btn.over = false;
          btn.downPointer = undefined;
          removeActiveButton(btn, "force cleanup - stuck button", netLog);
          callbacks.cancel?.(btn);
        } else {
          // This button is down but neither valid push nor cancel condition was met
          console.log("⚠️ Button down but lift ignored:", {
            buttonId: btn.id || "unnamed",
            timestamp: performance.now(),
            pointer: e.pointer,
            downPointer: btn.downPointer,
            isControllingPointer,
            containsE: btn.box.contains(e),
            pensLength: pens?.length || 0,
            reason: "lift event ignored - waiting for controlling pointer"
          });
        }
      }
    }

    // Note: Each piece may use the below to implement custom rolling behavior,
    //       which often differs among use cases such as pianos or general GUIs.

    // 4. Rollover: Run a rollover event if dragged on.
    // if (e.is("draw:any") && !this.down && this.box.contains(e)) {

    // For offScreenScrubbing, check if cursor is horizontally within bounds
    const horizontallyWithin =
      btn.offScreenScrubbing && e.x >= btn.box.x && e.x < btn.box.x + btn.box.w;

    if (
      e.is(`draw:${t}`) &&
      !btn.over &&
      (btn.box.contains(e) || horizontallyWithin)
    ) {
      // Check if we're dragging from another button
      // Don't just check activeButtons.size since buttons can be removed during rollout but still logically down
      // Instead, check if ANY button is down (indicating an active drag operation)
      const anyButtonDown = Array.from(activeButtons).some(activeBtn => activeBtn.down) || 
                           // Also check for buttons that might be down but not in activeButtons due to rollout
                           (e.drag && (e.drag.x !== e.x || e.drag.y !== e.y)); // Has actual drag movement
      const isDraggingFromOtherButton = anyButtonDown && !btn.down;

      console.log("🔍 Rollover detection:", {
        buttonId: btn.id || "unnamed",
        activeButtonsSize: activeButtons.size,
        anyButtonDown,
        isDraggingFromOtherButton,
        hasDragMovement: e.drag && (e.drag.x !== e.x || e.drag.y !== e.y),
        btnDown: btn.down,
        btnOver: btn.over
      });

      // Check if any active button has sticky scrubbing enabled
      const hasStickyButton = Array.from(activeButtons).some(
        (activeBtn) => activeBtn.stickyScrubbing,
      );

      // Only prevent rollover activation if:
      // 1. We're dragging from another button AND
      // 2. That other button has sticky scrubbing enabled AND
      // 3. This button doesn't allow rollover activation
      const shouldPreventRollover = isDraggingFromOtherButton && 
        hasStickyButton && 
        btn.noRolloverActivation;

      if (
        isDraggingFromOtherButton &&
        !shouldPreventRollover &&
        (btn.box.contains(e) || horizontallyWithin)
      ) {
        console.log("🎯 Button rollover activation started:", {
          buttonId: btn.id || "unnamed",
          timestamp: performance.now(),
          pointer: e.pointer,
          isDraggingFromOther: isDraggingFromOtherButton,
          shouldPreventRollover,
          horizontallyWithin,
          hasStickyButton
        });
        
        // Only allow rollover activation if conditions are met
        // In multitouch mode, only deactivate buttons that don't have their own active pointer
        for (const otherBtn of activeButtons) {
          if (otherBtn !== btn && !otherBtn.stickyScrubbing) {
            // In multitouch mode, only deactivate if this is the same pointer or if the other button doesn't have a specific pointer
            if (!this.multitouch || otherBtn.downPointer === e.pointer || otherBtn.downPointer === undefined) {
              console.log("🔄 Deactivating other button for rollover:", {
                fromButtonId: otherBtn.id || "unnamed",
                toButtonId: btn.id || "unnamed",
                pointer: e.pointer
              });
              otherBtn.down = false;
              otherBtn.over = false;
              otherBtn.actions?.up?.(otherBtn);
              removeActiveButton(otherBtn, "rollover deactivation", netLog);
            }
          }
        }

        // Only activate this button if no sticky button conflicts
        if (!hasStickyButton || btn.stickyScrubbing) {
          console.log("🎯 Activating button via rollover:", {
            buttonId: btn.id || "unnamed",
            hasStickyButton,
            stickyScrubbing: btn.stickyScrubbing
          });
          btn.down = true;
          console.log("🔧 Setting downPointer on rollover activation:", {
            buttonId: btn.id || "unnamed",
            eventPointer: e.pointer,
            beforeDownPointer: btn.downPointer,
            afterDownPointer: e.pointer || 0
          });
          btn.downPointer = e.pointer || 0;
          addActiveButton(btn, "rollover activation", netLog);
          callbacks.down?.(btn);
        }
      }

      // Always allow rollover callbacks for visual feedback
      if (
        !shouldPreventRollover ||
        horizontallyWithin
      ) {
        if (callbacks.rollover) {
          callbacks.rollover(btn);
        } else {
          callbacks.over?.(btn);
        }
        btn.over = true;
      }
    }

    // Scrub condition: allow if drag is within bounds OR if button is active from rollover
    // For sticky scrubbing, allow scrubbing even when dragging outside bounds
    // For offScreenScrubbing, allow scrubbing when horizontally within bounds even if vertically off-screen
    const containsDrag = btn.box.contains(e.drag);
    const inActiveButtons = activeButtons.has(btn);
    const horizontallyWithinForOffScreen =
      btn.offScreenScrubbing &&
      e.drag &&
      e.drag.x >= btn.box.x &&
      e.drag.x < btn.box.x + btn.box.w;
    const allowScrub =
      containsDrag ||
      (inActiveButtons && !btn.stickyScrubbing) ||
      (btn.stickyScrubbing && btn.down) ||
      (btn.offScreenScrubbing && btn.down && horizontallyWithinForOffScreen);

    if (e.is(`draw:${t}`) && btn.down && allowScrub) {
      // In multitouch mode, only allow scrubbing from the controlling pointer unless special cases apply
      const isControllingPointer = !this.multitouch || btn.downPointer === e.pointer || btn.downPointer === undefined;
      
      // Allow scrubbing if:
      // 1. This is the controlling pointer (main case), OR
      // 2. Button is active from rollover AND it's the same pointer that's dragging (preserves rollover but prevents cross-pointer contamination), OR
      // 3. Sticky scrubbing is enabled, OR  
      // 4. OffScreen scrubbing with horizontal bounds check
      if (
        isControllingPointer ||
        (inActiveButtons && !btn.stickyScrubbing && btn.downPointer === e.pointer) || // Rollover scrubbing with pointer match
        (btn.stickyScrubbing && btn.down) ||
        (btn.offScreenScrubbing && btn.down && horizontallyWithinForOffScreen)
      ) {
        callbacks.scrub?.(btn);
      }
    }

    // 5. Rollout: Run a rollout event if dragged off.
    if (
      e.is(`draw:${t}`) &&
      btn.over &&
      !btn.box.contains(e) &&
      btn.box.containsNone(pens)
    ) {
      // For offScreenScrubbing buttons, only trigger rollout if we're also outside horizontal bounds
      const shouldRollout =
        !btn.offScreenScrubbing ||
        (btn.offScreenScrubbing &&
          (e.x < btn.box.x || e.x >= btn.box.x + btn.box.w));

      // In multitouch mode, only trigger rollout if this is the pointer that controls this button
      const isControllingPointer = !this.multitouch || btn.downPointer === e.pointer || btn.downPointer === undefined;

      if (shouldRollout && isControllingPointer) {
        console.log("👻 Rollout (dragged off):", {
          buttonId: btn.id || "unnamed",
          timestamp: performance.now(),
          pointer: e.pointer,
          wasOver: btn.over,
          containsE: btn.box.contains(e),
          shouldRollout,
          reason: "dragged outside bounds"
        });
        
        // Track this rollout for detecting retap issues
        trackRollout(btn.id || "unnamed");
        
        // Call the rollout/out callbacks first
        if (callbacks.rollout) {
          console.log("🎭 Calling rollout callback:", btn.id || "unnamed");
          callbacks.rollout(btn);
        } else {
          console.log("🎭 Calling out callback:", btn.id || "unnamed");
          callbacks.out?.(btn);
        }
        
        console.log("📊 Button state after rollout callback:", {
          buttonId: btn.id || "unnamed",
          down: btn.down,
          over: btn.over,
          downPointer: btn.downPointer,
          inActiveButtons: activeButtons.has(btn)
        });
        
        // Ensure button state is properly cleaned up after rollout
        btn.over = false;
        
        // Always remove from activeButtons on rollout, regardless of down state
        // This ensures we don't get stuck buttons in the activeButtons set
        if (activeButtons.has(btn)) {
          removeActiveButton(btn, "rollout - always remove from active", netLog);
        }
        
        // If the button is still marked as down after the callback, clean up remaining state
        if (btn.down) {
          console.log("🧹 Rollout cleanup - button still down after callback:", {
            buttonId: btn.id || "unnamed",
            reason: "ensuring consistent state after rollout"
          });
          btn.down = false;
          btn.downPointer = undefined;
        }
      }
    }
    
    // Final safety check: if button is not down but still in activeButtons, remove it
    // This catches any edge cases where callbacks modify button state but don't sync with activeButtons
    if (!btn.down && activeButtons.has(btn)) {
      console.log("🛡️ Safety cleanup - button not down but in activeButtons:", {
        buttonId: btn.id || "unnamed",
        reason: "final state consistency check"
      });
  removeActiveButton(btn, "safety cleanup - button not down", netLog);
    }
  }

  // Draws a callback if the button is not disabled.
  paint(fn) {
    if (!this.disabled) fn(this);
  }

  enableIf(flag) {
    this.disabled = !flag;
  }
}

class TextButton {
  txt;
  btn;

  #gap = 4;
  #cw = 6; // Character width in pixels. Set from `typeface`.
  #g2 = this.#gap * 2;
  #h = 12 + this.#g2; // 19; //
  #offset = { x: this.#gap, y: this.#gap };

  constructor(text = "Button", pos = { x: 0, y: 0 }, typeface = TYPEFACE_UI) {
    this.#cw = typeface.blockWidth;
    this.#h = typeface.blockHeight + this.#gap * 2;

    this.txt = text;
    this.btn = new Button(this.#computePosition(text, { ...pos }));
  }

  get act() {
    return this.btn.act;
  }

  set disabled(d) {
    return (this.btn.disabled = d);
  }

  get disabled() {
    return this.btn.disabled;
  }

  get down() {
    return this.btn.down;
  }

  set down(d) {
    return (this.btn.down = d);
  }

  get noEdgeDetection() {
    return this.btn.noEdgeDetection;
  }

  set noEdgeDetection(value) {
    this.btn.noEdgeDetection = value;
  }

  get stickyScrubbing() {
    return this.btn.stickyScrubbing;
  }

  set stickyScrubbing(value) {
    this.btn.stickyScrubbing = value;
  }

  get width() {
    return this.txt.length * this.#cw + this.#gap * 2;
  }

  get height() {
    return this.#h;
  }

  // Compute position for box.
  // pos: {x, y} or { top, left } for positioning.
  // pos: {bottom, right} for bottom right...
  //      { center: "xy", screen } for screen centering.
  #computePosition(text, pos = { x: 0, y: 0 }) {
    pos = { ...pos }; // Make a shallow copy of pos because we will mutate it.
    let x, y;
    const w = text.length * this.#cw + this.#g2;
    const h = this.#h;

    if (pos.screen) {
      pos.screen.x = pos.screen.x || 0;
      pos.screen.y = pos.screen.y || 0;
    }

    if (pos.center === "xy") {
      return {
        x: pos.screen.x + (pos.x || 0) + pos.screen.width / 2 - w / 2,
        y: pos.screen.y + (pos.y || 0) + pos.screen.height / 2 - h / 2,
        w,
        h,
      };
    }

    // Position from top left if x and y are set on pos
    x = (pos.screen?.x || 0) + (pos.x || 0);
    y = (pos.screen?.y || 0) + (pos.y || 0);

    // Compute "bottom" and "right" properties if they exist.
    if (pos.bottom !== undefined) {
      y += pos.screen.height - pos.bottom - this.#h;
    } else {
      y += pos.top || 0;
    }

    if (pos.right !== undefined) {
      x += pos.screen.width - pos.right - w;
    } else {
      x += pos.left || 0;
    }

    return { x, y, w, h };
  }

  reposition(pos, txt) {
    if (txt) this.txt = txt;
    this.btn.box = Box.from(this.#computePosition(this.txt, pos));
  }

  paint(
    $,
    // scheme = [
    //   [0, 100, 0],
    //   [0, 255, 0, 150],
    //   [0, 200, 0],
    //   [0, 50, 0],
    // ],
    scheme = [0, 255, 255, 0],
    hoverScheme = [255, 0, 0, 255],
    disabledScheme = [64, 127, 127, 64],
  ) {
    let s;
    if (this.btn.disabled) {
      s = disabledScheme;
    } else {
      s = this.btn.down ? hoverScheme : scheme;
    }

    $.ink(s[0])
      .box(this.btn.box, "fill")
      .ink(s[1])
      .box(this.btn.box, "outline")
      .ink(s[2])
      .write(this.txt, p2.add(this.btn.box, this.#offset), s[3]);
  }
}

function setTypeface(tf) {
  TYPEFACE_UI = tf;
}

export { spinner, spinnerReset, cached, Button, TextButton, setTypeface };
