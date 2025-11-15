const { abs } = Math;

export class Gamepad {
  events = [];           // All events (backwards compatible)
  eventsByGamepad = [];  // Events separated by gamepad index
  deviceData = [];

  constructor() {
    window.addEventListener("gamepadconnected", (event) => {
      // console.log("🎮 Gamepad connected:", event.gamepad);
      this.deviceData[event.gamepad.index] = event.gamepad;
    });

    window.addEventListener("gamepaddisconnected", (event) => {
      // console.log("🎮 Gamepad disconnected:", event.gamepad);
      delete this.deviceData[event.gamepad.index];
    });

    setInterval(() => {
      const gamepads = navigator.getGamepads();
      gamepads.forEach((gamepad, gi) => {
        if (!gamepad) return;

        gamepad.buttons.forEach((button, bi) => {
          const prevButton = this.deviceData[gi]?.buttons[bi] || {
            pressed: false,
          };
          if (button.pressed !== prevButton.pressed) {
            const action = button.pressed ? "push" : "release";
            const name = `gamepad:${gi}:button:${bi}:${action}`;
            const event = { 
              name, 
              gamepad: gi, 
              button: bi, 
              action,
              gamepadId: gamepad.id // Include gamepad name/type
            };
            this.events.push(event);
            // Also add to per-gamepad array
            if (!this.eventsByGamepad[gi]) this.eventsByGamepad[gi] = [];
            this.eventsByGamepad[gi].push(event);
          }
        });

        const deadzone = 0.15;
        const changeThreshold = 0.05; // Increased threshold for more aggressive filtering

        gamepad.axes.forEach((value, ai) => {
          // Filter out broken axes on specific controllers
          // 8BitDo Micro (2dc8:9020) only has axes 0-1 for D-pad, axes 2+ are false positives
          const is8BitDo = gamepad.id && gamepad.id.includes("2dc8") && gamepad.id.includes("9020");
          if (is8BitDo && ai > 1) return; // Skip axes 2+ on 8BitDo Micro
          
          // Clamp axis values to valid range [-1, 1]
          value = Math.max(-1, Math.min(1, value));
          
          const prevValue = this.deviceData[gi]?.axes[ai] || 0;
          
          // Skip if value hasn't changed enough (stuck/broken/drifting axis)
          if (Math.abs(value - prevValue) < changeThreshold) return;
          
          // Apply deadzone - only create event if axis is outside deadzone
          if (Math.abs(value) > deadzone) {
            const name = `gamepad:${gi}:axis:${ai}:move`;
            const event = { 
              name, 
              gamepad: gi, 
              axis: ai, 
              value,
              gamepadId: gamepad.id // Include gamepad name/type
            };
            this.events.push(event);
            // Also add to per-gamepad array
            if (!this.eventsByGamepad[gi]) this.eventsByGamepad[gi] = [];
            this.eventsByGamepad[gi].push(event);
          } else if (Math.abs(prevValue) > deadzone) {
            // Send a zero event when returning to deadzone from active state
            const name = `gamepad:${gi}:axis:${ai}:move`;
            const event = { 
              name, 
              gamepad: gi, 
              axis: ai, 
              value: 0,
              gamepadId: gamepad.id
            };
            this.events.push(event);
            if (!this.eventsByGamepad[gi]) this.eventsByGamepad[gi] = [];
            this.eventsByGamepad[gi].push(event);
          }
        });

        this.deviceData[gi] = {
          buttons: gamepad.buttons.map((button) => ({
            pressed: button.pressed,
          })),
          axes: [...gamepad.axes],
          id: gamepad.id, // Store the gamepad ID
        };
      });
    }, 8); // Poll every 8 ms (~120 FPS)
  }

  // Clear all events (both unified and per-gamepad arrays)
  clearEvents() {
    this.events.length = 0;
    this.eventsByGamepad.forEach(arr => arr && (arr.length = 0));
  }
}
