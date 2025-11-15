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

        const deadzone = 0.065;

        gamepad.axes.forEach((value, ai) => {
          const prevValue = this.deviceData[gi]?.axes[ai] || 0;
          //if (abs(value) > deadzone/* && value !== prevValue*/) {
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
          //}
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
