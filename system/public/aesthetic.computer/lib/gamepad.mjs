const { abs } = Math;

export class Gamepad {
  events = [];
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
            this.events.push({ 
              name, 
              gamepad: gi, 
              button: bi, 
              action,
              gamepadId: gamepad.id // Include gamepad name/type
            });
          }
        });

        const deadzone = 0.065;

        gamepad.axes.forEach((value, ai) => {
          const prevValue = this.deviceData[gi]?.axes[ai] || 0;
          //if (abs(value) > deadzone/* && value !== prevValue*/) {
          const name = `gamepad:${gi}:axis:${ai}:move`;
          this.events.push({ 
            name, 
            gamepad: gi, 
            axis: ai, 
            value,
            gamepadId: gamepad.id // Include gamepad name/type
          });
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
}
