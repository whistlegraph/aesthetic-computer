// TODO:
// CamDoll
// A 3D camera dolly with user input.

// TODO: Use key repeat / e.repeat to shorten code if necessary / prevent
//       unwanted resets of state / ✨ allow disabling of state... ✨ - 24.07.03.02.50

import { GAMEPAD_MAPPINGS } from "./gamepad-mappings.mjs";

const { abs } = Math;

export class CamDoll {
  cam;
  sensitivity;

  #dolly;

  // Keyboard controls.
  #W;
  #S;
  #A;
  #D;
  #SPACE;
  #SHIFT;
  #UP;
  #DOWN;
  #LEFT;
  #RIGHT;

  // Gamepad button look controls (8BitDo Micro face buttons)
  #BTN_LOOK_UP;
  #BTN_LOOK_DOWN;
  #BTN_LOOK_LEFT;
  #BTN_LOOK_RIGHT;

  // Button look velocity for smoothing
  #buttonLookVelX = 0;
  #buttonLookVelY = 0;
  #buttonLookAccel = 0.05;  // Even more subtle acceleration
  #buttonLookDecel = 0.85;
  #buttonLookMaxSpeed = 1.0; // Slower max speed

  // Stick based controls.
  #ANALOG = {
    move: {
      x: 0,
      y: 0,
      z: 0,
    },
    look: {
      x: 0,
      y: 0,
      z: 0,
    },
  };

  #penLocked = false;

  constructor(Camera, Dolly, opts) {
    this.cam = new Camera(opts.fov || 80, {
      z: opts.z || 0,
      y: opts.y * -1 || -0.5,
      scale: [1, 1, 1],
    });
    this.sensitivity = opts.sensitivity || 0.00025;
    this.#dolly = new Dolly(this.cam); // moves the camera
  }

  sim() {
    // 🔫 FPS style camera movement.
    let forward = 0,
      updown = 0,
      strafe = 0;

    if (this.#W) forward = -this.sensitivity;
    if (this.#S) forward = this.sensitivity;
    if (this.#A) strafe = this.sensitivity;  // A moves left (positive in rotated space)
    if (this.#D) strafe = -this.sensitivity; // D moves right (negative in rotated space)
    if (this.#SPACE) updown = -this.sensitivity;
    if (this.#SHIFT) updown = this.sensitivity;

    if (
      this.#W ||
      this.#S ||
      this.#A ||
      this.#D ||
      this.#SPACE ||
      this.#SHIFT
    ) {
      this.#dolly.push({ x: strafe, y: updown, z: forward });
    }

    if (abs(this.#ANALOG.move.z) > 0 || abs(this.#ANALOG.move.x) > 0) {
      // console.log("Over zero:", this.#ANALOG.move.z);
      this.#dolly.push({
        x: this.#ANALOG.move.x,
        y: 0,
        z: this.#ANALOG.move.z,
      });
    }

    if (abs(this.#ANALOG.look.x) > 0 || abs(this.#ANALOG.look.y) > 0) {
      this.cam.rotX += this.#ANALOG.look.x;
      this.cam.rotY += this.#ANALOG.look.y;
    }

    // Button-based looking with acceleration/deceleration (gamepad face buttons)
    // Apply acceleration when buttons are held
    if (this.#BTN_LOOK_UP || this.#BTN_LOOK_DOWN) {
      if (this.#BTN_LOOK_UP) this.#buttonLookVelY += this.#buttonLookAccel;
      if (this.#BTN_LOOK_DOWN) this.#buttonLookVelY -= this.#buttonLookAccel;
      
      // Clamp to max speed
      if (this.#buttonLookVelY > this.#buttonLookMaxSpeed) this.#buttonLookVelY = this.#buttonLookMaxSpeed;
      if (this.#buttonLookVelY < -this.#buttonLookMaxSpeed) this.#buttonLookVelY = -this.#buttonLookMaxSpeed;
    } else {
      // Decelerate when no button pressed
      this.#buttonLookVelY *= this.#buttonLookDecel;
      if (abs(this.#buttonLookVelY) < 0.01) this.#buttonLookVelY = 0;
    }

    if (this.#BTN_LOOK_LEFT || this.#BTN_LOOK_RIGHT) {
      if (this.#BTN_LOOK_RIGHT) this.#buttonLookVelX += this.#buttonLookAccel;
      if (this.#BTN_LOOK_LEFT) this.#buttonLookVelX -= this.#buttonLookAccel;
      
      // Clamp to max speed
      if (this.#buttonLookVelX > this.#buttonLookMaxSpeed) this.#buttonLookVelX = this.#buttonLookMaxSpeed;
      if (this.#buttonLookVelX < -this.#buttonLookMaxSpeed) this.#buttonLookVelX = -this.#buttonLookMaxSpeed;
    } else {
      // Decelerate when no button pressed
      this.#buttonLookVelX *= this.#buttonLookDecel;
      if (abs(this.#buttonLookVelX) < 0.01) this.#buttonLookVelX = 0;
    }

    // Apply button look velocity to camera
    this.cam.rotX += this.#buttonLookVelY;
    this.cam.rotY += this.#buttonLookVelX;

    // Keyboard arrow keys
    if (this.#UP) this.cam.rotX += 1;
    if (this.#DOWN) this.cam.rotX -= 1;
    if (this.#LEFT) this.cam.rotY -= 1;
    if (this.#RIGHT) this.cam.rotY += 1;
    
    // Clamp pitch to prevent looking past straight up/down (prevents gimbal lock)
    const maxPitch = 89;
    if (this.cam.rotX > maxPitch) this.cam.rotX = maxPitch;
    if (this.cam.rotX < -maxPitch) this.cam.rotX = -maxPitch;

    this.#dolly.sim();
  }

  // TODO: Also add touch controls here.
  act(e) {
    // 💻️ Keyboard: WASD, SPACE and SHIFT for movement, ARROW KEYS for looking.
    if (e.is("keyboard:down:w")) this.#W = true;
    if (e.is("keyboard:down:s")) this.#S = true;
    if (e.is("keyboard:down:a")) this.#A = true;
    if (e.is("keyboard:down:d")) this.#D = true;

    if (e.is("keyboard:up:w")) this.#W = false;
    if (e.is("keyboard:up:s")) this.#S = false;
    if (e.is("keyboard:up:a")) this.#A = false;
    if (e.is("keyboard:up:d")) this.#D = false;

    if (e.is("keyboard:down:space")) this.#SPACE = true;
    if (e.ctrl === false && e.is("keyboard:down:shift")) this.#SHIFT = true;
    if (e.is("keyboard:up:space")) this.#SPACE = false;
    if (e.is("keyboard:up:shift")) this.#SHIFT = false;

    if (e.is("keyboard:down:arrowup")) this.#UP = true;
    if (e.is("keyboard:down:arrowdown")) this.#DOWN = true;
    if (e.is("keyboard:down:arrowleft")) this.#LEFT = true;
    if (e.is("keyboard:down:arrowright")) this.#RIGHT = true;

    if (e.is("keyboard:up:arrowup")) this.#UP = false;
    if (e.is("keyboard:up:arrowdown")) this.#DOWN = false;
    if (e.is("keyboard:up:arrowleft")) this.#LEFT = false;
    if (e.is("keyboard:up:arrowright")) this.#RIGHT = false;

    if (e.is("pen:locked")) this.#penLocked = true;
    if (e.is("pen:unlocked")) this.#penLocked = false;

    // Note: Sometimes multiple mouse buttons can be held... in which case
    //       e.button only holds the original (duplicate events are not sent).
    // TODO: These rates should be adjusted on mobile.
    if (
      (!this.#penLocked && e.is("draw")) ||
      (this.#penLocked && e.is("move"))
    ) {
      if (this.cam.type === "perspective") {
        this.cam.rotX -= e.delta.y / 3.5;
        this.cam.rotY += e.delta.x / 3.5;
        
        // Clamp pitch to prevent looking past straight up/down
        const maxPitch = 89;
        if (this.cam.rotX > maxPitch) this.cam.rotX = maxPitch;
        if (this.cam.rotX < -maxPitch) this.cam.rotX = -maxPitch;
      }
    }

    // 🖖 Touch
    // Two fingers for move forward.
    if (e.is("touch:2")) this.#W = true;
    if (e.is("lift:2")) this.#W = false;

    // Three fingers for moving backward.
    if (e.is("touch:3")) this.#S = true;
    if (e.is("lift:3")) this.#S = false;

    // 🎮 Gamepad

    if (e.is("gamepad")) {
      const deadzone = 0.05;
      const moveDamp = 0.002;
      const lookDamp = 1;

      // Detect controller type
      const gamepadId = e.gamepadId || "standard";
      const is8BitDoMicro = gamepadId.includes("8BitDo Micro");
      const mapping = GAMEPAD_MAPPINGS[gamepadId] || GAMEPAD_MAPPINGS["standard"];

      // 🕹️ D-PAD MOVEMENT (axes 0-1)
      // Axis 1: Forward/Backward (up/down = W/S)
      if (e.is("gamepad:0:axis:1")) {
        if (abs(e.value) < deadzone) {
          this.#ANALOG.move.z = 0;
        } else {
          this.#ANALOG.move.z = e.value * moveDamp;
        }
      }

      // Axis 0: Strafe Left/Right (A/D)
      if (e.is("gamepad:0:axis:0")) {
        if (abs(e.value) < deadzone) {
          this.#ANALOG.move.x = 0;
        } else {
          // For 8BitDo Micro: left=-1 should move left (negative x), right=+1 should move right (positive x)
          this.#ANALOG.move.x = -e.value * moveDamp; // Negated to fix direction
        }
      }

      // 🎮 Button-based looking (for controllers without right analog stick like 8BitDo Micro)
      // Check for button events
      if (e.button !== undefined && e.action) {
        const btn = e.button;
        const isPressed = e.action === "push";
        
        // For 8BitDo Micro: use face buttons for looking
        if (is8BitDoMicro) {
          if (btn === 0) this.#BTN_LOOK_RIGHT = isPressed;  // A (right)
          if (btn === 1) this.#BTN_LOOK_DOWN = isPressed;   // B (bottom)
          if (btn === 3) this.#BTN_LOOK_UP = isPressed;     // X (top)
          if (btn === 4) this.#BTN_LOOK_LEFT = isPressed;   // Y (left)
        }
      }

      // 🕹️ ANALOG STICK LOOKING (for standard controllers with right analog stick)
      if (!is8BitDoMicro) {
        // Axis 3: Look Up/Down (right stick vertical)
        if (e.is("gamepad:0:axis:3:move")) {
          if (abs(e.value) < deadzone) {
            this.#ANALOG.look.y = 0;
          } else {
            this.#ANALOG.look.y = -e.value * lookDamp;
          }
        }

        // Axis 2: Look Left/Right (right stick horizontal)
        if (e.is("gamepad:0:axis:2:move")) {
          if (abs(e.value) < deadzone) {
            this.#ANALOG.look.x = 0;
          } else {
            this.#ANALOG.look.x = -e.value * lookDamp;
          }
        }
      }
    }
  }
}
