// TODO:
// CamDoll
// A 3D camera dolly with user input.

// TODO: Use key repeat / e.repeat to shorten code if necessary / prevent
//       unwanted resets of state / ✨ allow disabling of state... ✨ - 24.07.03.02.50

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
      strafe = 0;    if (this.#W) forward = -this.sensitivity;
    if (this.#S) forward = this.sensitivity;
    if (this.#A) strafe = this.sensitivity;  // Flipped: A now moves right
    if (this.#D) strafe = -this.sensitivity; // Flipped: D now moves left
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
      console.log("Over zero:", this.#ANALOG.move.z);
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

    if (this.#UP) this.cam.rotX += 1;
    if (this.#DOWN) this.cam.rotX -= 1;
    if (this.#LEFT) this.cam.rotY -= 1;  // Fixed: LEFT now turns left
    if (this.#RIGHT) this.cam.rotY += 1; // Fixed: RIGHT now turns right
    
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

      // Move Forward and Backward
      if (e.is("gamepad:0:axis:1:move")) {
        if (abs(e.value) < deadzone) {
          this.#ANALOG.move.z = 0;
        } else {
          this.#ANALOG.move.z = e.value * moveDamp;
        }
      }

      // Strafe Left and Right
      if (e.is("gamepad:0:axis:0:move")) {
        if (abs(e.value) < deadzone) {
          this.#ANALOG.move.x = 0;
        } else {
          this.#ANALOG.move.x = e.value * moveDamp;
        }
      }

      // Look Up And Down
      if (e.is("gamepad:0:axis:2:move")) {
        if (abs(e.value) < deadzone) {
          this.#ANALOG.look.y = 0;
        } else {
          this.#ANALOG.look.y = -e.value * lookDamp;
          console.log(this.#ANALOG.look.y);
        }
      }

      // Look Left And Right
      if (e.is("gamepad:0:axis:3:move")) {
        if (abs(e.value) < deadzone) {
          this.#ANALOG.look.x = 0;
        } else {
          this.#ANALOG.look.x = -e.value * lookDamp;
          console.log(this.#ANALOG.look.x);
        }
      }
    }
  }
}
