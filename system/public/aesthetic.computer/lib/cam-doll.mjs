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

  // 🧗 Optional Quake-style grounded physics (enabled when opts.gravity is set).
  // When enabled, SPACE jumps only while grounded, SHIFT crouches (clamped to
  // ground, cannot dip below), WASD moves at runSpeed (walkSpeed while crouched),
  // and vertical motion is integrated with gravity + a ground clamp.
  // Conventions: speeds in world-units/second, gravity in u/s². cam.y is inverted
  // relative to world Y (positive cam.y = below origin), hence world-up = -cam.y.
  #physicsEnabled = false;
  #runSpeed = 0;
  #walkSpeed = 0;
  #jumpVelocity = 0;
  #gravity = 0;
  #groundY = 0;
  #eyeHeight = 1.5;
  #crouchEyeHeight = 0.8;
  #simHz = 120;
  #worldYVel = 0; // world-space vertical velocity, + is up
  #onGround = true;
  #crouchT = 0; // 0 standing → 1 crouched
  #moveDamp = 0.002;
  // Optional XZ rectangle bounding the solid floor. Outside these bounds the
  // floor clamp is disabled so the player falls off the edge. Format:
  // { xMin, xMax, zMin, zMax } in world units.
  #groundBounds = null;
  // When true, all physics input is ignored and the camera just free-falls.
  // External callers (e.g. a piece with a death state) set this via setFrozen().
  #frozen = false;

  // 🎥 Third-person mode. When enabled, the render camera lazily follows a
  // target point (player − forward·distance + up·height). The current render
  // position is stored in #tpCurrent and lerped toward the target each tick.
  // Physics runs on the *logical* player position; the render offset is
  // applied at the end of sim and undone at the start of the next.
  #thirdPerson = false;
  #thirdPersonDistance = 4.5;
  #thirdPersonHeight = 1.5;
  #thirdPersonFollow = 0.08; // per-tick lerp factor (≈100 ms to catch up)
  #appliedOffset = [0, 0, 0];
  #tpCurrent = null; // current render offset (cam-space), init lazy

  // 💀 Death floor clamp — when set, a frozen (dead) player lands on this
  // world Y instead of falling forever.
  #deathFloorY = null;
  #deathFloorEyeClearance = 0.3;

  constructor(Camera, Dolly, opts) {
    this.cam = new Camera(opts.fov || 80, {
      z: opts.z || 0,
      y: opts.y * -1 || -0.5,
      scale: [1, 1, 1],
    });
    this.sensitivity = opts.sensitivity || 0.00025;
    this.#dolly = new Dolly(this.cam); // moves the camera

    if (opts.gravity !== undefined) {
      this.#physicsEnabled = true;
      this.#gravity = opts.gravity;
      this.#runSpeed = opts.runSpeed ?? 10;
      this.#walkSpeed = opts.walkSpeed ?? this.#runSpeed * 0.5;
      this.#jumpVelocity = opts.jumpVelocity ?? 8;
      this.#groundY = opts.groundY ?? 0;
      this.#eyeHeight = opts.eyeHeight ?? 1.5;
      this.#crouchEyeHeight = opts.crouchEyeHeight ?? this.#eyeHeight * 0.55;
      this.#simHz = opts.simHz ?? 120;
      // Dolly steady-state speed = push / (1 - decay) = 10 * push (decay 0.9).
      // So analog gamepad moveDamp scales to hit runSpeed at full stick deflection.
      this.#moveDamp = this.#runSpeed / this.#simHz / 10;
      if (opts.groundBounds) this.#groundBounds = opts.groundBounds;
      if (typeof opts.deathFloorY === "number") this.#deathFloorY = opts.deathFloorY;
      // Snap camera onto the floor at boot.
      this.cam.y = -(this.#groundY + this.#eyeHeight);
    }
  }

  /** Clear every held input state. Called on window defocus / visibility loss
   *  so keys don't "stick" if the user alt-tabs mid-movement. */
  clearHeldKeys() {
    this.#W = this.#S = this.#A = this.#D = false;
    this.#SPACE = this.#SHIFT = false;
    this.#UP = this.#DOWN = this.#LEFT = this.#RIGHT = false;
    this.#BTN_LOOK_UP = this.#BTN_LOOK_DOWN = false;
    this.#BTN_LOOK_LEFT = this.#BTN_LOOK_RIGHT = false;
    this.#ANALOG.move.x = this.#ANALOG.move.y = this.#ANALOG.move.z = 0;
    this.#ANALOG.look.x = this.#ANALOG.look.y = this.#ANALOG.look.z = 0;
  }

  /** Teleport the camera back to standing on the floor at the configured
   *  ground position, clearing velocity. Used for respawn. */
  respawn(worldX = 0, worldZ = 0) {
    this.cam.x = -worldX;
    this.cam.z = -worldZ;
    this.cam.y = -(this.#groundY + this.#eyeHeight);
    this.#worldYVel = 0;
    this.#onGround = true;
    this.#frozen = false;
    // Kill any residual momentum in the dolly so we don't slide on spawn.
    this.#dolly.xVel = this.#dolly.yVel = this.#dolly.zVel = 0;
    this.clearHeldKeys();
  }

  /** When frozen the physics pass is skipped entirely (e.g. during a death
   *  animation). Player input is already zeroed via clearHeldKeys. */
  setFrozen(v) {
    this.#frozen = !!v;
    if (v) this.clearHeldKeys();
  }

  /** Enable / disable third-person view. On entry the render offset is
   *  re-initialised so the camera snaps into position; on exit it instantly
   *  returns to first person. */
  setThirdPerson(v, distance, height) {
    this.#thirdPerson = !!v;
    if (typeof distance === "number") this.#thirdPersonDistance = distance;
    if (typeof height === "number") this.#thirdPersonHeight = height;
    if (!this.#thirdPerson) this.#tpCurrent = null;
  }

  /** Flip between 1P and 3P. Use for single-tap / middle-mouse bindings. */
  toggleThirdPerson() {
    this.setThirdPerson(!this.#thirdPerson);
  }

  /** True if third-person mode is currently active. */
  get thirdPerson() { return this.#thirdPerson; }

  /** Live telemetry for HUDs / debug panels. */
  get physics() {
    if (!this.#physicsEnabled) return null;
    return {
      fov: this.cam.fov,
      runSpeed: this.#runSpeed,
      walkSpeed: this.#walkSpeed,
      jumpVelocity: this.#jumpVelocity,
      gravity: this.#gravity,
      onGround: this.#onGround,
      crouch: this.#crouchT,
      worldYVel: this.#worldYVel,
      thirdPerson: this.#thirdPerson,
      // Logical player position in the camera's negated-world coordinate
      // system (useful for anchoring body parts at the player, not at the
      // offset render camera).
      playerCamX: this.cam.x - this.#appliedOffset[0],
      playerCamY: this.cam.y - this.#appliedOffset[1],
      playerCamZ: this.cam.z - this.#appliedOffset[2],
    };
  }

  sim() {
    // 🎥 Undo last frame's third-person offset so physics runs on the logical
    // player position. We'll reapply a fresh offset at the end of this tick.
    if (this.#appliedOffset[0] !== 0 || this.#appliedOffset[1] !== 0 || this.#appliedOffset[2] !== 0) {
      this.cam.x -= this.#appliedOffset[0];
      this.cam.y -= this.#appliedOffset[1];
      this.cam.z -= this.#appliedOffset[2];
      this.#appliedOffset[0] = this.#appliedOffset[1] = this.#appliedOffset[2] = 0;
    }

    // 🔫 FPS style camera movement.
    let forward = 0,
      updown = 0,
      strafe = 0;

    if (this.#physicsEnabled) {
      // Quake-style: scale push so Dolly's 0.9-decay momentum settles at the
      // requested run/walk speed. Steady-state vel/frame = push/(1-decay),
      // and we want vel/frame = speed/simHz, so push = speed/(simHz*10).
      const speed = this.#SHIFT ? this.#walkSpeed : this.#runSpeed;
      const push = speed / this.#simHz / 10;
      if (this.#W) forward = -push;
      if (this.#S) forward = push;
      if (this.#A) strafe = push;
      if (this.#D) strafe = -push;
      // SPACE and SHIFT no longer push Y — handled in the physics pass below.
      if (this.#W || this.#S || this.#A || this.#D) {
        this.#dolly.push({ x: strafe, y: 0, z: forward });
      }
    } else {
      if (this.#W) forward = -this.sensitivity;
      if (this.#S) forward = this.sensitivity;
      if (this.#A) strafe = this.sensitivity; // A moves left (positive in rotated space)
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

    // 🌍 Grounded physics pass — only when opted-in via opts.gravity.
    if (this.#physicsEnabled) {
      const dt = 1 / this.#simHz;

      // Jump: SPACE while grounded. Holding SPACE re-jumps on landing.
      if (!this.#frozen && this.#SPACE && this.#onGround) {
        this.#worldYVel = this.#jumpVelocity;
        this.#onGround = false;
      }

      // Gravity + vertical integration. Always runs, even when frozen, so a
      // dead player still falls into the pit at a constant rate.
      if (!this.#onGround || this.#frozen) {
        this.#worldYVel -= this.#gravity * dt;
        // cam.y is inverted from world Y (see constructor): world-up is -cam.y,
        // so a positive worldYVel (up) decreases cam.y.
        this.cam.y -= this.#worldYVel * dt;
      }

      // Crouch lerp (shift toggles between stand and crouch eye height).
      const crouchTarget = (!this.#frozen && this.#SHIFT) ? 1 : 0;
      this.#crouchT += (crouchTarget - this.#crouchT) * 0.25;
      if (this.#crouchT < 0.0005 && crouchTarget === 0) this.#crouchT = 0;
      if (this.#crouchT > 0.9995 && crouchTarget === 1) this.#crouchT = 1;
      const effEyeHeight =
        this.#eyeHeight +
        (this.#crouchEyeHeight - this.#eyeHeight) * this.#crouchT;

      // Only floor-clamp when the player is within the solid ground bounds.
      // Outside the rectangle, gravity continues uninterrupted → pit fall.
      let onSolidGround = true;
      if (this.#groundBounds) {
        const px = -this.cam.x;
        const pz = -this.cam.z;
        const b = this.#groundBounds;
        onSolidGround =
          px >= b.xMin && px <= b.xMax && pz >= b.zMin && pz <= b.zMax;
      }

      const floorCamY = -(this.#groundY + effEyeHeight);
      if (onSolidGround && !this.#frozen) {
        if (this.cam.y >= floorCamY) {
          this.cam.y = floorCamY;
          if (this.#worldYVel < 0) this.#worldYVel = 0;
          this.#onGround = true;
        } else if (this.#onGround) {
          // If we're above the floor but onGround was set (e.g., eye height
          // just shrank because of crouch release), keep the eye glued to the
          // ground by snapping cam.y — no falling from a crouch-release.
          this.cam.y = floorCamY;
        }
      } else {
        // Over the edge (or frozen/dead): never grounded, just keep falling.
        this.#onGround = false;
      }

      // 💀 Death floor clamp — once frozen and the eye reaches the lava,
      // stop falling so the player lies on top of the pit instead of
      // descending forever.
      if (this.#frozen && this.#deathFloorY !== null) {
        const lavaCamY = -(this.#deathFloorY + this.#deathFloorEyeClearance);
        if (this.cam.y >= lavaCamY) {
          this.cam.y = lavaCamY;
          this.#worldYVel = 0;
        }
      }
    }

    // 🎥 Third-person lazy-follow. Compute the target offset (behind + above
    // the player along the current look direction) and lerp the applied
    // offset toward it. The offset is added to cam.x/y/z for rendering only
    // and is undone at the start of the next tick.
    if (this.#thirdPerson) {
      const rx = this.cam.rotX * Math.PI / 180;
      const ry = this.cam.rotY * Math.PI / 180;
      const cp = Math.cos(rx);
      const fx = Math.sin(ry) * cp;
      const fy = Math.sin(rx);
      const fz = Math.cos(ry) * cp;
      const d = this.#thirdPersonDistance;
      const h = this.#thirdPersonHeight;
      // Target offset in cam-space coords.
      const targetX = d * fx;
      const targetZ = d * fz;
      const targetY = d * fy - h;

      // Lazy lerp: on the first frame after enabling 3P, snap; otherwise ease.
      if (!this.#tpCurrent) {
        this.#tpCurrent = [targetX, targetY, targetZ];
      } else {
        const k = this.#thirdPersonFollow;
        this.#tpCurrent[0] += (targetX - this.#tpCurrent[0]) * k;
        this.#tpCurrent[1] += (targetY - this.#tpCurrent[1]) * k;
        this.#tpCurrent[2] += (targetZ - this.#tpCurrent[2]) * k;
      }
      this.#appliedOffset[0] = this.#tpCurrent[0];
      this.#appliedOffset[1] = this.#tpCurrent[1];
      this.#appliedOffset[2] = this.#tpCurrent[2];
      this.cam.x += this.#appliedOffset[0];
      this.cam.y += this.#appliedOffset[1];
      this.cam.z += this.#appliedOffset[2];
    }
  }

  // TODO: Also add touch controls here.
  act(e) {
    // 🔇 Window lost focus — drop every held input so keys don't "stick" when
    // the user alt-tabs / switches apps mid-movement. Same for visibility loss.
    if (e.is("defocus")) {
      this.clearHeldKeys();
      return;
    }

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
      const moveDamp = this.#moveDamp;
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
        // Axis 3: Look Up/Down (right stick vertical) → controls pitch (rotX)
        if (e.is("gamepad:0:axis:3:move")) {
          if (abs(e.value) < deadzone) {
            this.#ANALOG.look.x = 0;
          } else {
            this.#ANALOG.look.x = -e.value * lookDamp;
          }
        }

        // Axis 2: Look Left/Right (right stick horizontal) → controls yaw (rotY)
        if (e.is("gamepad:0:axis:2:move")) {
          if (abs(e.value) < deadzone) {
            this.#ANALOG.look.y = 0;
          } else {
            this.#ANALOG.look.y = e.value * lookDamp;
          }
        }
      }
    }
  }
}
