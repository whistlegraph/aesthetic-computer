// Gamepad Mappings Database
// Hardware-specific button/axis mappings for different controllers
// Uses CSS color names from lib/num.mjs for consistent color coding

export const GAMEPAD_MAPPINGS = {
  // 8BitDo Micro - Ultra-compact Bluetooth controller
  "8BitDo Micro gamepad": {
    vendor: "8BitDo",
    product: "Micro",
    description: "Ultra-compact keychain gamepad (72mm × 40.7mm)",
    
    // Button mapping (standard Gamepad API indices -> physical buttons)
    buttons: {
      0: { name: "A", position: "face_right", active: "lime", inactive: "darkgreen" },
      1: { name: "B", position: "face_bottom", active: "red", inactive: "maroon" },
      2: { name: "?", position: "unmapped", active: "cyan", inactive: "teal" },
      3: { name: "X", position: "face_top", active: "yellow", inactive: "olive" },
      4: { name: "Y", position: "face_left", active: "orangered", inactive: "saddlebrown" },
      5: { name: "?", position: "unmapped", active: "magenta", inactive: "indigo" },
      6: { name: "L", position: "shoulder_left", active: "orange", inactive: "sienna" },
      7: { name: "R", position: "shoulder_right", active: "dodgerblue", inactive: "midnightblue" },
      8: { name: "L2", position: "trigger_left", active: "coral", inactive: "brown" },
      9: { name: "R2", position: "trigger_right", active: "violet", inactive: "rebeccapurple" },
      10: { name: "Select", position: "center_left", active: "white", inactive: "slategray" },
      11: { name: "Start", position: "center_right", active: "springgreen", inactive: "darkslategray" },
      12: { name: "Home", position: "bottom_center", active: "deeppink", inactive: "darkslateblue" },
      13: { name: "?", position: "unmapped", active: "aqua", inactive: "steelblue" },
      14: { name: "?", position: "unmapped", active: "gold", inactive: "darkgoldenrod" },
      15: { name: "?", position: "unmapped", active: "hotpink", inactive: "crimson" },
      16: { name: "?", position: "unmapped", active: "lightcoral", inactive: "firebrick" }
    },
    
    // Axis mapping (standard Gamepad API indices -> physical controls)
    axes: {
      0: { 
        name: "D-Pad X", 
        type: "dpad", 
        direction: "horizontal", 
        active: { left: "lime", right: "chartreuse" }, 
        inactive: "darkseagreen" 
      },
      1: { 
        name: "D-Pad Y", 
        type: "dpad", 
        direction: "vertical", 
        active: { up: "greenyellow", down: "yellowgreen" }, 
        inactive: "olivedrab" 
      },
      2: { name: "?", type: "unmapped", active: "cyan", inactive: "teal" },
      3: { name: "?", type: "unmapped", active: "aqua", inactive: "steelblue" }
    },
    
    // Physical layout for diagram rendering
    layout: {
      type: "micro",
      hasAnalogSticks: false,
      width: 70,
      height: 35,
      
      dpad: {
        type: "axis",  // Uses axes instead of buttons
        x: 13,
        y: 17,
        size: 5,
        spacing: 8
      },
      
      faceButtons: {
        x: 57,
        y: 17,
        size: 4,
        spacing: 7,
        mapping: {
          top: 3,
          bottom: 1,
          left: 4,
          right: 0
        }
      },
      
      centerButtons: {
        select: { x: -6, y: 7, button: 10 },  // Relative to center
        start: { x: 2, y: 7, button: 11 },
        home: { x: 4, y: 30, button: 12 }  // Relative to center, bottom
      },
      
      shoulders: {
        left: { x: 7, y: -3, width: 13, button: 6 },
        right: { x: 50, y: -3, width: 13, button: 7 }  // x = width - 20
      },
      
      triggers: {
        left: { x: 21, y: -3, width: 4, button: 8 },
        right: { x: 45, y: -3, width: 4, button: 9 }  // x = width - 25
      }
    }
  },
  
  // Standard gamepad (Xbox/PlayStation style)
  "standard": {
    vendor: "Generic",
    product: "Standard Gamepad",
    description: "W3C Standard Gamepad mapping",
    
    buttons: {
      0: { name: "A/X", position: "face_bottom", active: "lime", inactive: "darkgreen" },
      1: { name: "B/○", position: "face_right", active: "red", inactive: "maroon" },
      2: { name: "X/□", position: "face_left", active: "dodgerblue", inactive: "navy" },
      3: { name: "Y/△", position: "face_top", active: "yellow", inactive: "darkgoldenrod" },
      4: { name: "LB/L1", position: "shoulder_left", active: "orange", inactive: "sienna" },
      5: { name: "RB/R1", position: "shoulder_right", active: "turquoise", inactive: "darkcyan" },
      6: { name: "LT/L2", position: "trigger_left", active: "coral", inactive: "brown" },
      7: { name: "RT/R2", position: "trigger_right", active: "violet", inactive: "indigo" },
      8: { name: "Select", position: "center_left", active: "white", inactive: "slategray" },
      9: { name: "Start", position: "center_right", active: "springgreen", inactive: "darkslategray" },
      10: { name: "LS", position: "stick_left_press", active: "cyan", inactive: "cadetblue" },
      11: { name: "RS", position: "stick_right_press", active: "magenta", inactive: "purple" },
      12: { name: "D-Up", position: "dpad_up", active: "chartreuse", inactive: "forestgreen" },
      13: { name: "D-Down", position: "dpad_down", active: "greenyellow", inactive: "olivedrab" },
      14: { name: "D-Left", position: "dpad_left", active: "limegreen", inactive: "seagreen" },
      15: { name: "D-Right", position: "dpad_right", active: "yellowgreen", inactive: "darkolivegreen" },
      16: { name: "Home", position: "center", active: "hotpink", inactive: "darkslateblue" }
    },
    
    axes: {
      0: { 
        name: "LS-X", 
        type: "stick", 
        stick: "left", 
        direction: "horizontal", 
        active: { left: "cyan", right: "aqua" }, 
        inactive: "dimgray" 
      },
      1: { 
        name: "LS-Y", 
        type: "stick", 
        stick: "left", 
        direction: "vertical", 
        active: { up: "deepskyblue", down: "skyblue" }, 
        inactive: "dimgray" 
      },
      2: { 
        name: "RS-X", 
        type: "stick", 
        stick: "right", 
        direction: "horizontal", 
        active: { left: "magenta", right: "orchid" }, 
        inactive: "dimgray" 
      },
      3: { 
        name: "RS-Y", 
        type: "stick", 
        stick: "right", 
        direction: "vertical", 
        active: { up: "violet", down: "plum" }, 
        inactive: "dimgray" 
      }
    },
    
    layout: {
      type: "standard",
      hasAnalogSticks: true
      // Layout details would go here
    }
  }
};

/**
 * Get the mapping for a specific gamepad by ID
 * @param {string} gamepadId - The gamepad.id string from the Gamepad API
 * @returns {object|null} - The mapping object or null if not found
 */
export function getGamepadMapping(gamepadId) {
  // Try exact match first
  if (GAMEPAD_MAPPINGS[gamepadId]) {
    return GAMEPAD_MAPPINGS[gamepadId];
  }
  
  // Try partial match (case-insensitive)
  const lowerID = gamepadId.toLowerCase();
  for (const [key, mapping] of Object.entries(GAMEPAD_MAPPINGS)) {
    if (lowerID.includes(key.toLowerCase())) {
      return mapping;
    }
  }
  
  // Default to standard mapping
  return GAMEPAD_MAPPINGS["standard"];
}

/**
 * Get button name for a specific gamepad and button index
 * @param {string} gamepadId - The gamepad.id string
 * @param {number} buttonIndex - The button index
 * @returns {string} - The button name
 */
export function getButtonName(gamepadId, buttonIndex) {
  const mapping = getGamepadMapping(gamepadId);
  return mapping.buttons[buttonIndex]?.name || `B${buttonIndex}`;
}

/**
 * Get axis name for a specific gamepad and axis index
 * @param {string} gamepadId - The gamepad.id string
 * @param {number} axisIndex - The axis index
 * @returns {string} - The axis name
 */
export function getAxisName(gamepadId, axisIndex) {
  const mapping = getGamepadMapping(gamepadId);
  return mapping.axes[axisIndex]?.name || `A${axisIndex}`;
}

/**
 * Get button colors (active/inactive) for a specific gamepad and button index
 * @param {string} gamepadId - The gamepad.id string
 * @param {number} buttonIndex - The button index
 * @returns {{active: string, inactive: string}} - Color names from CSS
 */
export function getButtonColors(gamepadId, buttonIndex) {
  const mapping = getGamepadMapping(gamepadId);
  const button = mapping.buttons[buttonIndex];
  return {
    active: button?.active || "white",
    inactive: button?.inactive || "gray"
  };
}

/**
 * Get axis colors (active/inactive) for a specific gamepad and axis index
 * @param {string} gamepadId - The gamepad.id string
 * @param {number} axisIndex - The axis index
 * @returns {{active: string, inactive: string}} - Color names from CSS
 */
export function getAxisColors(gamepadId, axisIndex) {
  const mapping = getGamepadMapping(gamepadId);
  const axis = mapping.axes[axisIndex];
  return {
    active: axis?.active || "cyan",
    inactive: axis?.inactive || "darkcyan"
  };
}

/**
 * Get all button info for a specific gamepad
 * @param {string} gamepadId - The gamepad.id string
 * @returns {object} - Button mapping object
 */
export function getButtonMapping(gamepadId) {
  const mapping = getGamepadMapping(gamepadId);
  return mapping.buttons;
}

/**
 * Get all axis info for a specific gamepad
 * @param {string} gamepadId - The gamepad.id string
 * @returns {object} - Axis mapping object
 */
export function getAxisMapping(gamepadId) {
  const mapping = getGamepadMapping(gamepadId);
  return mapping.axes;
}
