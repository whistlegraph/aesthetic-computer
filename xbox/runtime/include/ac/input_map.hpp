#pragma once
#include <cstdint>
#include <string_view>

namespace ac::xbox {

// Values mirror Windows.Gaming.Input.GamepadButtons, kept dependency-free so
// portable game code and host-side tests can include this header.
enum GamepadButton : std::uint32_t {
  menu = 0x1, view = 0x2, a = 0x4, b = 0x8, x = 0x10, y = 0x20,
  dpad_up = 0x40, dpad_down = 0x80, dpad_left = 0x100,
  dpad_right = 0x200, left_shoulder = 0x400, right_shoulder = 0x800,
  left_thumbstick = 0x1000, right_thumbstick = 0x2000
};

constexpr std::string_view event_name(GamepadButton button, bool down) {
  switch (button) {
    case a: return down ? "keyboard:down:space" : "keyboard:up:space";
    case b: return down ? "gamepad:down:b" : "gamepad:up:b";
    case x: return down ? "gamepad:down:x" : "gamepad:up:x";
    case y: return down ? "gamepad:down:y" : "gamepad:up:y";
    case dpad_up: return down ? "keyboard:down:arrowup" : "keyboard:up:arrowup";
    case dpad_down: return down ? "keyboard:down:arrowdown" : "keyboard:up:arrowdown";
    case dpad_left: return down ? "keyboard:down:arrowleft" : "keyboard:up:arrowleft";
    case dpad_right: return down ? "keyboard:down:arrowright" : "keyboard:up:arrowright";
    case menu: return down ? "keyboard:down:enter" : "keyboard:up:enter";
    default: return down ? "gamepad:down:unknown" : "gamepad:up:unknown";
  }
}

}  // namespace ac::xbox
