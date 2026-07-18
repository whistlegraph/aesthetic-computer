#include "ac/input_map.hpp"
#include "ac/runtime.hpp"
#include <cassert>

int main() {
  using namespace ac::xbox;
  static_assert(event_name(dpad_left, true) == "keyboard:down:arrowleft");
  static_assert(event_name(a, false) == "keyboard:up:space");
  Event e{"keyboard:down:arrowup", 1, 42};
  assert(e.is("keyboard:down:arrowup"));
  GamepadState pad;
  pad.down.insert("a");
  assert(pad.pressed("a"));
}
