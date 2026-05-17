-- hover-loop.lua — mpv script for the cover-video preview windows.
--
--  • Play ONLY while the mouse cursor is over the window; pause the
--    moment it leaves (saves CPU when you're not looking at a clip).
--  • Ctrl+w closes EVERY preview window at once (plain Cmd/Ctrl+w in
--    mpv only closes the focused one — these are separate processes).
--
-- Wired in by preview.sh via --script=.

local function on_mouse(_, val)
  if val == nil then return end
  -- val.hover is true while the pointer is inside the video area.
  mp.set_property_bool("pause", not val.hover)
end

mp.observe_property("mouse-pos", "native", on_mouse)

-- Belt-and-suspenders: also react to focus loss (e.g. cmd-tab away).
mp.observe_property("focused", "bool", function(_, focused)
  if focused == false then mp.set_property_bool("pause", true) end
end)

mp.add_key_binding("ctrl+w", "quit-all-previews", function()
  mp.commandv("run", "pkill", "-x", "mpv")
end)
