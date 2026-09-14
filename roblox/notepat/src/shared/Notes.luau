-- Note names, QWERTY mapping and colors from AC disks/notepat.mjs.
local Notes = {}
Notes.names = {"c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"}
Notes.keys = {"c", "v", "d", "s", "e", "f", "w", "g", "r", "a", "q", "b"}
Notes.colors = {
    {255, 0, 0}, {255, 192, 203}, {255, 165, 0}, {255, 255, 255},
    {255, 255, 0}, {0, 128, 0}, {0, 0, 0}, {0, 0, 255},
    {128, 128, 128}, {128, 0, 128}, {210, 180, 140}, {165, 42, 42},
}
-- Ships with the installed Roblox client. A short G4-like tone, not a synth.
Notes.soundId = "rbxasset://sounds/volume_slider.ogg"
Notes.sampleFrequency = 392
function Notes.index(key)
    for index, value in ipairs(Notes.keys) do
        if value == string.lower(key) then return index end
    end
    return nil
end
function Notes.frequency(index, octave)
    assert(type(index) == "number" and index % 1 == 0 and index >= 1 and index <= 12, "invalid note")
    assert(type(octave) == "number" and octave % 1 == 0 and octave >= 2 and octave <= 6, "invalid octave")
    local midi = (octave + 1) * 12 + index - 1
    return 440 * 2 ^ ((midi - 69) / 12)
end
function Notes.speed(index, octave)
    return Notes.frequency(index, octave) / Notes.sampleFrequency
end
function Notes.command(text)
    local command = string.lower(string.match(text, "^%s*(.-)%s*$"))
    if command == "notepat" or command == "prompt" then return command end
    return nil
end
return Notes
