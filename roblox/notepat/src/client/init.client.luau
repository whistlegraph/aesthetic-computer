local Players = game:GetService("Players")
local Input = game:GetService("UserInputService")
local SoundService = game:GetService("SoundService")
local ContentProvider = game:GetService("ContentProvider")
local TweenService = game:GetService("TweenService")
local storage = game:GetService("ReplicatedStorage"):WaitForChild("Notepat")
local Notes = require(storage.Notes)
local Holds = require(storage.Holds)
local holds = Holds.new()
local player = Players.LocalPlayer
local octave = 4
local page = "notepat"
local voices, pads = {}, {}
local background = Color3.fromRGB(24, 20, 30)
local foreground = Color3.fromRGB(255, 230, 250)

local function create(className, properties, parent)
    local instance = Instance.new(className)
    for key, value in pairs(properties) do instance[key] = value end
    instance.Parent = parent
    return instance
end
local gui = create("ScreenGui", {Name = "ACNotepat", ResetOnSpawn = false, ZIndexBehavior = Enum.ZIndexBehavior.Sibling}, player:WaitForChild("PlayerGui"))
local root = create("Frame", {Name = "Root", Size = UDim2.fromScale(1, 1), BackgroundColor3 = background, BorderSizePixel = 0}, gui)
local function text(className, label, parent, position, size)
    return create(className, {
        Text = label, Font = Enum.Font.Code, TextSize = 24, TextColor3 = foreground,
        BackgroundColor3 = background, BorderSizePixel = 0, Position = position, Size = size,
    }, parent)
end
local back = text("TextButton", "ac> notepat", root, UDim2.fromOffset(12, 6), UDim2.new(0.55, -12, 0, 48))
back.TextXAlignment = Enum.TextXAlignment.Left
local controls = create("Frame", {Name = "Octaves", Position = UDim2.new(1, -190, 0, 6), Size = UDim2.fromOffset(178, 48), BackgroundTransparency = 1}, root)
local lower = text("TextButton", "−", controls, UDim2.fromOffset(0, 0), UDim2.fromOffset(48, 48))
local octaveLabel = text("TextLabel", "4", controls, UDim2.fromOffset(50, 0), UDim2.fromOffset(74, 48))
local higher = text("TextButton", "+", controls, UDim2.fromOffset(126, 0), UDim2.fromOffset(48, 48))
local status = text("TextLabel", "Loading tone…", root, UDim2.new(0, 12, 1, -48), UDim2.new(1, -24, 0, 40))
status.TextSize = 18
status.TextXAlignment = Enum.TextXAlignment.Left
local instrument = create("Frame", {Name = "Pads", Position = UDim2.fromOffset(12, 66), Size = UDim2.new(1, -24, 1, -126), BackgroundTransparency = 1}, root)
local layout = create("UIGridLayout", {CellPadding = UDim2.fromOffset(8, 8), CellSize = UDim2.new(1/4, -6, 1/3, -6), SortOrder = Enum.SortOrder.LayoutOrder}, instrument)
local promptPage = create("Frame", {Name = "Prompt", Visible = false, Position = UDim2.fromOffset(12, 74), Size = UDim2.new(1, -24, 1, -140), BackgroundTransparency = 1}, root)
local command = text("TextBox", "", promptPage, UDim2.fromOffset(0, 0), UDim2.new(1, 0, 0, 64))
command.PlaceholderText = "notepat"
command.ClearTextOnFocus = false
command.TextXAlignment = Enum.TextXAlignment.Left
command.TextSize = 32
local launch = text("TextButton", "notepat →", promptPage, UDim2.fromOffset(0, 90), UDim2.new(1, 0, 0, 64))
launch.BackgroundColor3 = Color3.fromRGB(65, 36, 59)

local function releaseVoice(index)
    local voice = voices[index]
    voices[index] = nil
    if voice then
        TweenService:Create(voice, TweenInfo.new(0.08), {Volume = 0}):Play()
        task.delay(0.1, function() voice:Destroy() end)
    end
    if pads[index] then pads[index].BackgroundTransparency = 0.12 end
end
local function silence()
    for _, index in holds:clear() do releaseVoice(index) end
    -- Short one-shot samples may already have ended, but all sources reset.
end
local function noteOn(source, index)
    if page ~= "notepat" or Input:GetFocusedTextBox() then return end
    if not holds:press(source, index) then return end
    local sound = create("Sound", {
        Name = "NotepatVoice", SoundId = Notes.soundId,
        PlaybackSpeed = Notes.speed(index, octave), Volume = 0.24,
    }, SoundService)
    voices[index] = sound
    sound:Play()
    pads[index].BackgroundTransparency = 0
    status.Text = string.upper(Notes.names[index]) .. octave .. " · " .. math.floor(Notes.frequency(index, octave) + 0.5) .. " Hz"
end
local function noteOff(source)
    local index = holds:release(source)
    if index then releaseVoice(index) end
end
local function navigate(destination)
    silence()
    page = destination
    instrument.Visible = page == "notepat"
    controls.Visible = instrument.Visible
    promptPage.Visible = not instrument.Visible
    back.Text = page == "notepat" and "ac> notepat" or "aesthetic.computer>"
    status.Text = page == "notepat" and "Tap pads · letters play · [ ] octave" or "Type notepat and press Enter"
    if page == "prompt" then task.defer(function() command:CaptureFocus() end)
    else command:ReleaseFocus() end
end
local function setOctave(value)
    silence()
    octave = math.clamp(value, 2, 6)
    octaveLabel.Text = tostring(octave)
end
lower.Activated:Connect(function() setOctave(octave - 1) end)
higher.Activated:Connect(function() setOctave(octave + 1) end)
back.Activated:Connect(function() navigate(page == "notepat" and "prompt" or "notepat") end)
launch.Activated:Connect(function() navigate("notepat") end)
command.FocusLost:Connect(function(enter)
    if not enter then return end
    local destination = Notes.command(command.Text)
    if destination then navigate(destination) else status.Text = "Unknown piece. Try notepat." end
end)
for index, name in ipairs(Notes.names) do
    local rgb = Notes.colors[index]
    local pad = text("TextButton", string.upper(name) .. "\n" .. Notes.keys[index], instrument, UDim2.fromOffset(0, 0), UDim2.fromOffset(100, 100))
    pad.Name = "Pad" .. index
    pad.LayoutOrder = index
    pad.TextSize = 28
    pad.AutoButtonColor = false
    pad.BackgroundColor3 = Color3.fromRGB(rgb[1], rgb[2], rgb[3])
    pad.BackgroundTransparency = 0.12
    pad.TextColor3 = (rgb[1] * 0.299 + rgb[2] * 0.587 + rgb[3] * 0.114) > 135 and background or foreground
    create("UIStroke", {Color = Color3.fromRGB(95, 75, 92), Thickness = 1}, pad)
    pads[index] = pad
    pad.InputBegan:Connect(function(input)
        if input.UserInputType == Enum.UserInputType.Touch or input.UserInputType == Enum.UserInputType.MouseButton1 then
            noteOn(input, index)
        end
    end)
end
Input.InputBegan:Connect(function(input, processed)
    if processed or Input:GetFocusedTextBox() then return end
    if input.KeyCode == Enum.KeyCode.LeftBracket then setOctave(octave - 1); return end
    if input.KeyCode == Enum.KeyCode.RightBracket then setOctave(octave + 1); return end
    if input.KeyCode == Enum.KeyCode.Backquote then navigate(page == "notepat" and "prompt" or "notepat"); return end
    local index = Notes.index(input.KeyCode.Name)
    if index then noteOn("key:" .. input.KeyCode.Name, index) end
end)
Input.InputEnded:Connect(function(input)
    noteOff(input)
    noteOff("key:" .. input.KeyCode.Name)
end)
Input.WindowFocusReleased:Connect(silence)
Input.TextBoxFocused:Connect(silence)
gui.Destroying:Connect(silence)
-- Disable avatar input while the instrument owns the keyboard.
task.spawn(function()
    local playerModule = player:WaitForChild("PlayerScripts"):WaitForChild("PlayerModule", 10)
    if playerModule then
        local ok, module = pcall(require, playerModule)
        if ok then module:GetControls():Disable() end
    end
end)
local sample = create("Sound", {Name = "NotepatPreload", SoundId = Notes.soundId, Volume = 0}, SoundService)
task.spawn(function()
    local failed = false
    local ok = pcall(function()
        ContentProvider:PreloadAsync({sample}, function(_, result)
            if result ~= Enum.AssetFetchStatus.Success then failed = true end
        end)
    end)
    sample:Destroy()
    if not ok or failed then status.Text = "Tone unavailable on this client" elseif page == "notepat" then
        status.Text = "Tap pads · letters play · [ ] octave"
    end
end)
