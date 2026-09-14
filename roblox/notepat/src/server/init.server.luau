-- A safe seat behind the full-screen instrument. No remotes or user data.
local Players = game:GetService("Players")
local floor = Instance.new("Part")
floor.Name = "NotepatFloor"
floor.Anchored = true
floor.Size = Vector3.new(64, 1, 64)
floor.Position = Vector3.new(0, -1, 0)
floor.Color = Color3.fromRGB(24, 20, 30)
floor.Parent = workspace
local spawn = Instance.new("SpawnLocation")
spawn.Name = "NotepatSpawn"
spawn.Anchored = true
spawn.Size = Vector3.new(6, 1, 6)
spawn.Transparency = 1
spawn.Neutral = true
spawn.Parent = workspace
local function seated(character)
    local root = character:WaitForChild("HumanoidRootPart", 10)
    if root then root.Anchored = true end
end
local function playerAdded(player)
    player.CharacterAdded:Connect(seated)
    if player.Character then task.spawn(seated, player.Character) end
end
Players.PlayerAdded:Connect(playerAdded)
for _, player in Players:GetPlayers() do playerAdded(player) end
