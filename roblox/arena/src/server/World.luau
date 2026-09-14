local World = {}
World.lobby = Vector3.new(0, 6, 128)
World.starts = {Vector3.new(-26, 6, 0), Vector3.new(26, 6, 0)}
function World.onPad(pad, position)
    local point=pad.CFrame:PointToObjectSpace(position)
    return math.abs(point.X)<=pad.Size.X/2 and math.abs(point.Z)<=pad.Size.Z/2 and point.Y>=0 and point.Y<=7
end
function World.build()
    if workspace:FindFirstChild("AestheticArena") then return workspace.AestheticArena end
    local model = Instance.new("Model")
    model.Name = "AestheticArena"
    local function part(name, size, position, color, material)
        local p = Instance.new("Part")
        p.Name, p.Size, p.Position, p.Color = name, size, position, color
        p.Anchored = true
        p.Material = material or Enum.Material.SmoothPlastic
        p.TopSurface, p.BottomSurface = Enum.SurfaceType.Smooth, Enum.SurfaceType.Smooth
        p.Parent = model
        return p
    end
    local ink = Color3.fromRGB(24, 24, 40)
    local pink = Color3.fromRGB(255, 84, 172)
    local cyan = Color3.fromRGB(65, 235, 235)
    part("Ring", Vector3.new(112, 2, 112), Vector3.new(0, 0, 0), ink)
    for _, x in {-56, 56} do
        part("RingEdge", Vector3.new(0.3, 0.2, 112), Vector3.new(x, 1.1, 0), pink, Enum.Material.Neon)
    end
    for _, z in {-56, 56} do
        part("RingEdge", Vector3.new(112, 0.2, 0.3), Vector3.new(0, 1.1, z), cyan, Enum.Material.Neon)
    end
    part("Lobby", Vector3.new(48, 2, 28), Vector3.new(0, 0, 128), Color3.fromRGB(48, 44, 70))
    for _, x in {-24, 24} do part("Rail", Vector3.new(1, 4, 28), Vector3.new(x, 3, 128), ink) end
    for _, z in {114, 142} do part("Rail", Vector3.new(48, 4, 1), Vector3.new(0, 3, z), ink) end
    local spawn = Instance.new("SpawnLocation")
    spawn.Name, spawn.Size, spawn.Position = "LobbySpawn", Vector3.new(6, 1, 6), Vector3.new(0, 1.5, 133)
    spawn.Anchored, spawn.Neutral, spawn.Duration = true, true, 0
    spawn.Color, spawn.Parent = cyan, model
    local pad = part("QueuePad", Vector3.new(10, 0.3, 6), Vector3.new(0, 1.2, 120), pink, Enum.Material.Neon)
    local padFace=Instance.new("SurfaceGui")
    padFace.Face,padFace.CanvasSize,padFace.Parent=Enum.NormalId.Top,Vector2.new(600,360),pad
    local padText=Instance.new("TextLabel")
    padText.Name,padText.Size,padText.BackgroundTransparency="Status",UDim2.fromScale(1,1),1
    padText.Text,padText.TextColor3,padText.Font,padText.TextScaled="STEP ON TO PLAY",ink,Enum.Font.Code,true
    padText.Parent=padFace
    local sign = part("Sign", Vector3.new(30, 7, 1), Vector3.new(0, 10, 142), ink)
    local face = Instance.new("SurfaceGui")
    face.Face, face.CanvasSize, face.Parent = Enum.NormalId.Front, Vector2.new(1200, 2112), sign
    local label = Instance.new("TextLabel")
    label.Size, label.BackgroundTransparency = UDim2.fromScale(1, 1), 1
    label.Text, label.TextColor3 = "AESTHETIC ARENA\n1 vs 1 · GRENADES / RING OUT", cyan
    label.Font, label.TextScaled, label.Parent = Enum.Font.GothamBold, true, face
    model.Parent = workspace
    return model
end
return World
