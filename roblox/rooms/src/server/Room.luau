-- A bounded room interpreter. User data never becomes executable Luau.
local Room = {}
local function number(n, low, high)
    return type(n) == "number" and n == n and n >= low and n <= high
end
local function vector(v, low, high, integer)
    if type(v) ~= "table" or #v ~= 3 then return false end
    for k in v do
        if not number(k, 1, 3) or k % 1 ~= 0 then return false end
    end
    for i = 1, 3 do
        local n = v[i]
        if not number(n, low, high) or (integer and n % 1 ~= 0) then return false end
    end
    return true
end
local function keys(value, allowed)
    if type(value) ~= "table" then return false end
    for k in value do if not table.find(allowed, k) then return false end end
    return true
end
function Room.validate(data)
    assert(keys(data, {"schemaVersion", "spawn", "objects"}) and data.schemaVersion == 1, "Unsupported room")
    assert(vector(data.spawn, -64, 64), "Invalid spawn")
    assert(type(data.objects) == "table" and #data.objects >= 1 and #data.objects <= 64, "Object limit")
    local ids, count = {}, 0
    for index, object in data.objects do
        count += 1
        assert(number(index, 1, #data.objects) and index % 1 == 0, "Invalid object array")
        assert(keys(object, {"id", "kind", "position", "size", "color", "yaw", "bounce"}), "Invalid object fields")
        assert(type(object.id) == "string" and #object.id <= 40 and string.match(object.id, "^[a-z][a-z0-9%-]*$") and not ids[object.id], "Invalid object ID")
        ids[object.id] = true
        assert(object.kind == "platform" or object.kind == "goal", "Unsupported behavior")
        assert(vector(object.position, -64, 64) and vector(object.size, 0.5, 64) and vector(object.color, 0, 255, true), "Invalid geometry")
        assert(number(object.yaw, -180, 180) and number(object.bounce, 0, 100), "Invalid behavior value")
    end
    assert(count == #data.objects, "Sparse object array")
    return data
end
function Room.build(data, offset, ownerId)
    Room.validate(data)
    local model = Instance.new("Model")
    model.Name = "WhistlegraphRoom"
    if ownerId then model:SetAttribute("CreatorUserId", ownerId) end
    local ok, failure = pcall(function()
        for _, object in data.objects do
            local part = Instance.new("Part")
            part.Name, part.Anchored = object.id, true
            part.Size = Vector3.new(table.unpack(object.size))
            part.CFrame = CFrame.new(offset + Vector3.new(table.unpack(object.position))) * CFrame.Angles(0, math.rad(object.yaw), 0)
            part.Color = Color3.fromRGB(table.unpack(object.color))
            part.TopSurface, part.BottomSurface = Enum.SurfaceType.Smooth, Enum.SurfaceType.Smooth
            part.Parent = model
            if object.bounce > 0 or object.kind == "goal" then
                local touched = setmetatable({}, {__mode = "k"})
                part.Touched:Connect(function(hit)
                    local character = hit:FindFirstAncestorOfClass("Model")
                    local humanoid = character and character:FindFirstChildOfClass("Humanoid")
                    local root = character and character:FindFirstChild("HumanoidRootPart")
                    if not humanoid or not root or humanoid.Health <= 0 then return end
                    local now = os.clock()
                    if now - (touched[character] or -1) < 0.4 then return end
                    touched[character] = now
                    if object.bounce > 0 then
                        local velocity = root.AssemblyLinearVelocity
                        root.AssemblyLinearVelocity = Vector3.new(velocity.X, object.bounce, velocity.Z)
                    end
                    if object.kind == "goal" then
                        character:SetAttribute("WhistlegraphGoal", object.id)
                        part.Material = Enum.Material.Neon
                    end
                end)
            end
        end
        local spawn = Instance.new("SpawnLocation")
        spawn.Name, spawn.Anchored, spawn.Neutral = "RoomSpawn", true, true
        spawn.Size, spawn.Transparency, spawn.CanCollide = Vector3.new(4, 1, 4), 1, false
        spawn.Position = offset + Vector3.new(table.unpack(data.spawn)) - Vector3.new(0, 3, 0)
        spawn.Parent = model
    end)
    if not ok then model:Destroy(); error(failure) end
    return model, CFrame.new(offset + Vector3.new(table.unpack(data.spawn)))
end
return Room
