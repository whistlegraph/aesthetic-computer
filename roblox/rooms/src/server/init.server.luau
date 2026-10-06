local Players = game:GetService("Players")
local Http = game:GetService("HttpService")
local Room = require(script.Room)
local endpoint = "https://aesthetic.computer/api/whistlegraph-roblox"
local slots = {}
local function request(player, body)
    local response = Http:RequestAsync({
        Url = endpoint .. "?playerId=" .. tostring(player.UserId),
        Method = body and "POST" or "GET",
        Headers = { ["x-whistlegraph-bridge-key"] = Http:GetSecret("WHISTLEGRAPH_ROBLOX_BRIDGE_KEY"), ["Content-Type"] = "application/json" },
        Body = body and Http:JSONEncode(body) or nil,
    })
    assert(response.Success and #response.Body <= 65536, "Room service unavailable")
    return Http:JSONDecode(response.Body)
end
local function join(player)
    local index = 0
    while slots[index] do index += 1 end
    slots[index] = player
    local offset = Vector3.new(index * 200, 0, 0)
    local spawn = CFrame.new(offset + Vector3.new(0, 4, 20))
    local model = Room.build({schemaVersion=1, spawn={0,4,20},objects={{id="waiting",kind="platform",position={0,0,20},size={16,1,12},color={103,164,187},yaw=0,bounce=0}}}, offset, player.UserId)
    model.Parent = workspace
    local revision, acknowledged = 0, 0
    local function position(character)
        local root = character:WaitForChild("HumanoidRootPart", 10)
        if root and player.Parent then character:PivotTo(spawn) end
    end
    local added = player.CharacterAdded:Connect(position)
    if player.Character then task.spawn(position, player.Character) end
    task.spawn(function()
        while player.Parent do
            local ok, failure = pcall(function()
                local snapshot = request(player)
                assert(type(snapshot.revision) == "number" and snapshot.revision % 1 == 0 and snapshot.revision >= 1, "Invalid revision")
                if not player.Parent then return end
                if snapshot.revision > revision then
                    -- Construction is staged before the old room is removed.
                    local nextModel, nextSpawn = Room.build(snapshot.room, offset, player.UserId)
                    nextModel.Parent = workspace
                    local previous = model
                    model, spawn, revision = nextModel, nextSpawn, snapshot.revision
                    if player.Character then player.Character:PivotTo(spawn) end
                    previous:Destroy()
                    player:SetAttribute("WhistlegraphRoomRevision", revision)
                end
                if acknowledged < revision then
                    request(player,{action="applied",playerId=tostring(player.UserId),revision=revision})
                    acknowledged = revision
                end
                player:SetAttribute("WhistlegraphRoomStatus", "Applied")
            end)
            if not ok then
                player:SetAttribute("WhistlegraphRoomStatus", "Waiting for room")
                -- Keep the last good room; never log credentials or responses.
            end
            task.wait(ok and 5 or 15)
        end
        added:Disconnect(); model:Destroy(); slots[index] = nil
    end)
end
Players.PlayerAdded:Connect(join)
for _, player in Players:GetPlayers() do task.spawn(join, player) end
