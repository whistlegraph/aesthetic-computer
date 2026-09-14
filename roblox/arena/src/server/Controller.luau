local Controller = {}
function Controller.start()
    if game.ReplicatedStorage:FindFirstChild("ArenaState") then return game.ReplicatedStorage.ArenaState end
local Players = game:GetService("Players")
local ReplicatedStorage = game:GetService("ReplicatedStorage")
local RunService = game:GetService("RunService")
local Match = require(script.Parent.Match)
local World = require(script.Parent.World)
local GrenadeMath = require(ReplicatedStorage.GrenadeMath)
local Grenades = require(script.Parent.Grenades)
local Bot = require(script.Parent.Bot)
local Avatars = require(script.Parent.Avatars)
Players.CharacterAutoLoads=false
local bot = nil
local botLoading = false
local soloSince, soloId = nil, nil
local world = World.build()
local remote = Instance.new("RemoteEvent")
remote.Name, remote.Parent = "ArenaAction", ReplicatedStorage
local state = Instance.new("Folder")
state.Name, state.Parent = "ArenaState", ReplicatedStorage
require(script.Parent.Updates).start(state)
require(script.Parent.MediaBridge).start(world,state)
local match = Match.new()
local attacks, requests, staggered = {}, {}, {}
local function player(id) return id ~= Bot.ID and Players:GetPlayerByUserId(id) or nil end
local function body(id)
    local p = player(id)
    local c = id == Bot.ID and bot or (p and p.Character)
    return c and c:FindFirstChild("HumanoidRootPart"), c and c:FindFirstChildOfClass("Humanoid")
end
local function move(id, position, frozen, serverOwned)
    local root, humanoid = body(id)
    if not root or not humanoid then return end
    root.Anchored = false
    root.CFrame = CFrame.lookAt(position, Vector3.new(0, position.Y, 0))
    root.AssemblyLinearVelocity, root.AssemblyAngularVelocity = Vector3.zero, Vector3.zero
    if serverOwned then root:SetNetworkOwner(nil) else root:SetNetworkOwnershipAuto() end
    root.Anchored = frozen
    humanoid.WalkSpeed, humanoid.JumpPower = GrenadeMath.runSpeed, 45
end
local function alive(id)
    local root, h = body(id)
    if not root or not h or h.Health <= 0 then return false end
    local v = root.Position
    return v.Y > -8 and math.abs(v.X) < 60 and math.abs(v.Z) < 60
end
local grenades = Grenades.new(function(position, shotId)
    if match.phase ~= "Battle" then return end
    remote:FireAllClients("blast", position, shotId)
    for _, id in match.fighters do
        local target, h = body(id)
        local velocity = target and GrenadeMath.blast(position,target.Position)
        if velocity and h and h.Health>0 then
            local duration = id == Bot.ID and 0.55 or 0.45
            staggered[id] = os.clock()+duration
            h.WalkSpeed=0; h:Move(Vector3.zero)
            if id==Bot.ID then velocity*=1.5 end
            local owner=target:GetNetworkOwner()
            if owner then remote:FireClient(owner,"knockback",id,velocity,duration,match.round)
            else
                h:ChangeState(Enum.HumanoidStateType.Physics)
                target:ApplyImpulse(velocity*target.AssemblyMass)
            end
            task.delay(duration,function()
                if h.Parent and os.clock()>=(staggered[id] or 0) then
                    h.WalkSpeed=id==Bot.ID and GrenadeMath.botSpeed or GrenadeMath.runSpeed
                    if not owner and h.Health>0 then h:ChangeState(Enum.HumanoidStateType.Freefall) end
                end
            end)
        end
    end
end)
local function throw(id, now, aim, sequence, destination)
    local p=player(id)
    if now<(staggered[id] or 0) or not match:canAttack(id,now,attacks[id]) then
        if p then remote:FireClient(p,"throw-rejected",sequence) end
        return
    end
    local root,h=body(id)
    if not root or not h or h.Health<=0 or not alive(id) then return end
    aim=GrenadeMath.aim(aim)
    if not aim then return end
    attacks[id]=now
    local origin=root.Position+Vector3.new(0,2,0)+aim*3
    local _, flight=grenades:throw(origin,destination or (root.Position+aim*GrenadeMath.range),now,match.round,root.Parent,root.AssemblyLinearVelocity)
    remote:FireAllClients("thrown",id,sequence,flight)
end
local function action(p, command, packet)
    local now,id=os.clock(),p.UserId
    if now-(requests[id] or -math.huge)<0.1 then return end
    requests[id]=now
    if command=="throw" and type(packet)=="table" and type(packet.sequence)=="number"
        and packet.sequence%1==0 and packet.sequence>0 and packet.sequence<1e9 then
        throw(id,now,packet.aim,packet.sequence)
    end
end
remote.OnServerEvent:Connect(action)
local function added(p)
    local stats = Instance.new("Folder")
    stats.Name, stats.Parent = "leaderstats", p
    local wins = Instance.new("IntValue")
    wins.Name, wins.Parent = "Wins", stats
    local loading=false
    local function spawn()
        if loading or not p.Parent then return end
        loading=true
        local old=p.Character
        local c=Avatars.create(p.Name,Color3.fromRGB(65,235,235))
        c.Humanoid.DisplayName=p.DisplayName
        c:PivotTo(CFrame.new(World.lobby))
        c.Parent=workspace
        p.Character=c
        if old then old:Destroy() end
        c.Humanoid.Died:Connect(function() task.delay(3,spawn) end)
        loading=false
    end
    p.CharacterAdded:Connect(function(c)
        if not c:WaitForChild("HumanoidRootPart", 10) or p.Character ~= c or not p.Parent then return end
        -- A reset forfeits the current match, even if the new character spawns quickly.
        if table.find(match.fighters, p.UserId) and (match.phase == "Countdown" or match.phase == "Battle") then
            local other = match.fighters[1] == p.UserId and match.fighters[2] or match.fighters[1]
            match:finish((other == Bot.ID or player(other)) and other or nil, os.clock())
        end
        move(p.UserId, World.lobby, false, false)
    end)
    task.defer(spawn)
end
Players.PlayerAdded:Connect(added)
for _, p in Players:GetPlayers() do added(p) end
Players.PlayerRemoving:Connect(function(p)
    match:leave(p.UserId)
    attacks[p.UserId], requests[p.UserId], staggered[p.UserId] = nil, nil, nil
end)
local previous, accumulator = "Lobby", 0
RunService.Heartbeat:Connect(function(dt)
    grenades:tick(os.clock(), match.round)
    accumulator += dt
    if accumulator < 0.1 then return end
    accumulator = 0
    local now = os.clock()
    -- Queue membership follows physical occupancy, never a client toggle.
    for _,p in Players:GetPlayers() do
        local root,h=body(p.UserId)
        local onPad=root and h and h.Health>0 and World.onPad(world.QueuePad,root.Position)
        if onPad then match:join(p.UserId) else match:leave(p.UserId) end
    end
    if match.phase == "Lobby" and #match.queue == 1 then
        if soloId ~= match.queue[1] then soloId, soloSince = match.queue[1], now end
        if not bot and not botLoading then
            botLoading = true
            task.spawn(function()
                local ok, model = pcall(Bot.create)
                if ok then bot = model else warn("Practice bot failed to load; retrying"); task.wait(3) end
                botLoading = false
            end)
        end
        if now - soloSince >= 3 and bot and bot.Humanoid.Health > 0 then match:join(Bot.ID) end
    else
        soloId, soloSince = nil, nil
    end
    local queueSeconds=soloSince and math.max(0,math.ceil(3-(now-soloSince))) or 3
    state:SetAttribute("QueueSeconds",queueSeconds)
    world.QueuePad.Color=#match.queue>0 and Color3.fromRGB(65,235,235) or Color3.fromRGB(255,84,172)
    world.QueuePad.SurfaceGui.Status.Text=#match.queue==0 and "STEP ON TO PLAY" or (match.phase~="Lobby" and "QUEUED · NEXT ROUND" or (queueSeconds>0 and "STARTING "..queueSeconds or "LOADING DUMMY"))
    match:tick(now, alive)
    if match.phase ~= previous then
        if match.phase == "Battle" then
            if table.find(match.fighters, Bot.ID) then bot.Parent = workspace end
            for i, id in match.fighters do move(id, World.starts[i], false, true); attacks[id] = nil; staggered[id] = nil end
            for _, id in match.fighters do
                local root = body(id)
                if root then
                    root.Anchored = false
                    if id == Bot.ID then
                        local other = match.fighters[1] == Bot.ID and match.fighters[2] or match.fighters[1]
                        root:SetNetworkOwner(player(other))
                    else root:SetNetworkOwnershipAuto() end
                end
            end
        elseif match.phase == "Result" then
            grenades:clear()
            local winner = match.winner and player(match.winner)
            if winner then winner.leaderstats.Wins.Value += 1 end
            for i, id in match.fighters do
                if id ~= Bot.ID then move(id, World.lobby + Vector3.new(i * 4 - 6, 0, 0), false, false) end
            end
            if bot then bot:Destroy(); bot = nil end
        end
        previous = match.phase
    end
    if match.phase == "Battle" and table.find(match.fighters, Bot.ID) then
        local root, h = body(Bot.ID)
        local other = match.fighters[1] == Bot.ID and match.fighters[2] or match.fighters[1]
        local target = body(other)
        if root and h and target and alive(Bot.ID) and now >= (staggered[Bot.ID] or 0) then
            Bot.step(root, h, target.Position)
            if now - (attacks[Bot.ID] or -math.huge) >= 2.6 then
                throw(Bot.ID,now,target.Position-root.Position,0,target.Position)
            end
        end
    end
    state:SetAttribute("Round", match.round)
    state:SetAttribute("BotMatch", table.find(match.fighters, Bot.ID) ~= nil)
    state:SetAttribute("Phase", match.phase)
    state:SetAttribute("Seconds", math.max(0, math.ceil(match.deadline - now)))
    state:SetAttribute("QueueCount", #match.queue)
    state:SetAttribute("FighterA", match.fighters[1] or 0)
    state:SetAttribute("FighterB", match.fighters[2] or 0)
    local winner = match.winner and player(match.winner)
    state:SetAttribute("Winner", match.winner == Bot.ID and "Dummy" or (winner and winner.DisplayName or "Draw"))
    for _, p in Players:GetPlayers() do p:SetAttribute("ArenaQueued", table.find(match.queue, p.UserId) ~= nil) end
end)

return state
end
return Controller
