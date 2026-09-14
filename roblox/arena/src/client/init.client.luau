local Players = game:GetService("Players")
local CAS = game:GetService("ContextActionService")
local TweenService = game:GetService("TweenService")
local SoundService = game:GetService("SoundService")
local Debris = game:GetService("Debris")
local player = Players.LocalPlayer
local remote = game.ReplicatedStorage:WaitForChild("ArenaAction")
local state = game.ReplicatedStorage:WaitForChild("ArenaState")
local UI = require(game.ReplicatedStorage:WaitForChild("ACUI"))
local ShoveAnimation = require(game.ReplicatedStorage:WaitForChild("ShoveAnimation"))
local GrenadeMath=require(game.ReplicatedStorage:WaitForChild("GrenadeMath"))
local BlastFX=require(game.ReplicatedStorage:WaitForChild("BlastFX"))
local gui = Instance.new("ScreenGui")
gui.Name, gui.ResetOnSpawn, gui.Parent = "ArenaHUD", false, player:WaitForChild("PlayerGui")
local title = UI.bitmapLabel(gui, {Size = UDim2.new(0.44,0,0,56), Position = UDim2.new(0.23,0,0,0), BackgroundTransparency = 0.12})
local hint = UI.label(gui, {Size=UDim2.new(0.9,0,0,42),Position=UDim2.new(0.05,0,1,-142),BackgroundTransparency=1,TextColor3=Color3.new(1,1,1),TextSize=18})
local update = UI.label(gui, {Size=UDim2.new(0.9,0,0,44),Position=UDim2.new(0.05,0,0,130),TextColor3=UI.colors.pink,TextSize=16,Visible=false,Text="Update available · finish your round, then leave and join a fresh server"})
local function button(text, x, color)
    local b = Instance.new("TextButton")
    b.Size,b.Position = UDim2.new(0.38,0,0,58),UDim2.new(x,0,1,-94)
    b.Text,b.BackgroundColor3,b.BorderSizePixel = "",color,0
    b.Parent=gui
    local label=UI.bitmapLabel(b,{Size=UDim2.fromScale(1,1),BackgroundTransparency=1,TextColor3=UI.colors.ink,Text=text})
    return b,label
end
local queueButton,queueLabel=button("STEP ON PAD",0.09,UI.colors.cyan)
local nadeButton,nadeLabel=button("NADE F",0.53,UI.colors.pink)
local sounds = {swing="action_jump.mp3",hit="impact_explosion_03.mp3",tick="volume_slider.ogg",win="action_jump.mp3"}
local function sound(name, pitch)
    local s=Instance.new("Sound")
    s.SoundId="rbxasset://sounds/"..sounds[name]
    s.Volume,s.PlaybackSpeed,s.Parent=name=="hit" and 0.5 or 0.85,pitch or 1,SoundService
    s:Play()
    task.delay(0.15,function() print("AC_ARENA_AUDIO",name,"loaded",s.IsLoaded,"playing",s.IsPlaying) end)
    Debris:AddItem(s,3)
end
local cooldown, feedbackUntil = 0, 0
local lastPhase,lastSecond=nil,nil
local function isFighter() return state:GetAttribute("FighterA")==player.UserId or state:GetAttribute("FighterB")==player.UserId end
local function feedback(text)
    feedbackUntil=os.clock()+0.8
    hint.Text=text
end
local sequence=0
local predicted={}
local flights={}
local function removeFlight(key)
    local f=flights[key]
    if f then f.part:Destroy(); flights[key]=nil end
end
local function flightVisual(key, data, existing)
    local part=existing or Instance.new("Part")
    part.Name,part.Shape,part.Size="GrenadeVisual",Enum.PartType.Ball,Vector3.new(1.6,1.6,1.6)
    part.Anchored,part.CanCollide,part.CanTouch,part.CanQuery=true,false,false,false
    part.Material,part.Color=Enum.Material.Neon,Color3.fromRGB(255,175,40)
    local t=math.max(0,workspace:GetServerTimeNow()-data.started)
    local position=GrenadeMath.position(data.origin,data.velocity,data.gravity,t)
    data.offset=existing and (part.Position-position) or Vector3.zero
    data.corrected=workspace:GetServerTimeNow()
    part.Position=existing and part.Position or position
    part.Parent=workspace
    data.part=part
    flights[key]=data
end
game:GetService("RunService"):BindToRenderStep("ArenaGrenades",Enum.RenderPriority.Last.Value,function()
    local remaining=math.max(0,cooldown-os.clock())
    local label=remaining>0 and string.format("NADE %.1fs",math.ceil(remaining*10)/10) or "NADE F"
    if nadeLabel.Text~=label then nadeLabel.Text=label end
    local now=workspace:GetServerTimeNow()
    for key,f in flights do
        local t=math.max(0,now-f.started)
        if t>GrenadeMath.fuse+0.3 or state:GetAttribute("Phase")=="Result" or state:GetAttribute("Phase")=="Lobby" then removeFlight(key)
        else f.part.Position=GrenadeMath.position(f.origin,f.velocity,f.gravity,math.min(t,GrenadeMath.fuse))+f.offset*math.exp(-25*(now-f.corrected)) end
    end
end)
local function throw()
    if state:GetAttribute("Phase")~="Battle" or not isFighter() then feedback("Grenades unlock when the countdown ends"); return end
    if os.clock()<cooldown then return end
    local root=player.Character and player.Character:FindFirstChild("HumanoidRootPart")
    if not root then return end
    local aim=GrenadeMath.aim(workspace.CurrentCamera.CFrame.LookVector) or root.CFrame.LookVector
    cooldown=os.clock()+GrenadeMath.cooldown
    sequence+=1
    nadeLabel.Text=string.format("NADE %.1fs",GrenadeMath.cooldown)
    sound("swing",1.3)
    ShoveAnimation.play(player.Character)
    -- Local visual prediction starts immediately. Only the server creates a damaging grenade.
    local origin=root.Position+Vector3.new(0,2,0)+aim*3
    local key="predicted:"..sequence
    predicted[sequence]=key
    flightVisual(key,{origin=origin,velocity=GrenadeMath.velocity(origin,root.Position+aim*GrenadeMath.range,workspace.Gravity,root.AssemblyLinearVelocity),gravity=workspace.Gravity,started=workspace:GetServerTimeNow()})
    remote:FireServer("throw",{sequence=sequence,aim=aim})
end
queueButton.AutoButtonColor=false
queueButton.Selectable=false
nadeButton.Activated:Connect(throw)
CAS:BindActionAtPriority("ArenaNade",function(_,input)
    if input==Enum.UserInputState.Begin then throw() end
    return Enum.ContextActionResult.Sink
end,false,3000,Enum.KeyCode.F,Enum.KeyCode.ButtonR2)
local function refresh()
    local phase=state:GetAttribute("Phase") or "Lobby"
    local seconds=state:GetAttribute("Seconds") or 0
    update.Visible=(state:GetAttribute("LatestVersion") or 0)>game.PlaceVersion
    if phase=="Countdown" then
        title.Text=(state:GetAttribute("BotMatch") and "VS DUMMY " or "READY ")..seconds
        if lastSecond~=seconds and seconds>0 then sound("tick",1+seconds*0.1) end
    elseif phase=="Battle" then title.Text="RING OUT "..seconds.."s"
    elseif phase=="Result" then title.Text=state:GetAttribute("Winner")=="Draw" and "DRAW" or ((state:GetAttribute("Winner") or "").." WINS")
    else title.Text="AESTHETIC ARENA" end
    if phase=="Result" and lastPhase~="Result" then sound("win") end
    lastPhase,lastSecond=phase,seconds
    local queued=player:GetAttribute("ArenaQueued")
    local queueSeconds=state:GetAttribute("QueueSeconds") or 3
    queueLabel.Text=queued and (phase=="Lobby" and (queueSeconds>0 and "START "..queueSeconds or "LOADING") or "QUEUED") or "STEP ON PAD"
    queueButton.Visible=not isFighter() or phase=="Lobby" or phase=="Result"
    local fighting=isFighter() and phase=="Battle"
    nadeButton.BackgroundTransparency=fighting and 0 or 0.55
    if os.clock()>=feedbackUntil then
        if phase=="Countdown" and isFighter() then hint.Text="Get ready. Face your opponent."
        elseif fighting then hint.Text="Aim with camera · F / R2 / NADE · dodge the blast"
        elseif phase=="Result" then hint.Text="Round over. Returning to the lobby."
        elseif queued then hint.Text="Step off the pad to cancel"
        else hint.Text="Stand on the pink pad to play" end
    end
end
state.AttributeChanged:Connect(refresh)
player:GetAttributeChangedSignal("ArenaQueued"):Connect(refresh)
local knockbackUntil = setmetatable({}, {__mode="k"})
remote.OnClientEvent:Connect(function(event,attacker,victim,duration,round)
    if event=="knockback" then
        if state:GetAttribute("Phase")~="Battle" or round~=state:GetAttribute("Round") then return end
        if attacker~=player.UserId and not (attacker==0 and isFighter() and state:GetAttribute("BotMatch")) then return end
        local c=attacker==0 and workspace:FindFirstChild("PracticeDummy") or player.Character
        local root=c and c:FindFirstChild("HumanoidRootPart")
        local h=c and c:FindFirstChildOfClass("Humanoid")
        if not root or not h or h.Health<=0 then return end
        local untilTime=os.clock()+duration
        knockbackUntil[h]=untilTime
        h.WalkSpeed=0
        h:Move(Vector3.zero)
        h:ChangeState(Enum.HumanoidStateType.Physics)
        local startPosition=root.Position
        root:ApplyImpulse(victim*root.AssemblyMass)
        task.delay(duration,function()
            if h.Parent and knockbackUntil[h]==untilTime then
                h.WalkSpeed=attacker==0 and GrenadeMath.botSpeed or GrenadeMath.runSpeed
                if h.Health>0 then h:ChangeState(Enum.HumanoidStateType.Freefall) end
                if root.Parent then print("AC_ARENA_KNOCKBACK", attacker==0 and "dummy" or "player", "distance", math.round((root.Position-startPosition).Magnitude*10)/10) end
            end
        end)
        print("AC_ARENA_KNOCKBACK", "applied")
        return
    end
    if event=="thrown" then
        local flight=duration
        local oldKey=attacker==player.UserId and predicted[victim]
        local old=oldKey and flights[oldKey]
        if oldKey then flights[oldKey]=nil; predicted[victim]=nil end
        flightVisual(flight.id,flight,old and old.part)
        if attacker==player.UserId then
            feedback("NADE OUT")
        else
            local p=attacker~=0 and Players:GetPlayerByUserId(attacker)
            local c=attacker==0 and workspace:FindFirstChild("PracticeDummy") or (p and p.Character)
            ShoveAnimation.play(c)
        end
        print("AC_ARENA_THROW", "accepted")
    elseif event=="throw-rejected" then
        if predicted[attacker] then removeFlight(predicted[attacker]); predicted[attacker]=nil end
        feedback("Wait for grenade cooldown or stagger to end")
    elseif event=="blast" then
        removeFlight(victim)
        BlastFX.create(attacker,GrenadeMath.radius)
        sound("hit",0.9)
        print("AC_ARENA_BLAST", "rendered")
    end
end)
refresh()

print("AC_ARENA_READY",game.PlaceVersion)

require(script.StickMotion).start()

task.spawn(function()
    local world=workspace:WaitForChild("AestheticArena",15)
    local board=world and world:WaitForChild("ACGallery",15)
    local face=board and board:WaitForChild("Gallery",15)
    local artwork=face and face:WaitForChild("Artwork",15)
    if not artwork then return end
    local previous=""
    while artwork.Parent do
        local status=tostring(state:GetAttribute("MediaStatus")).." revision "..tostring(state:GetAttribute("MediaRevision")).." loaded "..tostring(artwork.IsLoaded)
        if status~=previous then print("AC_ARENA_MEDIA",status); previous=status end
        task.wait(2)
    end
end)
