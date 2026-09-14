local Math = require(game.ReplicatedStorage.GrenadeMath)
local Grenades = {}
Grenades.__index = Grenades
function Grenades.new(onBlast)
    return setmetatable({active = {}, onBlast = onBlast, serial = 0}, Grenades)
end
function Grenades:throw(origin, destination, now, round, thrower, movement)
    local grenade = Instance.new("Part")
    grenade.Name, grenade.Shape, grenade.Size = "Grenade", Enum.PartType.Ball, Vector3.new(1.6,1.6,1.6)
    grenade.Color, grenade.Material = Color3.fromRGB(255,175,40), Enum.Material.Neon
    -- The server sweeps the whole projectile between positions; no bouncing or tunneling.
    grenade.Anchored, grenade.CanCollide, grenade.CanTouch, grenade.CanQuery = true, false, false, false
    grenade.Transparency = 1
    grenade.Position, grenade.Parent = origin, workspace
    local params = RaycastParams.new()
    params.FilterType = Enum.RaycastFilterType.Exclude
    params.FilterDescendantsInstances = thrower and {grenade, thrower} or {grenade}
    params.RespectCanCollide = true
    self.serial += 1
    local flight = {id=self.serial,origin=origin,velocity=Math.velocity(origin,destination,workspace.Gravity,movement),gravity=workspace.Gravity,started=workspace:GetServerTimeNow(),round=round}
    local tag = Instance.new("BillboardGui")
    tag.Enabled = false
    tag.Size,tag.StudsOffset,tag.AlwaysOnTop,tag.Parent = UDim2.fromOffset(90,36),Vector3.new(0,2,0),true,grenade
    local label=Instance.new("TextLabel")
    label.Size,label.BackgroundTransparency,label.TextColor3=UDim2.fromScale(1,1),1,Color3.new(1,1,1)
    label.TextSize,label.Font,label.TextStrokeTransparency,label.Parent=24,Enum.Font.Code,0,tag
    label.Text="1.5"
    table.insert(self.active,{part=grenade,label=label,deadline=now+Math.fuse,round=round,
        id=flight.id,origin=origin,velocity=flight.velocity,started=now,elapsed=0,params=params})
    return grenade, flight
end
function Grenades:tick(now, round)
    for i=#self.active,1,-1 do
        local item=self.active[i]
        if item.round~=round or not item.part.Parent then
            item.part:Destroy(); table.remove(self.active,i)
        else
            local elapsed=math.min(now-item.started,Math.fuse)
            local contact=nil
            -- Substeps keep the arc accurate even after a slow server frame.
            while item.elapsed<elapsed and not contact do
                item.elapsed=math.min(item.elapsed+1/60,elapsed)
                local position=Math.position(item.origin,item.velocity,workspace.Gravity,item.elapsed)
                local delta=position-item.part.Position
                if delta.Magnitude>0.0001 then contact=workspace:Spherecast(item.part.Position,0.8,delta,item.params) end
                item.part.Position=contact and contact.Position or position
            end
            if contact or now>=item.deadline then
                local position=item.part.Position
                item.part:Destroy(); table.remove(self.active,i)
                self.onBlast(position,item.id)
            else item.label.Text=string.format("%.1f",item.deadline-now) end
        end
    end
end
function Grenades:clear()
    for _,item in self.active do item.part:Destroy() end
    table.clear(self.active)
end
return Grenades
