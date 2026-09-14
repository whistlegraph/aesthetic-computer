local TweenService=game:GetService("TweenService")
local Debris=game:GetService("Debris")
local FX={}
function FX.create(position,radius)
    local model=Instance.new("Model")
    model.Name="GrenadeBlast"
    local function part(name)
        local p=Instance.new("Part")
        p.Name,p.Anchored,p.CanCollide,p.CanTouch,p.CanQuery=name,true,false,false,false
        p.Material,p.Color=Enum.Material.Neon,Color3.fromRGB(255,175,40)
        p.Parent=model
        return p
    end
    local sphere=part("Sphere")
    sphere.Shape,sphere.Size,sphere.Position,sphere.Transparency=Enum.PartType.Ball,Vector3.one,position,0.45
    TweenService:Create(sphere,TweenInfo.new(0.5),{Size=Vector3.one*radius*2,Transparency=1}):Play()
    for i=1,32 do
        local a,b=(i-1)*math.pi*2/32,i*math.pi*2/32
        local start=Vector3.new(math.cos(a),0,math.sin(a))*radius
        local finish=Vector3.new(math.cos(b),0,math.sin(b))*radius
        local center=Vector3.new(position.X,1.3,position.Z)
        local bar=part("Ring")
        bar.Color=Color3.fromRGB(65,235,235)
        bar.Size,bar.Position=Vector3.new(0.3,0.25,0.3),center
        local middle=center+(start+finish)/2
        TweenService:Create(bar,TweenInfo.new(0.5),{
            CFrame=CFrame.lookAt(middle,center+finish),
            Size=Vector3.new(0.4,0.25,(finish-start).Magnitude+0.1),Transparency=1,
        }):Play()
    end
    model.Parent=workspace
    Debris:AddItem(model,0.65)
    return model
end
return FX
