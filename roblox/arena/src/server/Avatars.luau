-- Original stick figures over a consistent invisible R6 collision rig.
local Avatar={}
function Avatar.create(name,accent)
    local model=Instance.new("Model")
    model.Name=name
    local function part(name,size,position)
        local p=Instance.new("Part")
        p.Name,p.Size,p.Position,p.Transparency=name,size,position,1
        p.CanCollide,p.CanTouch=true,false
        p.TopSurface,p.BottomSurface=Enum.SurfaceType.Smooth,Enum.SurfaceType.Smooth
        p.Parent=model
        return p
    end
    local root=part("HumanoidRootPart",Vector3.new(2,2,1),Vector3.new(0,3,0))
    root.CanCollide=false
    local torso=part("Torso",Vector3.new(2,2,1),Vector3.new(0,3,0))
    local head=part("Head",Vector3.new(2,1,1),Vector3.new(0,4.5,0))
    local ra=part("Right Arm",Vector3.new(1,2,1),Vector3.new(1.5,3,0))
    local la=part("Left Arm",Vector3.new(1,2,1),Vector3.new(-1.5,3,0))
    local rl=part("Right Leg",Vector3.new(1,2,1),Vector3.new(0.5,1,0))
    local ll=part("Left Leg",Vector3.new(1,2,1),Vector3.new(-0.5,1,0))
    for _,p in {head,ra,la,rl,ll} do p.CanCollide=false end
    local function joint(name,a,b,c0,c1)
        local j=Instance.new("Motor6D")
        j.Name,j.Part0,j.Part1,j.C0,j.C1,j.Parent=name,a,b,c0,c1,a
    end
    joint("RootJoint",root,torso,CFrame.identity,CFrame.identity)
    joint("Neck",torso,head,CFrame.new(0,1,0),CFrame.new(0,-0.5,0))
    joint("Right Shoulder",torso,ra,CFrame.new(1.5,1,0),CFrame.new(0,1,0))
    joint("Left Shoulder",torso,la,CFrame.new(-1.5,1,0),CFrame.new(0,1,0))
    joint("Right Hip",torso,rl,CFrame.new(0.5,-1,0),CFrame.new(0,1,0))
    joint("Left Hip",torso,ll,CFrame.new(-0.5,-1,0),CFrame.new(0,1,0))
    local function visual(parent,size,offset,color,shape)
        local p=Instance.new("Part")
        p.Name,p.Size,p.Shape,p.Color="StickVisual",size,shape or Enum.PartType.Block,color
        p.CanCollide,p.CanTouch,p.CanQuery,p.Massless=false,false,false,true
        p.CFrame,p.Parent=parent.CFrame*offset,model
        local weld=Instance.new("WeldConstraint")
        weld.Part0,weld.Part1,weld.Parent=parent,p,p
        return p
    end
    local ink=Color3.fromRGB(24,24,40)
    local bodyColor=Color3.fromRGB(200,200,220)
    for _,limb in {ra,la,rl,ll} do visual(limb,Vector3.new(0.22,2,0.22),CFrame.identity,bodyColor) end
    visual(torso,Vector3.new(0.28,2,0.28),CFrame.identity,bodyColor)
    visual(torso,Vector3.new(3,0.22,0.22),CFrame.new(0,0.8,0),bodyColor)
    visual(torso,Vector3.new(1,0.22,0.22),CFrame.new(0,-0.85,0),bodyColor)
    visual(head,Vector3.new(1.55,1.55,1.55),CFrame.identity,accent,Enum.PartType.Ball)
    for _,x in {-0.27,0.27} do visual(head,Vector3.new(0.14,0.2,0.1),CFrame.new(x,0.1,-0.74),ink) end
    local h=Instance.new("Humanoid")
    h.Name,h.DisplayName,h.WalkSpeed="Humanoid",name,24
    h.Parent=model
    model.PrimaryPart=root
    model:SetAttribute("ACStick",true)
    return model
end
return Avatar
