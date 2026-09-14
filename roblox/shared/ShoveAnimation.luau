-- Procedural, client-rendered shove for Motor6D and upgraded avatar joints.
local RunService = game:GetService("RunService")
local Shove = {}
local playing = setmetatable({}, {__mode="k"})
function Shove.joints(character)
    local result = {}
    for _, joint in character:GetDescendants() do
        if (joint:IsA("Motor6D") or joint:IsA("AnimationConstraint")) and joint.Name:lower():find("shoulder") then
            local basis = joint:IsA("Motor6D") and joint.C0.Rotation
                or (joint.Attachment0 and joint.Attachment0.CFrame.Rotation or CFrame.identity)
            table.insert(result, {joint=joint, base=joint.Transform, basis=basis})
        end
    end
    return result
end
function Shove.pose(t)
    -- Slight draw-back, fast extension, held contact, then recovery.
    if t < 0.08 then return -0.12 * (t/0.08) end
    if t < 0.18 then return -0.12 + 1.12 * ((t-0.08)/0.1) end
    if t < 0.3 then return 1 end
    return math.max(0, 1-(t-0.3)/0.2)
end
function Shove.apply(joints, elapsed)
    local done = elapsed >= 0.5
    local rotation = CFrame.Angles(math.rad(85) * Shove.pose(elapsed), 0, 0)
    for _, item in joints do
        if item.joint.Parent then
            item.joint.Transform = done and item.base or (item.base * item.basis:Inverse() * rotation * item.basis)
        end
    end
    return done
end
function Shove.play(character)
    if not character or playing[character] then return 0 end
    local joints = Shove.joints(character)
    playing[character] = true
    character:SetAttribute("ACThrowUntil",os.clock()+0.55)
    local elapsed = 0
    local connection
    connection = RunService.PreSimulation:Connect(function(dt)
        elapsed += dt
        if elapsed >= 0.5 or not character.Parent then
            for _, item in joints do if item.joint.Parent then item.joint.Transform = item.base end end
            connection:Disconnect()
            playing[character] = nil
            return
        end
        Shove.apply(joints, elapsed)
    end)
    return #joints
end
return Shove
