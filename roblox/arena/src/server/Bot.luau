local Math = require(game.ReplicatedStorage.GrenadeMath)
local Bot = {ID = 0}
function Bot.create()
    local model = require(script.Parent.Avatars).create("Dummy",Color3.fromRGB(255,84,172))
    model.Name = "PracticeDummy"
    model.Humanoid.DisplayName = "Dummy"
    model.Humanoid.WalkSpeed = Math.botSpeed
    model.Humanoid.AutoRotate = false
    model:PivotTo(CFrame.new(9, 6, 0))
    model.HumanoidRootPart.Anchored = true
    local facing = Instance.new("AlignOrientation")
    local attachment = Instance.new("Attachment")
    attachment.Parent = model.HumanoidRootPart
    facing.Name, facing.Mode, facing.Attachment0 = "SmoothFacing", Enum.OrientationAlignmentMode.OneAttachment, attachment
    facing.MaxTorque, facing.Responsiveness, facing.RigidityEnabled = 100000, 12, false
    facing.Parent = model.HumanoidRootPart
    model.Parent = game:GetService("ServerStorage")
    return model
end
function Bot.destination(position, target)
    -- Recover toward the center near the edge; otherwise approach striking range.
    if math.abs(position.X) > 50 or math.abs(position.Z) > 50 then
        return Vector3.new(0, position.Y, 0)
    end
    local delta = Vector3.new(target.X - position.X, 0, target.Z - position.Z)
    if delta.Magnitude <= 22 then return position end
    return target - delta.Unit * 20
end
function Bot.step(root, humanoid, target)
    humanoid.WalkSpeed = Math.botSpeed
    humanoid.AutoRotate = false
    local flatTarget = Vector3.new(target.X, root.Position.Y, target.Z)
    if (flatTarget - root.Position).Magnitude > 0.01 then
        root.SmoothFacing.CFrame = CFrame.lookAt(Vector3.zero, flatTarget - root.Position)
    end
    humanoid:MoveTo(Bot.destination(root.Position, target))
end
return Bot
