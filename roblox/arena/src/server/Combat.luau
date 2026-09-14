local Combat = {}
function Combat.impulse(origin, facing, target, mass)
    local delta = target - origin
    local flat = Vector3.new(delta.X, 0, delta.Z)
    if delta.Magnitude > 8 or flat.Magnitude < 0.01 then return nil end
    if facing:Dot(flat.Unit) < 0.35 then return nil end
    return (flat.Unit * 42 + Vector3.new(0, 8, 0)) * mass
end
return Combat
