local Grenade = {fuse = 1.5, radius = 20, cooldown = 1.8, range = 22, runSpeed = 24, botSpeed = 18}
function Grenade.aim(value)
    if typeof(value) ~= "Vector3" then return nil end
    if value.X ~= value.X or value.Y ~= value.Y or value.Z ~= value.Z or value.Magnitude > 10000 then return nil end
    local flat = Vector3.new(value.X, 0, value.Z)
    return flat.Magnitude > 0.01 and flat.Unit or nil
end
function Grenade.position(origin, velocity, gravity, elapsed)
    return origin + velocity * elapsed - Vector3.new(0, 0.5 * gravity * elapsed^2, 0)
end
function Grenade.velocity(origin, destination, gravity, movement)
    local delta = destination - origin
    local flat = Vector3.new(delta.X, 0, delta.Z)
    if flat.Magnitude > Grenade.range then flat = flat.Unit * Grenade.range end
    local time = 0.5
    local inherited = movement and Vector3.new(movement.X,0,movement.Z) * 0.35 or Vector3.zero
    if inherited.Magnitude > 9 then inherited = inherited.Unit * 9 end
    return inherited + flat / time + Vector3.new(0, math.clamp(delta.Y, -10, 10)/time + 0.5*gravity*time, 0)
end
function Grenade.blast(position, target)
    local delta = target - position
    if delta.Magnitude > Grenade.radius then return nil end
    local flat = Vector3.new(delta.X, 0, delta.Z)
    local direction = flat.Magnitude > 0.01 and flat.Unit or Vector3.new(1, 0, 0)
    local strength = math.max(0.3, 1-delta.Magnitude/Grenade.radius)
    return direction * (70*strength) + Vector3.new(0, 32*strength, 0)
end
return Grenade
