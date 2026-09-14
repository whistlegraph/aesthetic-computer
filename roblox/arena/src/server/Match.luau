-- Engine-independent match rules. Time is supplied by the server.
local Match = {}
Match.__index = Match
function Match.new()
    return setmetatable({phase = "Lobby", queue = {}, fighters = {}, deadline = 0, winner = nil, round = 0}, Match)
end
function Match:join(id)
    if table.find(self.queue, id) or table.find(self.fighters, id) then return false end
    table.insert(self.queue, id)
    return true
end
function Match:leave(id)
    local index = table.find(self.queue, id)
    if index then table.remove(self.queue, index) end
end
function Match:finish(winner, now)
    self.winner = winner
    self.phase = "Result"
    self.deadline = now + 5
end
function Match:tick(now, alive)
    if self.phase == "Lobby" and #self.queue >= 2 then
        self.fighters = {table.remove(self.queue, 1), table.remove(self.queue, 1)}
        self.round += 1
        self.winner = nil
        self.phase = "Battle"
        self.deadline = now + 60
    elseif self.phase == "Battle" then
        local a, b = alive(self.fighters[1]), alive(self.fighters[2])
        if not a or not b then
            self:finish(a and self.fighters[1] or (b and self.fighters[2] or nil), now)
        elseif now >= self.deadline then
            self:finish(nil, now)
        end
    elseif self.phase == "Result" and now >= self.deadline then
        self.phase = "Lobby"
        self.fighters = {}
        self.winner = nil
    end
end
function Match:canAttack(id, now, lastAttack)
    return self.phase == "Battle" and table.find(self.fighters, id) ~= nil
        and now - (lastAttack or -math.huge) >= 1.8
end
return Match
