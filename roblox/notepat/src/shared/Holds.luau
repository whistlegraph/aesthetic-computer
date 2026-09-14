-- Multiple fingers/keys may hold the same pad without releasing one another.
local Holds = {}
Holds.__index = Holds
function Holds.new()
    return setmetatable({sources = {}, counts = {}}, Holds)
end
function Holds:press(source, note)
    if self.sources[source] then return false end
    self.sources[source] = note
    self.counts[note] = (self.counts[note] or 0) + 1
    return self.counts[note] == 1
end
function Holds:release(source)
    local note = self.sources[source]
    if not note then return nil end
    self.sources[source] = nil
    self.counts[note] -= 1
    if self.counts[note] == 0 then self.counts[note] = nil; return note end
    return nil
end
function Holds:clear()
    local notes = {}
    for note in pairs(self.counts) do table.insert(notes, note) end
    self.sources = {}; self.counts = {}
    return notes
end
return Holds
