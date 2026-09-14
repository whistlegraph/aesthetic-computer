local Updates = {}
function Updates.accept(state, payload, currentVersion)
    if type(payload) ~= "table" or type(payload.version) ~= "number"
        or payload.version % 1 ~= 0 or payload.version <= currentVersion then return false end
    if payload.version <= (state:GetAttribute("LatestVersion") or currentVersion) then return false end
    state:SetAttribute("LatestVersion", payload.version)
    return true
end
function Updates.start(state)
    task.spawn(function()
        for attempt = 1, 5 do
            local ok = pcall(function()
                game:GetService("MessagingService"):SubscribeAsync("ac-arena-release", function(message)
                    local parsed, payload = pcall(function() return game:GetService("HttpService"):JSONDecode(message.Data) end)
                    if parsed then Updates.accept(state, payload, game.PlaceVersion) end
                end)
            end)
            if ok then return end
            task.wait(attempt * 2)
        end
        warn("Arena update notices unavailable on this server")
    end)
end
return Updates
