local Http=game:GetService("HttpService")
local Bridge={url="https://assets.aesthetic.computer/roblox/manifest.json"}
function Bridge.validate(data)
    if type(data)~="table" or data.schemaVersion~=1 or type(data.revision)~="number" or data.revision%1~=0 or data.revision<1 or type(data.items)~="table" or #data.items>12 then return nil end
    local items={}
    for _,item in data.items do
        if type(item)~="table" or item.approved~=true or type(item.imageAssetId)~="string" or not string.match(item.imageAssetId,"^[1-9]%d*$") or #item.imageAssetId>20 then return nil end
        table.insert(items,item.imageAssetId)
    end
    return items
end
function Bridge.fetch()
    local response=Http:RequestAsync({Url=Bridge.url,Method="GET",Headers={Accept="application/json"}})
    if not response.Success or #response.Body>32768 then error("Media manifest unavailable") end
    local data=Http:JSONDecode(response.Body)
    local items=Bridge.validate(data)
    if not items then error("Invalid media manifest") end
    return items,data.revision
end
function Bridge.start(world,state)
    local board=Instance.new("Part")
    board.Name,board.Size,board.Anchored="ACGallery",Vector3.new(20,14,1),true
    board.Color=Color3.fromRGB(24,24,40)
    board.CFrame=CFrame.lookAt(Vector3.new(-17,9,114),Vector3.new(-17,9,128))
    board.Parent=world
    local face=Instance.new("SurfaceGui")
    face.Name,face.Face,face.CanvasSize,face.Parent="Gallery",Enum.NormalId.Front,Vector2.new(800,560),board
    local image=Instance.new("ImageLabel")
    image.Name,image.Size,image.Position,image.BackgroundTransparency="Artwork",UDim2.new(1,-32,1,-80),UDim2.fromOffset(16,16),1
    image.ScaleType,image.Parent=Enum.ScaleType.Fit,face
    local credit=Instance.new("TextLabel")
    credit.Text,credit.Font,credit.TextSize="AESTHETIC COMPUTER",Enum.Font.Code,28
    credit.Size,credit.Position,credit.BackgroundTransparency=UDim2.new(1,0,0,56),UDim2.new(0,0,1,-56),1
    credit.TextColor3,credit.Parent=Color3.fromRGB(65,235,235),face
    task.spawn(function()
        local lastRevision=0
        while world.Parent do
            local ok,items,revision=pcall(Bridge.fetch)
            if ok and revision>=lastRevision then
                lastRevision=revision
                image.Image=#items>0 and "rbxassetid://"..items[1] or ""
                state:SetAttribute("MediaRevision",revision)
                state:SetAttribute("MediaStatus","connected")
            elseif not ok then state:SetAttribute("MediaStatus","retrying") end
            -- Retain the last good image across transient failures.
            task.wait(60)
        end
    end)
    return board
end
return Bridge
