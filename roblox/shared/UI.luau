-- AC lettering with GNU Unifont-derived Unicode fallback.
local UI = {}
UI.colors = {ink = Color3.fromRGB(24, 24, 40), cyan = Color3.fromRGB(65, 235, 235), pink = Color3.fromRGB(255, 84, 172)}
function UI.label(parent, properties)
    local label = Instance.new("TextLabel")
    label.BackgroundColor3 = UI.colors.ink
    label.TextColor3 = UI.colors.cyan
    label.FontFace = Font.fromEnum(Enum.Font.Code)
    label.TextSize, label.TextWrapped = 22, true
    for key, value in properties or {} do label[key] = value end
    label.Parent = parent
return label
end
function UI.bitmapLabel(parent, properties)
    local label = UI.label(parent, properties)
    local glyphs = require(script.Glyphs)
    local unicode = require(script.Unifont)
    local ink = Instance.new("Frame")
    ink.Name, ink.BackgroundTransparency, ink.Size, ink.Parent = "ACLettering", 1, UDim2.fromScale(1, 1), label
    local function render()
        ink:ClearAllChildren()
        local chars, width, height = {}, 0, 10
        for _, code in utf8.codes(label.Text) do
            local runs=glyphs[utf8.char(code)]
            local glyph=runs and {runs=runs,width=6,height=10} or unicode.glyph(code)
            if not glyph then label.TextTransparency=0; return end
            table.insert(chars,glyph)
            width+=glyph.width
            height=math.max(height,glyph.height)
        end
        label.TextTransparency=1
        local scale=math.max(1,math.min(4,math.floor(math.min(label.AbsoluteSize.X/math.max(1,width),label.AbsoluteSize.Y/height))))
        local x=math.floor((label.AbsoluteSize.X-width*scale)/2)
        local y=math.floor((label.AbsoluteSize.Y-height*scale)/2)
        for _,glyph in chars do
            local baseline=height-glyph.height
            for _,run in glyph.runs do
                local pixel=Instance.new("Frame")
                pixel.BorderSizePixel,pixel.BackgroundColor3=0,label.TextColor3
                pixel.Position=UDim2.fromOffset(x+run[1]*scale,y+(baseline+run[2])*scale)
                pixel.Size,pixel.Parent=UDim2.fromOffset(run[3]*scale,scale),ink
            end
            x+=glyph.width*scale
        end
    end
    label:GetPropertyChangedSignal("Text"):Connect(render)
    label:GetPropertyChangedSignal("TextColor3"):Connect(render)
    label:GetPropertyChangedSignal("AbsoluteSize"):Connect(render)
    render()
    return label
end
return UI
