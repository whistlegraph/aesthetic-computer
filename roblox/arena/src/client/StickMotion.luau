local Motion={}
function Motion.start()
    local rigs=setmetatable({},{__mode="k"})
    local function track(model)
        if not model:IsA("Model") or not model:GetAttribute("ACStick") then return end
        local torso=model:FindFirstChild("Torso")
        local root=model:FindFirstChild("HumanoidRootPart")
        if torso and root then rigs[model]={torso=torso,root=root,phase=0} end
    end
    for _,model in workspace:GetChildren() do track(model) end
    workspace.ChildAdded:Connect(function(model)
        task.spawn(function()
            if model:IsA("Model") then model:WaitForChild("Torso",5); task.wait(0.1); track(model) end
        end)
    end)
    game:GetService("RunService").PreSimulation:Connect(function(dt)
        for model,rig in rigs do
            if not model.Parent then rigs[model]=nil
            else
                local v=rig.root.AssemblyLinearVelocity
                local speed=Vector3.new(v.X,0,v.Z).Magnitude
                rig.phase+=dt*math.min(speed,30)*0.65
                local angle=math.sin(rig.phase)*math.min(speed/24,1)*0.7
                for _,name in {"Right Hip","Left Hip","Right Shoulder","Left Shoulder"} do
                    local joint=rig.torso:FindFirstChild(name)
                    if joint and (not name:find("Shoulder") or os.clock()>=(model:GetAttribute("ACThrowUntil") or 0)) then
                        local sign=(name=="Right Hip" or name=="Left Shoulder") and 1 or -1
                        joint.Transform=CFrame.Angles(angle*sign,0,0)
                    end
                end
            end
        end
    end)
end
return Motion
