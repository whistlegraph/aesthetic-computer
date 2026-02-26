function setup()
  size(256, 256)
  noStroke()
end

function draw()
  background(16, 16, 24)

  local pulse = 48 + math.sin(frameCount * 0.05) * 18
  fill(255, 190, 0)
  circle(width / 2, height / 2, pulse * 2)

  fill(255, 255, 255)
  text("L5 -> AC", 16, 22)

  if mouseIsPressed then
    fill(255, 255, 255)
    circle(mouseX, mouseY, 10)
  end
end
