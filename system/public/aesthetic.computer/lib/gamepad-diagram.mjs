import { getButtonColors } from "./gamepad-mappings.mjs";

export function drawMiniControllerDiagram(ink, gp, x, y) {
  if (!gp || !ink) return;
  const baseX = x;
  const baseY = y;
  const gamepadId = gp.id || "standard";
  const is8BitDo = gamepadId.includes("8BitDo") || gamepadId.includes("2dc8");

  const getColors = (btnIndex) => {
    const colors = getButtonColors(gamepadId, btnIndex);
    const isPressed = gp.pressedButtons.includes(btnIndex);
    return isPressed ? colors.active : colors.inactive;
  };

  const width = is8BitDo ? 40 : 48;
  const height = is8BitDo ? 20 : 24;

  const hasActiveInput =
    gp.pressedButtons.length > 0 ||
    Object.values(gp.axes).some((v) => Math.abs(parseFloat(v)) > 0.3);

  ink(hasActiveInput ? "slategray" : "dimgray").box(baseX, baseY, width, height);

  if (is8BitDo) {
    const dpadX = baseX + 8;
    const dpadY = baseY + 10;
    const dpadSize = 4;

    const axisX = parseFloat(gp.axes["0"] || 0);
    const axisY = parseFloat(gp.axes["1"] || 0);

    ink(axisY < -0.3 ? "lime" : "gray").box(
      dpadX,
      dpadY - 5,
      dpadSize,
      dpadSize,
    );
    ink(axisY > 0.3 ? "lime" : "gray").box(
      dpadX,
      dpadY + 5,
      dpadSize,
      dpadSize,
    );
    ink(axisX < -0.3 ? "lime" : "gray").box(
      dpadX - 5,
      dpadY,
      dpadSize,
      dpadSize,
    );
    ink(axisX > 0.3 ? "lime" : "gray").box(
      dpadX + 5,
      dpadY,
      dpadSize,
      dpadSize,
    );
    ink("black").box(dpadX, dpadY, dpadSize, dpadSize);

    const faceX = baseX + 30;
    const faceY = baseY + 10;
    const btnSize = 4;

    ink(getColors(3)).box(faceX, faceY - 5, btnSize, btnSize);
    ink(getColors(1)).box(faceX, faceY + 5, btnSize, btnSize);
    ink(getColors(4)).box(faceX - 5, faceY, btnSize, btnSize);
    ink(getColors(0)).box(faceX + 5, faceY, btnSize, btnSize);

    ink(getColors(6)).box(baseX, baseY, 12, 2);
    ink(getColors(7)).box(baseX + 28, baseY, 12, 2);
  } else {
    const lStickX = baseX + 10;
    const lStickY = baseY + 8;
    const axisX = parseFloat(gp.axes["0"] || 0);
    const axisY = parseFloat(gp.axes["1"] || 0);
    const lStickActive = Math.abs(axisX) > 0.1 || Math.abs(axisY) > 0.1;

    ink(lStickActive ? "cyan" : "gray").box(lStickX - 3, lStickY - 3, 6, 6, "line");
    const dotX = lStickX + Math.round(axisX * 2);
    const dotY = lStickY + Math.round(axisY * 2);
    ink("cyan").box(dotX, dotY, 1, 1);

    const dpadX = baseX + 10;
    const dpadY = baseY + 18;
    const dpadSize = 3;

    ink(getColors(12)).box(dpadX, dpadY - 4, dpadSize, dpadSize);
    ink(getColors(13)).box(dpadX, dpadY + 4, dpadSize, dpadSize);
    ink(getColors(14)).box(dpadX - 4, dpadY, dpadSize, dpadSize);
    ink(getColors(15)).box(dpadX + 4, dpadY, dpadSize, dpadSize);

    const faceX = baseX + 38;
    const faceY = baseY + 10;
    const btnSize = 4;

    ink(getColors(3)).box(faceX, faceY - 5, btnSize, btnSize);
    ink(getColors(0)).box(faceX, faceY + 5, btnSize, btnSize);
    ink(getColors(2)).box(faceX - 5, faceY, btnSize, btnSize);
    ink(getColors(1)).box(faceX + 5, faceY, btnSize, btnSize);

    const rStickX = baseX + 28;
    const rStickY = baseY + 18;
    const axis2 = parseFloat(gp.axes["2"] || 0);
    const axis3 = parseFloat(gp.axes["3"] || 0);
    const rStickActive = Math.abs(axis2) > 0.1 || Math.abs(axis3) > 0.1;

    ink(rStickActive ? "yellow" : "gray").box(rStickX - 3, rStickY - 3, 6, 6, "line");
    const dot2X = rStickX + Math.round(axis2 * 2);
    const dot2Y = rStickY + Math.round(axis3 * 2);
    ink("yellow").box(dot2X, dot2Y, 1, 1);

    ink(getColors(4)).box(baseX + 2, baseY, 10, 2);
    ink(getColors(6)).box(baseX + 2, baseY + 2, 8, 2);
    ink(getColors(5)).box(baseX + 36, baseY, 10, 2);
    ink(getColors(7)).box(baseX + 38, baseY + 2, 8, 2);
  }
}
