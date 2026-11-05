// The two classic 4 × 3 octave grids own the playing area. Header/footer
// reservations never overlap a pad, at either orientation.
export function classicNotepatLayout({ width, height }, { top = 33, bottom = 14, side = 0 } = {}) {
  const usableWidth = Math.max(1, width - side), margin = 3;
  const available = Math.max(1, height - top - bottom - margin * 2);
  const splitLayout = width > height;
  const gap = splitLayout ? Math.min(24, Math.max(4, usableWidth * 0.045)) : 4;
  const cols = splitLayout ? 8 : 4, rows = splitLayout ? 3 : 6;
  const maxWidth = Math.max(1, (usableWidth - margin * 2 - (splitLayout ? gap : 0)) / cols);
  const maxHeight = Math.max(1, (available - (splitLayout ? 0 : gap)) / rows);
  const buttonWidth = Math.max(1, Math.floor(Math.min(maxWidth, maxHeight * 1.5)));
  const buttonHeight = Math.max(1, Math.floor(Math.min(maxHeight, buttonWidth)));
  const blockHeight = rows * buttonHeight + (splitLayout ? 0 : gap);
  const topButtonY = top + margin + Math.floor(Math.max(0, available - blockHeight) / 2);
  const leftOctaveX = Math.floor((usableWidth - (splitLayout ? 8 * buttonWidth + gap : 4 * buttonWidth)) / 2);
  const rightOctaveX = leftOctaveX + 4 * buttonWidth + gap;
  return { buttonWidth, buttonHeight, topButtonY, buttonsPerRow: 4, totalRows: rows,
    margin, bottomPadding: bottom, hudReserved: top, reservedTop: top,
    splitLayout, compactMode: splitLayout, leftOctaveX, rightOctaveX, notesPerSide: 12,
    octaveGap: splitLayout ? 0 : gap, usableWidth, sidePanelWidth: side,
    centerX: leftOctaveX + 4 * buttonWidth, centerAreaWidth: splitLayout ? gap : 0,
    buttonBlockHeight: 3 * buttonHeight, bottomCenterY: height - bottom - 12,
    miniInputsEnabled: false, verticalTrack: false, trackHeight: 0, trackSpacing: 0, melodyButtonRect: null };
}
