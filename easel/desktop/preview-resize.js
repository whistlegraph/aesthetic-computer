// A single preview size: moving over the picture never changes its bounds.
window.installPreviewResize = () => {
  const shell = document.getElementById("artifact-shell"),
    artifact = document.getElementById("artifact");
  let drag = null,
    custom = null,
    frame = 0;
  const apply = () => {
    if (!custom) return;
    const right = Math.max(8, Math.min(innerWidth - 104, custom.right)),
      top = Math.max(8, Math.min(innerHeight - 80, custom.top));
    const width = Math.max(96, Math.min(innerWidth - right - 8, custom.width)),
      height = Math.max(72, Math.min(innerHeight - top - 40, custom.height));
    custom = { width, height, right, top };
    if (
      !window.currentPreviewMedium ||
      window.currentPreviewMedium === "piece"
    ) {
      window.previewUserDimensions = [
        (128 * (width - 12)) / (height - 12),
        128,
      ];
      window.setPreviewDimensions(...window.previewUserDimensions);
    }
    for (const [key, value] of Object.entries(custom))
      shell.style.setProperty("--resized-preview-" + key, `${value}px`);
    shell.dataset.resized = "true";
  };
  artifact.addEventListener("pointerenter", () => {
    if (!drag) document.body.dataset.previewZone = "picture";
  });
  shell.addEventListener("pointerleave", () => {
    if (!drag) document.body.dataset.previewZone = "";
  });
  for (const direction of ["n", "e", "s", "w", "nw", "ne", "se", "sw"]) {
    const grip = document.createElement("button");
    grip.type = "button";
    grip.className = "preview-resize-edge";
    grip.dataset.edge = direction;
    if (direction === "sw") grip.id = "preview-resize-grip";
    grip.setAttribute("aria-label", `Resize preview ${direction}`);
    grip.title = "Drag to reshape preview";
    shell.append(grip);
    grip.addEventListener("pointerenter", () => {
      document.body.dataset.previewZone = "resize";
    });
    const allowed = () =>
      !document.body.classList.contains("preview-fullscreen");
    const bounds = () => {
      const rect = shell.getBoundingClientRect();
      return {
        width: rect.width,
        height: rect.height,
        right: innerWidth - rect.right,
        top: rect.top,
      };
    };
    grip.addEventListener("pointerdown", (event) => {
      if (event.button !== 0 || !allowed()) return;
      event.preventDefault();
      event.stopPropagation();
      custom = bounds();
      drag = { x: event.clientX, y: event.clientY, ...custom, direction };
      shell.dataset.resizing = "true";
      grip.setPointerCapture(event.pointerId);
    });
    grip.addEventListener("pointermove", (event) => {
      if (!drag) return;
      const dx = event.clientX - drag.x,
        dy = event.clientY - drag.y,
        d = drag.direction;
      custom = {
        width: drag.width + (d.includes("w") ? -dx : d.includes("e") ? dx : 0),
        height:
          drag.height + (d.includes("n") ? -dy : d.includes("s") ? dy : 0),
        right: drag.right - (d.includes("e") ? dx : 0),
        top: drag.top + (d.includes("n") ? dy : 0),
      };
      if (!frame)
        frame = requestAnimationFrame(() => {
          frame = 0;
          apply();
        });
    });
    const end = () => {
      if (!drag) return;
      cancelAnimationFrame(frame);
      frame = 0;
      apply();
      drag = null;
      delete shell.dataset.resizing;
      document.body.dataset.previewZone = "";
    };
    for (const name of ["pointerup", "pointercancel", "lostpointercapture"])
      grip.addEventListener(name, end);
    grip.addEventListener("keydown", (event) => {
      if (
        !allowed() ||
        !["ArrowLeft", "ArrowRight", "ArrowUp", "ArrowDown"].includes(event.key)
      )
        return;
      event.preventDefault();
      event.stopPropagation();
      custom = bounds();
      const dx =
        event.key === "ArrowLeft" ? -16 : event.key === "ArrowRight" ? 16 : 0;
      const dy =
        event.key === "ArrowUp" ? -16 : event.key === "ArrowDown" ? 16 : 0;
      if (direction.includes("w")) custom.width -= dx;
      if (direction.includes("e")) {
        custom.width += dx;
        custom.right -= dx;
      }
      if (direction.includes("n")) {
        custom.height -= dy;
        custom.top += dy;
      }
      if (direction.includes("s")) custom.height += dy;
      apply();
    });
  }
  window.addEventListener("resize", () => {
    if (custom) apply();
  });
};
