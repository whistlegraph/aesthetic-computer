// High-level controls consume the same document as ACUI's native renderer.
export const noPaintUI = await (
  await fetch(new URL("./Sources/ACUI/Resources/nopaint.json", import.meta.url))
).json();
const resourceRoot = new URL("./Sources/ACUI/Resources/", import.meta.url);
export const brands = await (
  await fetch(new URL("brands.json", resourceRoot))
).json();
export function brandLogo(id) {
  const brand = Object.hasOwn(brands, id) ? brands[id] : brands.model;
  const image = document.createElement("img");
  image.className = "ac-brand-logo";
  image.src = new URL(brand.file, resourceRoot).href;
  image.alt = `${brand.name} logo`;
  image.title = brand.name;
  image.width = image.height = 32;
  image.draggable = false;
  return image;
}
export function paintingLayout(width, height, spec = noPaintUI.layout) {
  const compact = height < spec.compactHeight;
  const side = Math.max(
    1,
    Math.floor(
      Math.min(
        width,
        height -
          (compact ? spec.compactControls : spec.controls) -
          spec.statusHeight,
      ),
    ),
  );
  return {
    side,
    controls: Math.max(1, height - side - spec.statusHeight),
    status: spec.statusHeight,
  };
}
export function actionButton(action, dispatch) {
  const button = document.createElement("button");
  button.type = "button";
  button.textContent = action.label;
  button.dataset.action = action.id;
  button.dataset.enabled = action.enabled;
  button.title = [action.help, action.key && `(${action.key.toUpperCase()})`]
    .filter(Boolean)
    .join(" ");
  button.addEventListener("click", () => dispatch(action.id));
  return button;
}
export function decisionBar(dispatch, spec = noPaintUI) {
  const bar = document.createElement("div");
  bar.className = "decisions";
  for (const action of spec.components.find((c) => c.kind === "decisions")
    .actions)
    bar.append(actionButton(action, dispatch));
  return bar;
}
export function menu(spec, dispatch) {
  const details = document.createElement("details"),
    summary = document.createElement("summary");
  summary.textContent = spec.label;
  details.append(summary);
  const items = document.createElement("div");
  items.className = "menu-items";
  for (const action of spec.actions)
    items.append(
      actionButton(action, (id) => {
        details.open = false;
        dispatch(id);
      }),
    );
  details.append(items);
  return details;
}
export function updateActions(root, capabilities) {
  for (const button of root.querySelectorAll("[data-enabled]"))
    button.disabled = !capabilities[button.dataset.enabled];
}
const handlePalettes = new Map();
export function handleLabel(handle) {
  const label = document.createElement("span");
  label.className = "ac-handle";
  const characters = Array.from(handle, (character, index) => {
    const span = document.createElement("span");
    span.textContent = character;
    const hue = (330 + index * 47) % 360;
    span.style.color = `light-dark(hsl(${hue} 65% 37%), hsl(${hue} 85% 72%))`;
    label.append(span);
    return span;
  });
  if (!handlePalettes.has(handle)) {
    handlePalettes.set(
      handle,
      fetch(
        `/api/handle-colors?handle=${encodeURIComponent(handle.replace(/^@/, ""))}`,
        {
          signal: AbortSignal.timeout(10000),
        },
      )
        .then((response) => (response.ok ? response.json() : null))
        .then((data) => (Array.isArray(data?.colors) ? data.colors : []))
        .catch(() => []),
    );
  }
  handlePalettes.get(handle).then((colors) => {
    characters.forEach((span, index) => {
      const color = colors[index];
      if (
        color &&
        [color.r, color.g, color.b].every(
          (v) => Number.isFinite(v) && v >= 0 && v <= 255,
        )
      )
        span.style.color = `rgb(${color.r}, ${color.g}, ${color.b})`;
    });
  });
  return label;
}
export function applyPalette(spec = noPaintUI) {
  const dark = matchMedia("(prefers-color-scheme: dark)");
  const apply = () => {
    for (const [key, colors] of Object.entries(spec.colors))
      document.documentElement.style.setProperty(
        `--${key}`,
        colors[dark.matches ? 1 : 0],
      );
  };
  apply();
  dark.addEventListener("change", apply);
}
