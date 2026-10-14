// High-level controls consume the same document as ACUI's native renderer.
export const noPaintUI = await (
  await fetch(new URL("./Sources/ACUI/Resources/nopaint.json", import.meta.url))
).json();
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
