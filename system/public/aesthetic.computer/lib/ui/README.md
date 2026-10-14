# AC UI

High-level interface documents with platform renderers. No Paint is the first
consumer: `nopaint.art/ai/` in the browser and the Swift No Paint app.

`Sources/ACUI/Resources/nopaint.json` is the single source for the component
inventory, decision actions, enablement keys, shortcuts, menu actions, layout
measurements, light/dark palette, change sizes and brush sizes. `web.mjs`
renders DOM controls. The `ACUI` Swift package decodes that same document and
provides a SwiftUI decision bar and layout measurements. Swift keeps native
canvas rendering, menus, input and accessibility; DOM keeps browser controls.

The state owner supplies `canNo`, `canPaint`, `canChange`, `canDone`, and other
capabilities. Rendering cannot submit inference or publish work. Model catalogs,
server prices and account balances remain runtime data, never constants in the
interface document. Native model/status views and the web model dialog retain
their platform implementations; sharing the document does not imply that every
view implementation is shared.

The package resource bundle must accompany an installed native executable.
No Paint's `native/build.zsh` copies `ACUI_ACUI.bundle` into app resources.

Browser validation: `node tests/browser/nopaint-ai-web.test.mjs` from the repo
root. It uses one isolated Chrome instance, mocked authentication/provider/
publication endpoints, and saves desktop, phone and dark-mode screenshots.
No paid inference or public test paintings are created.
