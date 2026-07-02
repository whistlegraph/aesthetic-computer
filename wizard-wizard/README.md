# wizard-wizard

**WizardWizard** — the wizard of wizards. A little hand-pixeled wizard guy
(purple hat, gold star, gray beard — `wizardGrid` in main.swift, no emoji)
in the menu bar who lists every Aesthetic Inc wizard and summons them on click:

ClipWizard · DateWizard · GlyphWizard · JukeWizard · ShotWizard · WaveWizard

Each menu item wears its wizard's mascot; **About Our Wizards…** opens the
full roster with blurbs. WaveWizard asks for a spec.json (its picker opens
in `wave-wizard/samples`). Launch output logs to `/tmp/wizardwizard-<slug>.log`.

## Build & run

```sh
wizard-wizard/bin/wizardwizard   # cached build + launch; replaces a running instance
```

New wizards join the roster in the `wizards` array at the top of
`Sources/WizardWizard/main.swift` — dir, executable name, blurb. Mascots are
read from each wizard's own `Sources/<Exe>/Assets/<exe>-mascot.png` at
runtime; a wizard without one gets the pixel guy as a stand-in.

⌘-drag him to reorder among your other menu bar items.
