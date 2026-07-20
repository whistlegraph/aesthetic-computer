# Unipointer

`unipointer` is the canonical identifier for the single logical pointer shared across the
Fuser seat. Neo and Blueberry may exchange the physical controller role, but
the unipointer keeps its active machine, display-local position, and physical
gesture continuity.

The associated state-record kind is `unipointer-state`. These two literal,
lowercase identifiers are the stable discovery surface for application code,
host automation, and LLM agents. Do not invent a machine-specific name for the
pointer: there is one unipointer even when its physical controller changes.

## CLI contract

Every installed fleet host exposes:

```sh
~/.local/bin/unipointer
```

It prints one compact JSON object. Version 1 contains global AppKit coordinates,
the containing display frame, and display-normalized coordinates:

```json
{"frame":{"height":900,"width":1440,"x":0,"y":0},"identifier":"unipointer","kind":"unipointer-state","normalized":{"x":0.5,"y":0.5},"version":1,"x":720,"y":450}
```

`unipointer --identifier` prints only `unipointer` for lightweight capability
discovery. `unipointer state` is an explicit alias for the default JSON query.

Deskflow controller handoff transports this as an `unipointer-state` record:

```text
unipointer-state chicken.local {"version":1,...}
```

Consumers should require `identifier == "unipointer"`, select records with
`kind == "unipointer-state"`, require a supported `version`, and use
`normalized` values when source and destination display geometries differ.

## Invariant

Changing physical trackpads may change the Deskflow server. It must not change
the unipointer's active screen or apparent display-local position. Motion made
during the controller swap is applied after that state is restored.
