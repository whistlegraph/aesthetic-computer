#!/bin/zsh
set -eu
HERE=${0:A:h:h}
mkdir -p "$HERE/local"
export PATH="$HOME/.local/bin:/opt/homebrew/bin:/usr/bin:/bin:$PATH"
# GUI apps do not inherit the interactive fnm Node path.
if ! command -v node >/dev/null; then
  nodes=("$HOME"/.local/share/fnm/node-versions/*/installation/bin(N))
  (( ${#nodes} )) && export PATH="${nodes[-1]}:$PATH"
fi
if curl --max-time 2 -fsS http://127.0.0.1:8767/api/state >/dev/null 2>&1; then exit 0; fi
args=()
if [[ -f "$HERE/local/play-server.json" ]]; then
  resume=$(python3 - "$HERE" <<'PY'
import json,sys
from pathlib import Path
root=Path(sys.argv[1]); value=json.loads((root/'local/play-server.json').read_text())
session=(root/value['session']).resolve()
if session.is_relative_to(root/'local/play') and (session/'session.json').is_file(): print(session)
PY
)
  [[ -z "$resume" ]] || args=(--resume "$resume")
fi
# Server lifetime is shared by the browser and native app. Quitting either UI
# does not kill the other's move or lose its accepted painting.
export NOPAINT_ENABLE_FAL=0
exec uv run --offline --no-project --python 3.12 --with-requirements "$HERE/local-requirements.txt" python "$HERE/play.py" "${args[@]}" >> "$HERE/local/play-evolution.log" 2>&1
