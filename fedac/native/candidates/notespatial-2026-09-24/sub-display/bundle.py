"""Bundle the display changes onto an existing SUB receiver directory; no restart."""
from pathlib import Path
import argparse
parser=argparse.ArgumentParser();parser.add_argument('receiver',type=Path);args=parser.parse_args()
source=Path(__file__).parent
if not (args.receiver/'server.mjs').is_file():raise SystemExit('Expected an existing receiver directory')
helper=(source/'track-info.mjs').read_text().replace('export function','function')
for name in ['app.mjs','display.mjs']:
 target=args.receiver/name
 backup=target.with_suffix(target.suffix+'.before-track')
 if target.exists() and not backup.exists():backup.write_bytes(target.read_bytes())
 module='\n'.join(line for line in (source/name).read_text().splitlines() if "from './track-info.mjs'" not in line)
 target.write_text(helper+'\n'+module+'\n')
print('Updated app/display routes; reload and re-arm the browser while idle.')
