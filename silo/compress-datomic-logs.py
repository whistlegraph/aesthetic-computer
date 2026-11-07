#!/usr/bin/env python3
"""Compress closed daily Datomic logs older than a week; never discard them."""
from pathlib import Path
import os
import re
import subprocess
import time

opened = set()
for fd in Path('/proc').glob('[0-9]*/fd/*'):
    try:
        opened.add(os.readlink(fd))
    except OSError:
        pass

for path in Path('/var/log/datomic').glob('*.log'):
    if (path.is_symlink() or not re.fullmatch(r'\d{4}-\d{2}-\d{2}\.log', path.name)
            or str(path) in opened or path.stat().st_mtime >= time.time() - 8 * 86400
            or Path(str(path) + '.gz').exists()):
        continue
    subprocess.run(['nice', '-n', '19', 'gzip', '--', str(path)], check=True)
