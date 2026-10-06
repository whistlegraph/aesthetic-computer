"""Small stdin-only transaction helper, run locally or over authenticated SSH."""
import fcntl, hashlib, json, os, pathlib, subprocess, sys, tempfile, time, uuid

home = pathlib.Path.home()
root = home / '.config/slab/displays'
root.mkdir(parents=True, exist_ok=True)
config = home / 'Library/Deskflow/deskflow-handoff-server.conf'
routes = root / 'routes.json'

def digest(text):
    return hashlib.sha256(text.encode()).hexdigest()

def atomic(path, text):
    fd, tmp = tempfile.mkstemp(dir=path.parent)
    try:
        with os.fdopen(fd, 'w') as f:
            f.write(text)
            f.flush()
            os.fsync(f.fileno())
        os.replace(tmp, path)
    finally:
        if os.path.exists(tmp): os.unlink(tmp)

def read():
    text = config.read_text()
    return dict(config=text, hash=digest(text),
        handoff=json.loads((home / '.config/slab/deskflow-handoff.json').read_text()),
        role=json.loads((home / '.config/slab/deskflow.json').read_text()).get('role'),
        routes=routes.read_text() if routes.exists() else None)

def reload():
    # Clients keep their connection. Only an active server needs a reload.
    state = json.loads((home / '.config/slab/deskflow.json').read_text())
    if state.get('role') != 'server': return
    agent = state.get('agent', 'computer.aesthetic.deskflow')
    if not all(c.isalnum() or c in '._-' for c in agent): raise ValueError('Invalid agent')
    log = home / 'Library/Logs/deskflow-core.log'
    offset = log.stat().st_size if log.exists() else 0
    service = 'gui/%s/%s' % (os.getuid(), agent)
    subprocess.run(['/bin/launchctl', 'kill', 'SIGKILL', service], capture_output=True, timeout=5)
    subprocess.run(['/bin/launchctl', 'kickstart', service], capture_output=True, timeout=5)
    for _ in range(80):
        with log.open(errors='replace') as f:
            f.seek(offset)
            tail = f.read()
        if 'started server' in tail: return
        if 'cannot read configuration' in tail or 'configuration error' in tail:
            raise RuntimeError('Deskflow rejected the configuration')
        time.sleep(.1)
    raise RuntimeError('Deskflow did not report a running server within 8 seconds')

request = json.load(sys.stdin)
with (root / 'seat.lock').open('a') as lock:
    fcntl.flock(lock, fcntl.LOCK_EX)
    before = read()
    if request['operation'] == 'read':
        print(json.dumps(before))
        sys.exit(0)
    if request.get('expected') != before['hash']:
        raise RuntimeError('Deskflow changed since this window loaded; refresh first')
    backup = root / ('deskflow-seat-%s.json' % uuid.uuid4())
    atomic(backup, json.dumps(before, indent=2))
    try:
        atomic(config, request['config'])
        if request.get('routes') is None:
            routes.unlink(missing_ok=True)
        else:
            atomic(routes, request['routes'])
        reload()
    except Exception:
        atomic(config, before['config'])
        if before['routes'] is None: routes.unlink(missing_ok=True)
        else: atomic(routes, before['routes'])
        reload()
        raise
    print(json.dumps(dict(backup=str(backup), hash=digest(request['config']))))
