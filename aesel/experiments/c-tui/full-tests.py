"""Run existing Aesel feature suites through the native host, unchanged protocols."""
import os, runpy, subprocess, sys
from pathlib import Path
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[1]
sys.path.insert(0,str(ROOT/'test'))
host=os.environ.get('AESEL_TEST_NATIVE_HOST',str(HERE/'.build/aesel-native'))
os.environ['AESEL_TEST_NATIVE_HOST']=host
original=subprocess.Popen
def hosted(command,*args,**kwargs):
    if isinstance(command,list) and any(str(part).endswith(('/src/tui.mjs','/src/launch.mjs')) for part in command):
        env=dict(kwargs.get('env',os.environ))
        if env.get('AESEL_TEST_LOG'):
            root=Path(env['AESEL_TEST_LOG']).parent
            env['HOME']=str(root)
            env.setdefault('AESEL_CONFIG_DIR',str(root/'.config/aesel'))
            env.setdefault('AESEL_TRANSCRIPTS',str(root/'transcripts'))
        kwargs['env']=env
        command=[host,'--',*command]
    return original(command,*args,**kwargs)
subprocess.Popen=hosted
runpy.run_path(str(ROOT/'test'/sys.argv[1]),run_name='__main__')
