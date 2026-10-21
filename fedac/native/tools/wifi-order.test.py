#!/usr/bin/env python3
"""Run the production auto-connect function against fake radio/file I/O."""
from pathlib import Path
import subprocess
import tempfile

native = Path(__file__).resolve().parents[1]
source = (native / 'src/wifi.c').read_text()
start = source.index('static void wifi_do_autoconnect(ACWifi *wifi) {')
end = source.index('// Worker thread', start)
function = source[start:end]
harness = r'''
#include "wifi.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define AC_SSID "preset"
#define AC_PASS "fixture"
static int attempts, fail_first, cooled;
static char tried[8][WIFI_SSID_MAX];
static FILE *test_fopen(const char *path, const char *mode) {
    assert(strcmp(path, "/mnt/wifi_creds.json") == 0);
    FILE *f = tmpfile();
    fputs("[{\"ssid\":\"Saved venue\",\"pass\":\"fixture\"}]", f);
    rewind(f); return f;
}
static void wifi_log(ACWifi *w, const char *fmt, ...) {}
static void wifi_do_scan(ACWifi *w) {}
static void wifi_set_state_and_status(ACWifi *w, WiFiState state, const char *s) {w->state=state;}
static int wifi_cooling(const char *s) {return cooled && strcmp(s,"Saved venue")==0;}
static void wifi_cool(const char *s) {if(strcmp(s,"Saved venue")==0)cooled=1;}
static void wifi_do_connect(ACWifi *w, const char *ssid, const char *pass) {
    assert(attempts<8); strcpy(tried[attempts++],ssid);
    w->state=(fail_first && attempts==1)?WIFI_STATE_FAILED:WIFI_STATE_CONNECTED;
}
#define fopen test_fopen
FUNCTION
#undef fopen
int main(void) {
    ACWifi w={0}; pthread_mutex_init(&w.lock, NULL);
    w.network_count=2;
    strcpy(w.networks[0].ssid,"CULTUREHUB LA");w.networks[0].signal=-20;
    strcpy(w.networks[1].ssid,"Saved venue");w.networks[1].signal=-80;
    wifi_do_autoconnect(&w);
    assert(attempts==1 && strcmp(tried[0],"Saved venue")==0);
    attempts=0;fail_first=1;w.state=WIFI_STATE_OFF;
    wifi_do_autoconnect(&w);
    assert(attempts==2 && strcmp(tried[0],"Saved venue")==0 && strcmp(tried[1],"CULTUREHUB LA")==0);
    attempts=0;fail_first=0;w.state=WIFI_STATE_OFF;
    wifi_do_autoconnect(&w);
    assert(attempts==1 && strcmp(tried[0],"CULTUREHUB LA")==0);
    attempts=0;cooled=0;w.network_count=1;w.state=WIFI_STATE_OFF;
    wifi_do_autoconnect(&w);
    assert(attempts==1 && strcmp(tried[0],"CULTUREHUB LA")==0);
    pthread_mutex_destroy(&w.lock);
    puts("PASS: saved venue precedes stronger built-in preferred network; failure, cooldown and absence retain fallback.");
}
'''.replace('FUNCTION', function)
with tempfile.TemporaryDirectory() as temp:
    root = Path(temp)
    (root / 'test.c').write_text(harness)
    subprocess.run(['cc', '-I', str(native / 'src'), str(root / 'test.c'), '-pthread', '-o', str(root / 'test')], check=True)
    subprocess.run([str(root / 'test')], check=True)
