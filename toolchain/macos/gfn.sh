#!/bin/bash
# gfn.sh — GeForce NOW network mode for a fleet Mac on Wi-Fi.
#
#   gfn.sh on      pin AWDL (AirDrop/Handoff radio-hopping) off while you play  [admin]
#   gfn.sh off     restore AWDL + AirDrop                                       [admin]
#   gfn.sh check   jitter on the Wi-Fi hop + GFN's own last network test       (no admin)
#
# Why (measured on blueberry 2026-09-02): with awdl0 up the gateway hop is
# avg 18 ms / max 80 ms / stddev 23; with it down, avg 3.5 / max 18 / stddev 3.
# macOS re-raises awdl0 whenever AirDrop/Continuity is touched, so `on`
# installs a root LaunchDaemon that re-lowers it every 15 s; `off` removes it.
# Privilege: sudo when there's a TTY, else a macOS admin dialog (works from
# Claude's `!` runner). Tailscale is left alone: no exit node, so GFN traffic
# never enters it.

set -u
LABEL=computer.aesthetic.gfn-awdl
PLIST=/Library/LaunchDaemons/$LABEL.plist
GFN_LOG="$HOME/Library/Application Support/NVIDIA/GeForceNOW/console.log"

as_root() { # $1 = script text, $2 = prompt
  if [ -t 0 ]; then sudo bash -c "$1"; else
    local esc; esc=$(printf '%s' "$1" | sed 's/\\/\\\\/g; s/"/\\"/g')
    osascript -e "do shell script \"$esc\" with administrator privileges with prompt \"$2\""
  fi
}

jitter() { # $1 = host, $2 = label
  ping -c 50 -i 0.1 "$1" 2>/dev/null | awk -v L="$2" '
    /time=/{split($0,a,"time=");t=a[2]+0;n++;s+=t;if(t>m)m=t;if(n==1||t<mn)mn=t;if(t>20)sp++}
    END{if(!n){printf "  %-18s no replies\n",L;exit}
        printf "  %-18s min %5.1f  avg %5.1f  max %6.1f ms   spikes>20ms %2d/%d\n",L,mn,s/n,m,sp+0,n}'
}

gw() { route -n get default 2>/dev/null | awk '/gateway/{print $2}'; }

check() {
  local up; up=$(ifconfig awdl0 2>/dev/null | head -1 | grep -q '<UP' && echo UP || echo down)
  local held; held=$( [ -f "$PLIST" ] && echo "held by $LABEL" || echo "not held" )
  echo "awdl0: $up ($held)   AirDrop: $(defaults read com.apple.sharingd DiscoverableMode 2>/dev/null || echo default)"
  echo "Wi-Fi: $(system_profiler SPAirPortDataType 2>/dev/null | awk '/Current Network Information/{f=1} f&&/Channel:/{c=$0} f&&/Signal \/ Noise/{print c" |"$0; exit}' | sed 's/  */ /g')"
  echo "Jitter (50 pings @100ms):"
  jitter "$(gw)" "gateway $(gw)"
  jitter 1.1.1.1 "1.1.1.1"
  if [ -f "$GFN_LOG" ]; then
    echo "GFN last NetworkTest:"
    grep -o '"NetworkTest"\|"latency":[0-9]*\|"latencyWithStream":[0-9]*\|"dataLoss":[0-9.]*\|"percentile99thFrameJitter":[0-9.]*\|"zoneName":"[^"]*"\|"networkQuality":"[^"]*"\|"networkType":"[^"]*"' "$GFN_LOG" \
      | tail -8 | tr '\n' ' ' | sed 's/"//g; s/^/  /'; echo
  fi
}

# Single-quoted XML so it survives the as_root quoting layers.
DAEMON_XML="<?xml version='1.0' encoding='UTF-8'?>
<!DOCTYPE plist PUBLIC '-//Apple//DTD PLIST 1.0//EN' 'http://www.apple.com/DTDs/PropertyList-1.0.dtd'>
<plist version='1.0'><dict>
  <key>Label</key><string>$LABEL</string>
  <key>ProgramArguments</key><array>
    <string>/bin/bash</string><string>-c</string>
    <string>while true; do /sbin/ifconfig awdl0 down; sleep 15; done</string>
  </array>
  <key>RunAtLoad</key><true/>
  <key>KeepAlive</key><true/>
</dict></plist>"

case "${1:-check}" in
  on)
    defaults write com.apple.sharingd DiscoverableMode -string Off; killall sharingd 2>/dev/null
    TMP_PLIST=$(mktemp /tmp/$LABEL.XXXX); printf '%s\n' "$DAEMON_XML" > "$TMP_PLIST"
    as_root "cp $TMP_PLIST $PLIST; chown root:wheel $PLIST; chmod 644 $PLIST; launchctl bootout system/$LABEL 2>/dev/null; launchctl bootstrap system $PLIST && /sbin/ifconfig awdl0 down && echo 'awdl0 pinned down by $LABEL'" \
      "gfn.sh on: pin the AWDL (AirDrop) radio down while you play GeForce NOW"
    sleep 2; check ;;
  off)
    as_root "launchctl bootout system/$LABEL 2>/dev/null; rm -f $PLIST; /sbin/ifconfig awdl0 up; echo 'awdl0 released'" \
      "gfn.sh off: restore AirDrop/AWDL"
    defaults write com.apple.sharingd DiscoverableMode -string Everyone; killall sharingd 2>/dev/null
    sleep 2; check ;;
  check) check ;;
  *) sed -n '2,8p' "$0"; exit 1 ;;
esac
