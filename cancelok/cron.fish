#!/usr/bin/env fish
# cron.fish — install (or remove) the hourly cancelok loop on this Mac.
#
# One pad an hour, generated, certified, and shipped while you're not looking.
# By morning there's a queue, and emptying it is a minute of thumbs.
#
#   fish cancelok/cron.fish install
#   fish cancelok/cron.fish status
#   fish cancelok/cron.fish uninstall
#   fish cancelok/cron.fish now        # one turn, right here, watch it work

set -l REPO (cd (dirname (status filename))/..; pwd)
set -l LABEL computer.aesthetic.cancelok
set -l PLIST "$HOME/Library/LaunchAgents/$LABEL.plist"
set -l LOG "$HOME/Library/Logs/cancelok.log"
set -l CMD $argv[1]

# The loop needs serve-local (the gate measures against it) and `claude` (the
# generator IS claude). launchd starts with a bare PATH, so we hand it the real
# one rather than discovering at 3am that node isn't on it.
set -l NODE (which node)
set -l REALPATH (dirname $NODE):/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin

switch "$CMD"
  case install
    mkdir -p (dirname $PLIST)
    echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">
<plist version=\"1.0\">
<dict>
  <key>Label</key><string>$LABEL</string>
  <key>ProgramArguments</key>
  <array>
    <string>/bin/sh</string>
    <string>-lc</string>
    <string>cd $REPO; curl -sf -o /dev/null http://localhost:8899/ || (node marketing/av-reels/bin/serve-local.mjs &amp; sleep 3); node cancelok/loop.mjs --tries 2</string>
  </array>
  <key>EnvironmentVariables</key>
  <dict><key>PATH</key><string>$REALPATH</string></dict>
  <key>StartInterval</key><integer>3600</integer>
  <key>RunAtLoad</key><false/>
  <key>StandardOutPath</key><string>$LOG</string>
  <key>StandardErrorPath</key><string>$LOG</string>
</dict>
</plist>" > $PLIST
    launchctl bootout gui/(id -u)/$LABEL 2>/dev/null
    launchctl bootstrap gui/(id -u) $PLIST
    echo "⏰ cancelok runs hourly. log: $LOG"
    echo "   watch it: tail -f $LOG"

  case uninstall
    launchctl bootout gui/(id -u)/$LABEL 2>/dev/null
    rm -f $PLIST
    echo "🛑 cancelok cron removed."

  case status
    if launchctl list | grep -q $LABEL
      echo "⏰ installed and loaded."
      launchctl list $LABEL | grep -E 'PID|LastExitStatus'
      test -f $LOG; and echo "--- last run ---"; and tail -12 $LOG
    else
      echo "not installed. → fish cancelok/cron.fish install"
    end

  case now
    cd $REPO
    curl -sf -o /dev/null http://localhost:8899/; or begin
      node marketing/av-reels/bin/serve-local.mjs &
      sleep 3
    end
    node cancelok/loop.mjs --tries 2

  case '*'
    echo "usage: cron.fish install | uninstall | status | now"
end
