#!/usr/bin/env fish
# cron.fish — install (or remove) the hourly cancelok loop on this Mac.
#
# One pad an hour, generated and certified while you're not looking. By morning
# there's a queue, and emptying it is a minute of thumbs.
#
# The cron does NOT publish. It runs --no-ship, so a pad it makes waits in the
# local queue until you've seen it and said so. Nothing reaches other people on
# a timer — the machine can make things unattended, but it can't decide that
# anyone else should have to look at them. To publish one you kept:
#
#   node cancelok/ship.mjs <pad> "<trait>"    # → ^pads, commit, push, deploy
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

# launchd starts with a bare PATH — it does not source your shell — so every
# binary has to be handed over as an absolute path it will still be able to find
# after a reboot.
#
# `which node` is a TRAP here: under fnm it resolves to a per-shell temp dir with
# a PID in the name (…/fnm_multishells/36675_…/bin/node). Baking that into a
# launchd plist works today and dies the next time you log in, and the failure is
# invisible because the cron just quietly stops making anything. So we resolve the
# STABLE fnm alias instead, and fall back to homebrew.
set -l NODE
for candidate in "$HOME/.local/share/fnm/aliases/default/bin/node" /opt/homebrew/bin/node /usr/local/bin/node
  if test -x $candidate
    set NODE $candidate
    break
  end
end
if test -z "$NODE"
  echo "❌ no stable node found — refusing to install a cron that will rot."
  exit 1
end

# `claude` IS the generator, and it lives in ~/.local/bin, which launchd has never
# heard of.
set -l CLAUDE (command -v claude)
if test -z "$CLAUDE"; set CLAUDE "$HOME/.local/bin/claude"; end
if not test -x $CLAUDE
  echo "❌ claude not found at $CLAUDE — the generator cannot run."
  exit 1
end

set -l REALPATH (dirname $NODE):(dirname $CLAUDE):/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin

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
    <string>cd $REPO; curl -sf -o /dev/null http://localhost:8899/ || ($NODE marketing/av-reels/bin/serve-local.mjs &amp; sleep 3); exec $NODE cancelok/loop.mjs --tries 2 --no-ship</string>
  </array>
  <key>EnvironmentVariables</key>
  <dict>
    <key>PATH</key><string>$REALPATH</string>
    <key>CANCELOK_CLAUDE</key><string>$CLAUDE</string>
  </dict>
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
