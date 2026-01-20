#!/usr/bin/env fish
# AT user pages hosts helper
# Usage:
#   ./hosts-helper.fish jeffrey
#   ./hosts-helper.fish jeffrey.at.aesthetic.computer
#
# This script prints the /etc/hosts line you can add on your HOST machine
# to map a handle subdomain to localhost for testing.

if test (count $argv) -lt 1
  echo "Usage: ./hosts-helper.fish <handle>"
  echo "Example: ./hosts-helper.fish jeffrey"
  exit 1
end

set handle $argv[1]
if not string match -q '*.*' $handle
  set handle "$handle.at.aesthetic.computer"
end

set line "127.0.0.1 $handle"

echo "Add this line to your HOST /etc/hosts:" 

echo $line

echo ""
echo "Then open: http://$handle:4177/user.html"
echo "(Make sure the local dev server is running: npm run dev:user-pages)"
