#!/bin/sh
while :; do
  awk '
    /^crtc\[/ {kind="crtc"; key=$2 " " $3}
    /^connector\[/ {kind="connector"; connector=$2}
    kind=="crtc" && /mode:/ {resolution=$2; gsub(/[":]/,"",resolution); modes[key]=resolution; rates[key]=$3}
    kind=="connector" && /crtc=/ && connector ~ /^HDMI/ {pipe=$0; sub(/^[[:space:]]*crtc=/,"",pipe); if(pipe!="(null)"){selected=pipe; name=connector}}
    END {split(modes[selected],size,"x"); printf "{\"connector\":\"%s\",\"width\":%d,\"height\":%d,\"hz\":%d}\n",name,size[1],size[2],rates[selected]}
  ' /sys/kernel/debug/dri/0/state > /tmp/ac-hdmi-state.json.new
  mv /tmp/ac-hdmi-state.json.new /tmp/ac-hdmi-state.json
  sleep 2
done
