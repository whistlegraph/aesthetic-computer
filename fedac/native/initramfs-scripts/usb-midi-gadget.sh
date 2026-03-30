#!/bin/sh
# usb-midi-gadget.sh — configure ac-native as a USB MIDI gadget

STATE="/run/usb-midi.state"
GADGET_ROOT="/sys/kernel/config/usb_gadget/ac-midi"
CONFIG_NAME="c.1"
FUNC_NAME="midi.usb0"
LANG="0x409"
VID="0x1d6b"
PID="0x0104"

serial_number() {
  if [ -f /mnt/.machine-id ]; then
    tr -d '\r\n' </mnt/.machine-id
    return
  fi
  echo "ac-midi"
}

first_typec_port() {
  for port_path in /sys/class/typec/port*; do
    [ -d "$port_path" ] || continue
    port="$(basename "$port_path")"
    case "$port" in
      *-*) continue ;;
    esac
    echo "$port"
    return
  done
}

current_role() {
  path="$1"
  primary="$2"
  secondary="$3"
  [ -r "$path" ] || return
  raw="$(cat "$path" 2>/dev/null)"
  case "$raw" in
    *"[$primary]"*) echo "$primary" ;;
    *"[$secondary]"*) echo "$secondary" ;;
    *) echo "$raw" | sed 's/[][]//g' ;;
  esac
}

try_device_role() {
  port="$1"
  [ -n "$port" ] || return 0
  pbase="/sys/class/typec/$port"

  if [ -w "$pbase/power_role" ] && grep -q "sink" "$pbase/power_role" 2>/dev/null; then
    printf "sink\n" >"$pbase/power_role" 2>/dev/null || true
  fi
  if [ -w "$pbase/data_role" ] && grep -q "device" "$pbase/data_role" 2>/dev/null; then
    printf "device\n" >"$pbase/data_role" 2>/dev/null || true
  fi
}

first_udc() {
  for udc_path in /sys/class/udc/*; do
    [ -e "$udc_path" ] || continue
    basename "$udc_path"
    return
  done
}

find_alsa_device() {
  [ -r /proc/asound/cards ] || return
  while IFS= read -r line; do
    case "$line" in
      *"MIDI Gadget"*|*"f_midi"*)
        card="$(echo "$line" | sed -n 's/^[[:space:]]*\([0-9][0-9]*\).*/\1/p')"
        [ -n "$card" ] && echo "hw:$card,0,0"
        return
        ;;
    esac
  done </proc/asound/cards
}

log_line() {
  ts="$(date -u '+%Y-%m-%dT%H:%M:%SZ' 2>/dev/null || date 2>/dev/null || echo now)"
  line="$ts [usb-midi] $*"
  echo "$line" >>/tmp/usb-midi-gadget.log 2>/dev/null || true
  if [ -d /mnt ]; then
    echo "$line" >>/mnt/ac-native.log 2>/dev/null || true
    echo "$line" >>/mnt/usb-midi.log 2>/dev/null || true
  fi
}

write_state() {
  enabled="$1"
  active="$2"
  reason="$3"
  udc="$4"
  port="$5"
  power_role="$6"
  data_role="$7"
  alsa_device="$8"
  serial="$9"

  mkdir -p /run
  {
    echo "enabled=$enabled"
    echo "active=$active"
    echo "reason=$reason"
    echo "udc=$udc"
    echo "port=$port"
    echo "power_role=$power_role"
    echo "data_role=$data_role"
    echo "alsa_device=$alsa_device"
    echo "serial=$serial"
  } >"$STATE"
  log_line "enabled=$enabled active=$active reason=$reason udc=$udc port=$port power=$power_role data=$data_role alsa=$alsa_device serial=$serial"
}

ensure_configfs() {
  mkdir -p /sys/kernel/config
  if ! grep -q " /sys/kernel/config " /proc/mounts 2>/dev/null; then
    mount -t configfs none /sys/kernel/config 2>/dev/null || true
  fi
  [ -d /sys/kernel/config/usb_gadget ]
}

cleanup_gadget() {
  if [ -e "$GADGET_ROOT/UDC" ]; then
    printf "\n" >"$GADGET_ROOT/UDC" 2>/dev/null || true
  fi
  rm -f "$GADGET_ROOT/configs/$CONFIG_NAME/$FUNC_NAME" 2>/dev/null || true
  rmdir "$GADGET_ROOT/functions/$FUNC_NAME" 2>/dev/null || true
  rmdir "$GADGET_ROOT/configs/$CONFIG_NAME/strings/$LANG" 2>/dev/null || true
  rmdir "$GADGET_ROOT/configs/$CONFIG_NAME" 2>/dev/null || true
  rmdir "$GADGET_ROOT/strings/$LANG" 2>/dev/null || true
  rmdir "$GADGET_ROOT" 2>/dev/null || true
}

bring_up() {
  serial="$(serial_number)"
  port="$(first_typec_port)"
  [ -n "$port" ] && try_device_role "$port"
  sleep 1

  power_role=""
  data_role=""
  if [ -n "$port" ]; then
    power_role="$(current_role "/sys/class/typec/$port/power_role" source sink)"
    data_role="$(current_role "/sys/class/typec/$port/data_role" host device)"
  fi

  if ! ensure_configfs; then
    write_state 1 0 no-configfs "" "$port" "$power_role" "$data_role" "" "$serial"
    return 1
  fi

  cleanup_gadget
  mkdir -p "$GADGET_ROOT" || {
    write_state 1 0 create-failed "" "$port" "$power_role" "$data_role" "" "$serial"
    return 1
  }

  printf "%s\n" "$VID" >"$GADGET_ROOT/idVendor" || true
  printf "%s\n" "$PID" >"$GADGET_ROOT/idProduct" || true
  printf "0x0200\n" >"$GADGET_ROOT/bcdUSB" || true
  printf "0x0100\n" >"$GADGET_ROOT/bcdDevice" || true

  mkdir -p "$GADGET_ROOT/strings/$LANG"
  printf "%s\n" "$serial" >"$GADGET_ROOT/strings/$LANG/serialnumber"
  printf "aesthetic.computer\n" >"$GADGET_ROOT/strings/$LANG/manufacturer"
  printf "ac native usb midi\n" >"$GADGET_ROOT/strings/$LANG/product"

  mkdir -p "$GADGET_ROOT/configs/$CONFIG_NAME/strings/$LANG"
  printf "usb midi\n" >"$GADGET_ROOT/configs/$CONFIG_NAME/strings/$LANG/configuration"
  printf "250\n" >"$GADGET_ROOT/configs/$CONFIG_NAME/MaxPower" || true

  if ! mkdir "$GADGET_ROOT/functions/$FUNC_NAME" 2>/dev/null; then
    write_state 1 0 no-midi-function "" "$port" "$power_role" "$data_role" "" "$serial"
    cleanup_gadget
    return 1
  fi

  printf "1\n" >"$GADGET_ROOT/functions/$FUNC_NAME/in_ports" || true
  printf "1\n" >"$GADGET_ROOT/functions/$FUNC_NAME/out_ports" || true
  printf "512\n" >"$GADGET_ROOT/functions/$FUNC_NAME/buflen" || true
  printf "32\n" >"$GADGET_ROOT/functions/$FUNC_NAME/qlen" || true
  printf "acnative\n" >"$GADGET_ROOT/functions/$FUNC_NAME/id" || true

  ln -s "$GADGET_ROOT/functions/$FUNC_NAME" "$GADGET_ROOT/configs/$CONFIG_NAME/$FUNC_NAME" 2>/dev/null || true

  udc="$(first_udc)"
  if [ -z "$udc" ]; then
    write_state 1 0 no-udc "" "$port" "$power_role" "$data_role" "" "$serial"
    return 1
  fi

  if ! printf "%s\n" "$udc" >"$GADGET_ROOT/UDC" 2>/dev/null; then
    write_state 1 0 bind-failed "$udc" "$port" "$power_role" "$data_role" "" "$serial"
    return 1
  fi

  alsa_device=""
  for _ in 1 2 3; do
    alsa_device="$(find_alsa_device)"
    [ -n "$alsa_device" ] && break
    sleep 1
  done

  write_state 1 1 ok "$udc" "$port" "$power_role" "$data_role" "$alsa_device" "$serial"
  return 0
}

bring_down() {
  port="$(first_typec_port)"
  power_role=""
  data_role=""
  [ -n "$port" ] && power_role="$(current_role "/sys/class/typec/$port/power_role" source sink)"
  [ -n "$port" ] && data_role="$(current_role "/sys/class/typec/$port/data_role" host device)"
  cleanup_gadget
  write_state 0 0 disabled "" "$port" "$power_role" "$data_role" "" "$(serial_number)"
}

case "${1:-up}" in
  up)
    bring_up
    ;;
  down)
    bring_down
    ;;
  refresh)
    bring_down
    bring_up
    ;;
  *)
    echo "usage: $0 [up|down|refresh]" >&2
    exit 1
    ;;
esac
