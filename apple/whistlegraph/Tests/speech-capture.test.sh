#!/bin/sh
set -eu
app_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
speech_check_dir=$(mktemp -d "${TMPDIR:-/tmp}/whistlegraph-speech.XXXXXX")
trap 'rm -rf "$speech_check_dir"' EXIT HUP INT TERM
swiftc -swift-version 5 "$app_dir/Sources/LiveTranscription.swift" "$app_dir/Sources/RecordedTranscription.swift" "$app_dir/Sources/UtteranceRecording.swift" "$app_dir/Tests/SpeechCaptureCheck.swift" -o "$speech_check_dir/check"
"$speech_check_dir/check"
