#!/bin/sh
# Focused Foundation-only marker/HTML/file-boundary check; no package build.
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
probeDir=$(mktemp -d /tmp/slab-artifact-check.XXXXXX)
probe="$probeDir/check.swift"
trap 'rm -rf "$probeDir"' EXIT
cat "$root/Sources/SlabMenubar/LocalArtifactPreview.swift" > "$probe"
cat >> "$probe" <<'SWIFT'
let marker: [String: Any] = ["path": "/tmp/picture.png", "mime": "image/png", "version": 2, "artifactId": "picture-one"]
let picture = LocalArtifactPreview(marker: marker, kind: "picture")!
assert(picture.version == 2)
assert(LocalArtifactPreview(marker: marker, kind: "piece") == nil)
var invalid = marker; invalid["path"] = "https://example.com/a.png"
assert(LocalArtifactPreview(marker: invalid, kind: "picture") == nil)
invalid = marker; invalid["mime"] = "text/html"
assert(LocalArtifactPreview(marker: invalid, kind: "picture") == nil)
invalid = marker; invalid["version"] = 0
assert(LocalArtifactPreview(marker: invalid, kind: "picture") == nil)
let sound = LocalArtifactPreview(marker: ["path":"/tmp/sound.wav", "mime":"audio/wav", "version":1,"artifactId":"sound-one"], kind:"sound")!
let audioHTML = sound.html(nonce:"test")
assert(audioHTML.contains("controls preload='metadata'"))
assert(!audioHTML.contains("autoplay"))
assert(audioHTML.contains("onloadedmetadata=ready"))
assert(audioHTML.contains("<div class='name' title='/tmp/sound.wav'>sound.wav</div>"))
let reelMarker: [String: Any] = ["path":"/tmp/reel one.mov", "mime":"video/quicktime", "version":3,"artifactId":"video-one"]
let reel = LocalArtifactPreview(marker: reelMarker, kind:"video")!
assert(reel.name == "reel one.mov")
assert(reel.sizeLimit > 64 * 1024 * 1024)
let videoHTML = reel.html(nonce:"test")
assert(videoHTML.contains("<video id='artifact' controls muted autoplay loop"))
assert(videoHTML.contains("onloadedmetadata=ready"))
assert(videoHTML.contains(">reel one.mov</div>"))
assert(LocalArtifactPreview(marker: reelMarker, kind:"picture") == nil)
let paper = LocalArtifactPreview(marker: ["path":"/tmp/source.txt", "mime":"text/plain", "version":1,"artifactId":"paper-one"], kind:"paper")!
let html = paper.html(text:"<script>oops()</script>", nonce:"test")
assert(html.contains("&lt;script&gt;oops()&lt;/script&gt;"))
assert(html.contains("default-src 'none'"))
let directory = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
try FileManager.default.createDirectory(at:directory,withIntermediateDirectories:true)
defer { try? FileManager.default.removeItem(at:directory) }
let file = directory.appendingPathComponent("source.txt")
try "Hello".write(to:file,atomically:true,encoding:.utf8)
let actual = LocalArtifactPreview(marker:["path":file.path,"mime":"text/plain","version":1,"artifactId":"paper-one"],kind:"paper")!
let bytes = try actual.readValidatedFile()
assert(bytes == Data("Hello".utf8))
let link = directory.appendingPathComponent("link.txt")
try FileManager.default.createSymbolicLink(at:link,withDestinationURL:file)
let linked = LocalArtifactPreview(marker:["path":link.path,"mime":"text/plain","version":1,"artifactId":"paper-one"],kind:"paper")!
do { _ = try linked.readValidatedFile(); fatalError("accepted symlink") } catch {}
let staging = directory.appendingPathComponent("stage")
try FileManager.default.createDirectory(at:staging,withIntermediateDirectories:true)
let staged = try actual.stageFile(into: staging)
assert(staged.lastPathComponent == "artifact")
let stagedBytes = try Data(contentsOf: staged)
assert(stagedBytes == Data("Hello".utf8))
do { _ = try linked.stageFile(into: staging); fatalError("staged symlink") } catch {}
var wav = Data(repeating: 0, count: 48)
wav.replaceSubrange(0..<4, with: Data("RIFF".utf8))
wav.replaceSubrange(8..<16, with: Data("WAVEfmt ".utf8))
wav.replaceSubrange(36..<40, with: Data("data".utf8))
wav[20] = 1; wav[22] = 1; wav[34] = 16; wav[45] = 64
assert(LocalArtifactPreview.waveform(wav)?.contains("Audio waveform") == true)
assert(LocalArtifactPreview.waveform(Data("invalid".utf8)) == nil)
print("Local artifact preview checks passed")
SWIFT
swift "$probe"
