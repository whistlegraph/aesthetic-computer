// Keep one proper Dock tile for an app: add it if missing, and give every
// tile that lacks one the bookmark and bundle id the Dock needs to draw it.
//
// A tile written with only `_CFURLString` (what `defaults write … -array-add`
// makes) shows as a "?" on current macOS, and replacing the bundle in place
// orphans the old bookmark. Prints what it changed; the caller restarts Dock.
//
//   swift mac-dock-tile.swift /Applications/oskiewar.app
import Foundation

let app = URL(fileURLWithPath: CommandLine.arguments[1]).standardizedFileURL
let domain = "com.apple.dock" as CFString
var apps = (CFPreferencesCopyAppValue("persistent-apps" as CFString, domain) as? [[String: Any]]) ?? []

func tile(for url: URL) throws -> [String: Any] {
  var data: [String: Any] = [
    "file-data": ["_CFURLString": url.absoluteString, "_CFURLStringType": 15],
    "file-label": url.deletingPathExtension().lastPathComponent,
    "book": try url.bookmarkData(),
  ]
  if let bundle = Bundle(url: url)?.bundleIdentifier { data["bundle-identifier"] = bundle }
  return ["tile-data": data, "tile-type": "file-tile"]
}

var changed = false
var found = false
for index in apps.indices {
  let data = apps[index]["tile-data"] as? [String: Any] ?? [:]
  guard let string = (data["file-data"] as? [String: Any])?["_CFURLString"] as? String,
        let url = URL(string: string) else { continue }
  let isApp = url.standardizedFileURL.path.lowercased() == app.path.lowercased()
  if isApp { found = true }
  // Rebuild the target app's tile every time (its bundle was just replaced),
  // and any other tile the Dock cannot draw.
  if isApp || (data["book"] == nil && FileManager.default.fileExists(atPath: url.path)) {
    apps[index] = try tile(for: isApp ? app : url)
    changed = true
    print("repaired", url.path)
  }
}
if !found {
  apps.append(try tile(for: app))
  changed = true
  print("added", app.path)
}
if changed {
  CFPreferencesSetAppValue("persistent-apps" as CFString, apps as CFArray, domain)
  CFPreferencesAppSynchronize(domain)
}
