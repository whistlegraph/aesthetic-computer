import Cocoa
let opts = CGWindowListOption(arrayLiteral: .optionOnScreenOnly, .excludeDesktopElements)
guard let info = CGWindowListCopyWindowInfo(opts, kCGNullWindowID) as? [[String:Any]] else { exit(1) }
let target = CommandLine.arguments.count > 1 ? CommandLine.arguments[1] : "WaveWizard"
for w in info {
  if let owner = w[kCGWindowOwnerName as String] as? String, owner.contains(target),
     let name = w[kCGWindowName as String] as? String, !name.isEmpty,
     let wid = w[kCGWindowNumber as String] {
    print("\(wid)")
    exit(0)
  }
}
exit(1)
