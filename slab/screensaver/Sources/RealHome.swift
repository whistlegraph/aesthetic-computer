import Foundation
import Darwin

/// Resolve the *real* user home directory.
///
/// The screensaver runs inside Apple's sandboxed `legacyScreenSaver` host,
/// where `NSHomeDirectory()` / `$HOME` point at a per-saver container
/// (`~/Library/Containers/com.apple.ScreenSaver.Engine.legacyScreenSaver/…`)
/// rather than `/Users/<you>`. The slab state lives under the real home, so
/// we read it from the passwd database (directory-services backed, immune to
/// the container redirection) instead of the environment.
enum RealHome {
    static let path: String = {
        if let pw = getpwuid(getuid()) {
            let dir = String(cString: pw.pointee.pw_dir)
            if !dir.isEmpty { return dir }
        }
        return ProcessInfo.processInfo.environment["HOME"] ?? NSHomeDirectory()
    }()
}
