import Foundation
import IOKit

enum MenuBandHaptics {
    static let isAvailable: Bool = detectAvailability()

    private static func detectAvailability() -> Bool {
        var iterator: io_iterator_t = 0
        let result = IOServiceGetMatchingServices(
            kIOMasterPortDefault,
            IOServiceMatching("AppleMultitouchTrackpadHIDEventDriver"),
            &iterator
        )
        guard result == KERN_SUCCESS else { return false }
        defer { IOObjectRelease(iterator) }

        var service = IOIteratorNext(iterator)
        while service != 0 {
            defer { IOObjectRelease(service) }
            if boolProperty("ForceSupported", on: service) == true ||
                boolProperty("ActuationSupported", on: service) == true ||
                multitouchDefaultProperty("ForceSupported", on: service) == true ||
                multitouchDefaultProperty("ActuationSupported", on: service) == true {
                return true
            }
            service = IOIteratorNext(iterator)
        }
        return false
    }

    private static func boolProperty(_ key: String, on service: io_object_t) -> Bool? {
        guard let value = IORegistryEntryCreateCFProperty(
            service,
            key as CFString,
            kCFAllocatorDefault,
            0
        )?.takeRetainedValue() else {
            return nil
        }
        return value as? Bool
    }

    private static func multitouchDefaultProperty(_ key: String, on service: io_object_t) -> Bool? {
        guard let value = IORegistryEntryCreateCFProperty(
            service,
            "DefaultMultitouchProperties" as CFString,
            kCFAllocatorDefault,
            0
        )?.takeRetainedValue() as? [String: Any] else {
            return nil
        }
        return value[key] as? Bool
    }
}
