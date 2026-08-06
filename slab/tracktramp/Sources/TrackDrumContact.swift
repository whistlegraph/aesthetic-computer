import CoreGraphics

struct TrackpadContact: Equatable {
    let identifier: Int32
    let point: CGPoint
    let state: Int32

    var isActive: Bool { state == 3 || state == 4 }
    var isBegan: Bool { state == 3 }
}

struct TrackpadContactChanges {
    let active: [TrackpadContact]
    let began: [CGPoint]
    let lifted: [CGPoint]
    let activeByID: [Int32: CGPoint]

    static func resolve(previous: [Int32: CGPoint],
                        contacts: [TrackpadContact]) -> TrackpadContactChanges {
        let active = contacts.filter(\.isActive)
        let current = Dictionary(uniqueKeysWithValues: active.map {
            ($0.identifier, $0.point)
        })
        let began = active.compactMap { contact in
            contact.isBegan || previous[contact.identifier] == nil
                ? contact.point : nil
        }
        let lifted = previous.compactMap { identifier, point in
            current[identifier] == nil ? point : nil
        }
        return TrackpadContactChanges(
            active: active,
            began: began,
            lifted: lifted,
            activeByID: current
        )
    }
}
