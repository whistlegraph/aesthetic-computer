import CoreGraphics

private func contact(_ id: Int32, _ x: CGFloat, _ y: CGFloat,
                     began: Bool = false) -> TrackpadContact {
    TrackpadContact(
        identifier: id,
        point: CGPoint(x: x, y: y),
        state: began ? 3 : 4
    )
}

let first = TrackpadContactChanges.resolve(
    previous: [:],
    contacts: [contact(1, 0.2, 0.3, began: true)]
)
precondition(first.began == [CGPoint(x: 0.2, y: 0.3)])
precondition(first.lifted.isEmpty)

let held = TrackpadContactChanges.resolve(
    previous: first.activeByID,
    contacts: [contact(1, 0.4, 0.5)]
)
precondition(held.began.isEmpty)
precondition(held.lifted.isEmpty)
precondition(held.activeByID[1] == CGPoint(x: 0.4, y: 0.5))

let replaced = TrackpadContactChanges.resolve(
    previous: held.activeByID,
    contacts: [contact(2, 0.7, 0.8, began: true)]
)
precondition(replaced.began == [CGPoint(x: 0.7, y: 0.8)])
precondition(replaced.lifted == [CGPoint(x: 0.4, y: 0.5)])

let lifted = TrackpadContactChanges.resolve(
    previous: replaced.activeByID,
    contacts: []
)
precondition(lifted.active.isEmpty)
precondition(lifted.lifted == [CGPoint(x: 0.7, y: 0.8)])

print("✓ TrackDrum contact transitions")
