import AppKit
import XCTest
@testable import MenuBand

#if !MAC_APP_STORE
final class ShapedownTests: XCTestCase {
    func testSingleFingerKitRegions() {
        let empty = TrackpadPercussionPad.State()
        XCTAssertEqual(transition(empty, [CGPoint(x: 0.2, y: 0.8)]).entered, [.kick])
        XCTAssertEqual(transition(empty, [CGPoint(x: 0.2, y: 0.2)]).entered, [.snare])
        XCTAssertEqual(transition(empty, [CGPoint(x: 0.8, y: 0.2)]).entered, [.hatClosed])
    }

    func testTwoFingersChooseCompoundArticulations() {
        let empty = TrackpadPercussionPad.State()
        let reverse = transition(empty, [CGPoint(x: 0.2, y: 0.8),
                                         CGPoint(x: 0.8, y: 0.7)])
        XCTAssertEqual(reverse.entered, [.reverseKick])
        let open = transition(empty, [CGPoint(x: 0.2, y: 0.2),
                                      CGPoint(x: 0.8, y: 0.3)])
        XCTAssertEqual(open.entered, [.hatOpen])
    }

    func testCompoundGestureDoesNotFireSinglePadWhenOneFingerLifts() {
        let empty = TrackpadPercussionPad.State()
        let open = transition(empty, [CGPoint(x: 0.2, y: 0.2),
                                      CGPoint(x: 0.8, y: 0.3)])
        let oneLeft = transition(open.state, [CGPoint(x: 0.2, y: 0.2)])
        XCTAssertEqual(oneLeft.exited, [.hatOpen])
        XCTAssertTrue(oneLeft.entered.isEmpty)
        XCTAssertEqual(oneLeft.state.bottom, .cooldown)

        let clear = transition(oneLeft.state, [])
        XCTAssertEqual(clear.state.bottom, .none)
    }

    func testDrumSkinMovementDoesNotRetriggerButAddedFingerDoes() {
        let held = CGPoint(x: 0.30, y: 0.40)
        let moved = CGPoint(x: 0.34, y: 0.44)
        XCTAssertTrue(TrackpadDrumSkinPad.newStrikes(
            previous: [held], current: [moved]
        ).isEmpty)

        let added = CGPoint(x: 0.82, y: 0.76)
        XCTAssertEqual(TrackpadDrumSkinPad.newStrikes(
            previous: [held], current: [moved, added]
        ), [added])
    }

    func testDrumSkinLiftReturnsReleasedContactPosition() {
        let released = CGPoint(x: 0.18, y: 0.92)
        let held = CGPoint(x: 0.72, y: 0.40)
        XCTAssertEqual(TrackpadDrumSkinPad.liftedTouches(
            previous: [released, held], current: [CGPoint(x: 0.73, y: 0.41)]
        ), [released])
    }

    func testDrumSkinZonesFollowRoundedTrackpadInsets() {
        XCTAssertEqual(MenuBandPercussion.drumSkinZone(
            at: CGPoint(x: 0.5, y: 0.5)), .kick)
        XCTAssertEqual(MenuBandPercussion.drumSkinZone(
            at: CGPoint(x: 0.5, y: 0.70)), .tom)
        XCTAssertEqual(MenuBandPercussion.drumSkinZone(
            at: CGPoint(x: 0.5, y: 0.80)), .snare)
        XCTAssertEqual(MenuBandPercussion.drumSkinZone(
            at: CGPoint(x: 0.5, y: 0.88)), .hat)
        XCTAssertEqual(MenuBandPercussion.drumSkinZone(
            at: CGPoint(x: 0.5, y: 0.98)), .click)

        // The wider physical X axis crosses the same inset materials farther
        // from normalized center than Y; this is trackpad geometry, not a
        // circular or diamond approximation.
        XCTAssertEqual(MenuBandPercussion.drumSkinZone(
            at: CGPoint(x: 0.75, y: 0.5)), .kick)
        XCTAssertEqual(MenuBandPercussion.drumSkinZone(
            at: CGPoint(x: 0.82, y: 0.5)), .tom)
        XCTAssertEqual(MenuBandPercussion.drumSkinZone(
            at: CGPoint(x: 0.88, y: 0.5)), .snare)
        XCTAssertEqual(MenuBandPercussion.drumSkinZone(
            at: CGPoint(x: 0.93, y: 0.5)), .hat)
        XCTAssertEqual(MenuBandPercussion.drumSkinZone(
            at: CGPoint(x: 0.98, y: 0.5)), .click)
    }

    func testDrumSkinMovementHasNoSoftwareDeadZone() {
        let held = [CGPoint(x: 0.25, y: 0.60)]
        XCTAssertEqual(TrackpadDrumSkinPad.averageMovement(
            previous: held, current: held
        ), 0, accuracy: 0.000_001)
        XCTAssertEqual(TrackpadDrumSkinPad.averageMovement(
            previous: held, current: [CGPoint(x: 0.2501, y: 0.60)]
        ), 0.0001, accuracy: 0.000_001)
    }

    func testDominantScratchSeparatesMovingFingerFromTensionAnchors() throws {
        let motion = try XCTUnwrap(TrackpadDrumSkinPad.dominantScratch(
            previous: [CGPoint(x: 0.20, y: 0.30), CGPoint(x: 0.80, y: 0.70)],
            current: [CGPoint(x: 0.21, y: 0.30), CGPoint(x: 0.86, y: 0.70)]
        ))
        XCTAssertEqual(motion.point.x, 0.86, accuracy: 0.000_001)
        XCTAssertEqual(motion.movement, 0.06, accuracy: 0.000_001)
        XCTAssertEqual(motion.delta.dx, 0.06, accuracy: 0.000_001)
        XCTAssertEqual(motion.delta.dy, 0, accuracy: 0.000_001)
        XCTAssertEqual(motion.anchors, [CGPoint(x: 0.21, y: 0.30)])
    }

    func testScratchPitchMovesLinearlyInOctavesAcrossWideRange() {
        XCTAssertEqual(MenuBandPercussion.scratchPitchMultiplier(speed: 0),
                       1, accuracy: 0.000_001)
        XCTAssertEqual(MenuBandPercussion.scratchPitchMultiplier(speed: 1 / 0.82),
                       2, accuracy: 0.000_001)
        XCTAssertEqual(MenuBandPercussion.scratchPitchMultiplier(speed: 2 / 0.82),
                       4, accuracy: 0.000_001)
        XCTAssertEqual(MenuBandPercussion.scratchPitchMultiplier(speed: 100),
                       exp2(2.25), accuracy: 0.000_001)
    }

    func testScratchDistinguishesCrossingFromFollowingMaterialContour() {
        let point = CGPoint(x: 0.50, y: 0.70)
        let along = MenuBandPercussion.scratchContourCrossing(
            at: point, direction: CGVector(dx: 0.01, dy: 0)
        )
        let across = MenuBandPercussion.scratchContourCrossing(
            at: point, direction: CGVector(dx: 0, dy: 0.01)
        )
        XCTAssertLessThan(along, 0.05)
        XCTAssertGreaterThan(across, 0.95)
    }

    func testDrumSkinChartRendersAtPhysicalAspectRatio() throws {
        _ = NSApplication.shared
        var membrane = TrackpadMembraneSimulation()
        membrane.reset(at: 1)
        membrane.impulse(at: CGPoint(x: 0.50, y: 0.50), amount: 0.9)
        membrane.advance(to: 1.085,
                         touches: [CGPoint(x: 0.50, y: 0.50),
                                   CGPoint(x: 0.94, y: 0.52)])
        let image = TrackpadDrumSkinPad.image(
            touches: [CGPoint(x: 0.50, y: 0.50),
                      CGPoint(x: 0.94, y: 0.52)],
            energy: [
                .init(point: CGPoint(x: 0.50, y: 0.50), level: 0.88),
                .init(point: CGPoint(x: 0.24, y: 0.72), level: 0.34),
            ],
            membrane: membrane.snapshot()
        )
        XCTAssertEqual(image.size.width / image.size.height,
                       140.0 / 88.0, accuracy: 0.000_001)
        guard let output = ProcessInfo.processInfo.environment[
            "MENUBAND_DRUM_CHART_SNAPSHOT"
        ] else { return }
        let tiff = try XCTUnwrap(image.tiffRepresentation)
        let bitmap = try XCTUnwrap(NSBitmapImageRep(data: tiff))
        let png = try XCTUnwrap(bitmap.representation(using: .png,
                                                       properties: [:]))
        try png.write(to: URL(fileURLWithPath: output), options: .atomic)
    }

    func testSurfaceVelocityFieldIsStrongestAtCenterAndAnchorsAttenuateIt() {
        let center = CGPoint(x: 0.5, y: 0.5)
        let edge = CGPoint(x: 0.01, y: 0.5)
        let centerEnergy = MenuBandPercussion.surfaceVelocityEnergy(
            at: center, anchors: []
        )
        XCTAssertGreaterThan(centerEnergy,
                             MenuBandPercussion.surfaceVelocityEnergy(at: edge, anchors: []))
        XCTAssertGreaterThan(centerEnergy,
                             MenuBandPercussion.surfaceVelocityEnergy(
                                at: center, anchors: [CGPoint(x: 0.2, y: 0.2),
                                                      CGPoint(x: 0.8, y: 0.8)]
                             ))
    }

    func testSurfaceEnergyBuildsLocallyThenDecays() {
        var field = TrackpadSurfaceEnergy()
        let point = CGPoint(x: 0.5, y: 0.5)
        field.reset(at: 1)
        field.energize(at: point, amount: 0.4, now: 1)
        let first = field.energy(at: point, now: 1)
        field.energize(at: point, amount: 0.4, now: 1)
        let built = field.energy(at: point, now: 1)
        XCTAssertGreaterThan(built, first)
        XCTAssertGreaterThan(built,
                             field.energy(at: CGPoint(x: 0.95, y: 0.95), now: 1))
        XCTAssertLessThan(field.energy(at: point, now: 3), built)
    }

    func testMembraneSimulationPinsRimAndPropagatesAcrossSurface() {
        var membrane = TrackpadMembraneSimulation()
        membrane.reset(at: 1)
        membrane.impulse(at: CGPoint(x: 0.5, y: 0.5), amount: 1)
        membrane.advance(to: 1.12, touches: [])
        let snapshot = membrane.snapshot()
        XCTAssertEqual(snapshot.height(at: CGPoint(x: 0, y: 0.5)), 0,
                       accuracy: 0.000_001)
        XCTAssertGreaterThan(abs(snapshot.height(at: CGPoint(x: 0.5, y: 0.5))),
                             0.01)
        XCTAssertGreaterThan(abs(snapshot.height(at: CGPoint(x: 0.65, y: 0.5))),
                             0.000_1)
    }

    func testReleasedMembraneRingsDownWithoutInstability() {
        var membrane = TrackpadMembraneSimulation()
        membrane.reset(at: 1)
        membrane.impulse(at: CGPoint(x: 0.42, y: 0.58), amount: 1)
        var now = 1.0
        for _ in 0..<18 {
            now += 1.0 / 60.0
            membrane.advance(to: now, touches: [])
        }
        let earlyPeak = membrane.snapshot().heights.map(abs).max() ?? 0
        for _ in 0..<180 {
            now += 1.0 / 60.0
            membrane.advance(to: now, touches: [])
        }
        let late = membrane.snapshot().heights
        XCTAssertTrue(late.allSatisfy(\.isFinite))
        XCTAssertLessThan(late.map(abs).max() ?? 0, earlyPeak)
    }

    func testFullestShapeSurvivesStaggeredFourFingerLift() {
        var memory = ShapedownGestureMemory()
        let quad = points(4)
        XCTAssertEqual(memory.update(quad), quad)
        XCTAssertEqual(memory.update(points(3)).count, 4)
        XCTAssertEqual(memory.update(points(2)).count, 4)
        XCTAssertEqual(memory.update(points(1)).count, 4)
        XCTAssertEqual(memory.release().count, 4)
        XCTAssertTrue(memory.peak.isEmpty)
    }

    func testContactIdentityRecoversSameCountRapidReplacement() {
        let old = CGPoint(x: 0.30, y: 0.80)
        let fresh = CGPoint(x: 0.34, y: 0.82)
        let changes = TrackpadContactChanges.resolve(
            previous: [11: old],
            contacts: [TrackpadContact(identifier: 12, point: fresh, state: 3)]
        )
        XCTAssertEqual(changes.began, [fresh])
        XCTAssertEqual(changes.lifted, [old])
        XCTAssertTrue(changes.sameCountReplacement)
    }

    func testSustainedContactIdentityDoesNotRetrigger() {
        let old = CGPoint(x: 0.30, y: 0.80)
        let moved = CGPoint(x: 0.31, y: 0.81)
        let changes = TrackpadContactChanges.resolve(
            previous: [11: old],
            contacts: [TrackpadContact(identifier: 11, point: moved, state: 4)]
        )
        XCTAssertTrue(changes.began.isEmpty)
        XCTAssertTrue(changes.lifted.isEmpty)
        XCTAssertFalse(changes.sameCountReplacement)
    }

    func testRepeatedTapFramesRetriggerAfterEveryCompleteLift() {
        let point = CGPoint(x: 0.48, y: 0.52)
        var active: [Int32: CGPoint] = [:]
        for identifier: Int32 in [21, 22, 23, 24] {
            let down = TrackpadContactChanges.resolve(
                previous: active,
                contacts: [TrackpadContact(identifier: identifier,
                                            point: point, state: 3)]
            )
            XCTAssertEqual(down.began, [point])
            active = down.activeByID

            let up = TrackpadContactChanges.resolve(previous: active, contacts: [])
            XCTAssertEqual(up.lifted, [point])
            XCTAssertTrue(up.activeByID.isEmpty)
            active = up.activeByID
        }
    }

    func testOverlappingFingerTapsEachProduceTheirOwnBeginAndLift() {
        let first = CGPoint(x: 0.30, y: 0.55)
        let second = CGPoint(x: 0.70, y: 0.55)
        let oneDown = TrackpadContactChanges.resolve(
            previous: [:],
            contacts: [TrackpadContact(identifier: 31, point: first, state: 3)]
        )
        let twoDown = TrackpadContactChanges.resolve(
            previous: oneDown.activeByID,
            contacts: [
                TrackpadContact(identifier: 31, point: first, state: 4),
                TrackpadContact(identifier: 32, point: second, state: 3),
            ]
        )
        XCTAssertEqual(twoDown.began, [second])
        XCTAssertTrue(twoDown.lifted.isEmpty)

        let firstUp = TrackpadContactChanges.resolve(
            previous: twoDown.activeByID,
            contacts: [TrackpadContact(identifier: 32, point: second, state: 4)]
        )
        XCTAssertEqual(firstUp.lifted, [first])

        let secondUp = TrackpadContactChanges.resolve(
            previous: firstUp.activeByID, contacts: []
        )
        XCTAssertEqual(secondUp.lifted, [second])
        XCTAssertTrue(secondUp.activeByID.isEmpty)
    }

    func testFreshTrackpadContactCancelsOverlayFadeAndRestoresOpacity() {
        let overlay = PitchBendCursorOverlayWindow()
        overlay.show(image: NSImage(size: NSSize(width: 20, height: 20)),
                     atScreenPoint: .zero)
        overlay.fadeOut(after: 1, duration: 1)
        XCTAssertTrue(overlay.isFadeScheduled)
        overlay.alphaValue = 0.35

        overlay.resumeFromFade()

        XCTAssertFalse(overlay.isFadeScheduled)
        XCTAssertEqual(overlay.alphaValue, 1)
        overlay.dismiss()
    }

    func testScratchArmsSoonerButNeverFromOneMovementFrame() {
        XCTAssertFalse(AppDelegate.shouldArmTrackpadScratch(frames: 1, travel: 0.20))
        XCTAssertFalse(AppDelegate.shouldArmTrackpadScratch(frames: 2, travel: 0.0119))
        XCTAssertTrue(AppDelegate.shouldArmTrackpadScratch(frames: 2, travel: 0.012))
    }

    func testPhysicalClickBoostsScratchButRestingTouchDoesNot() {
        XCTAssertEqual(AppDelegate.physicalClickScratchMultiplier(
            clicked: false), 1, accuracy: 0.000_001)
        XCTAssertEqual(AppDelegate.physicalClickScratchMultiplier(
            clicked: true), 1.55, accuracy: 0.000_001)
    }

    func testTabSelectedPerformanceFXDoesNotAutoEndWithoutHeldNote() {
        XCTAssertFalse(AppDelegate.shouldAutoEndTrackpadFX(
            performanceSessionActive: true,
            keyboardNotesHeld: false
        ))
    }

    func testTabOnlyTogglesPhysicalSkinAndPitchBend() {
        XCTAssertEqual(AppDelegate.trackpadPadModeAfterTab(.skin), .fx)
        XCTAssertEqual(AppDelegate.trackpadPadModeAfterTab(.fx), .skin)
        XCTAssertEqual(AppDelegate.trackpadPadModeAfterTab(.synth), .fx)
        XCTAssertEqual(AppDelegate.trackpadPadModeAfterTab(.kit), .fx)
    }

    func testOrdinaryPitchBendFXStillEndsAfterRelease() {
        XCTAssertTrue(AppDelegate.shouldAutoEndTrackpadFX(
            performanceSessionActive: false,
            keyboardNotesHeld: false
        ))
        XCTAssertFalse(AppDelegate.shouldAutoEndTrackpadFX(
            performanceSessionActive: false,
            keyboardNotesHeld: true
        ))
    }

    func testTransientKeyboardDrumSurfaceEndsAfterRelease() {
        XCTAssertTrue(AppDelegate.shouldOpenTransientTrackpadSurface(
            keyboardNotesHeld: true,
            modeLatched: false
        ))
        XCTAssertTrue(AppDelegate.shouldPersistKeyboardOpenedTrackpadSurface(
            localCaptureArmed: true
        ), "menubar-key focus must keep the drum alive between taps")
        XCTAssertFalse(AppDelegate.shouldPersistKeyboardOpenedTrackpadSurface(
            localCaptureArmed: false
        ), "a global TYPE-mode note should remain transient")
        XCTAssertTrue(AppDelegate.shouldEndTrackpadSessionAfterNoteChange(
            isLatched: true,
            performanceSessionActive: false,
            keyboardNotesHeld: false
        ), "a note-opened drum surface must restore the system cursor")
        XCTAssertFalse(AppDelegate.shouldEndTrackpadSessionAfterNoteChange(
            isLatched: true,
            performanceSessionActive: true,
            keyboardNotesHeld: false
        ), "explicit quiet focus owns the drum surface until focus exits")
        XCTAssertFalse(AppDelegate.shouldFadeTrackpadOverlayAfterLift(
            performanceSessionActive: true
        ), "a focused drum chart should remain visible between taps")
        XCTAssertTrue(AppDelegate.shouldFadeTrackpadOverlayAfterLift(
            performanceSessionActive: false
        ))
    }

    func testAbsoluteTrackpadFXMapsCenterToNeutral() {
        let values = AppDelegate.absoluteTrackpadFXValues(
            at: CGPoint(x: 0.5, y: 0.5), bendRange: 2, echoEnabled: true
        )
        XCTAssertEqual(values.bend, 0, accuracy: 0.000_001)
        XCTAssertEqual(values.fxX, 0, accuracy: 0.000_001)
        XCTAssertEqual(values.space, 0, accuracy: 0.000_001)
        XCTAssertEqual(values.echo, 0, accuracy: 0.000_001)
    }

    func testAbsoluteTrackpadFXMapsCornersToPitchAndEffects() {
        let upperRight = AppDelegate.absoluteTrackpadFXValues(
            at: CGPoint(x: 1, y: 1), bendRange: 2, echoEnabled: true
        )
        XCTAssertEqual(upperRight.bend, 2, accuracy: 0.000_001)
        XCTAssertEqual(upperRight.echo, 1, accuracy: 0.000_001)
        XCTAssertEqual(upperRight.space, 0, accuracy: 0.000_001)

        let lowerLeft = AppDelegate.absoluteTrackpadFXValues(
            at: CGPoint(x: 0, y: 0), bendRange: 2, echoEnabled: true
        )
        XCTAssertEqual(lowerLeft.bend, -2, accuracy: 0.000_001)
        XCTAssertEqual(lowerLeft.echo, 0, accuracy: 0.000_001)
        XCTAssertEqual(lowerLeft.space, 1, accuracy: 0.000_001)
    }

    func testAbsoluteTrackpadFXKeepsDisabledEchoHalfNeutral() {
        let values = AppDelegate.absoluteTrackpadFXValues(
            at: CGPoint(x: 1, y: 0.5), bendRange: 2, echoEnabled: false
        )
        XCTAssertEqual(values.fxX, 0, accuracy: 0.000_001)
        XCTAssertEqual(values.echo, 0, accuracy: 0.000_001)
        XCTAssertEqual(values.space, 0, accuracy: 0.000_001)
    }

    func testPitchBendKeepsOnePrimaryTrackpadContact() {
        var primary = TrackpadPrimaryContact()
        let first = TrackpadContact(
            identifier: 41, point: CGPoint(x: 0.2, y: 0.3), state: 3
        )
        let extra = TrackpadContact(
            identifier: 42, point: CGPoint(x: 0.8, y: 0.9), state: 3
        )
        XCTAssertEqual(primary.update([first]), first.point)
        XCTAssertEqual(primary.update([extra, first]), first.point)
        XCTAssertEqual(primary.identifier, first.identifier)
    }

    func testPitchBendDoesNotHandOffUntilEveryFingerLifts() {
        var primary = TrackpadPrimaryContact()
        let first = TrackpadContact(
            identifier: 51, point: CGPoint(x: 0.2, y: 0.3), state: 3
        )
        let extra = TrackpadContact(
            identifier: 52, point: CGPoint(x: 0.8, y: 0.9), state: 4
        )
        _ = primary.update([first])
        XCTAssertNil(primary.update([extra]))
        XCTAssertEqual(primary.identifier, first.identifier)
        XCTAssertNil(primary.update([]))
        XCTAssertNil(primary.identifier)
        XCTAssertEqual(primary.update([extra]), extra.point)
    }

    func testSamePadReplacementRetriggersKitVoice() {
        let first = CGPoint(x: 0.30, y: 0.80)
        let down = TrackpadPercussionPad.transition(
            from: .init(), touches: [first], began: [first]
        )
        let replacement = CGPoint(x: 0.34, y: 0.82)
        let retrigger = TrackpadPercussionPad.transition(
            from: down.state, touches: [replacement], began: [replacement]
        )
        XCTAssertEqual(retrigger.entered, [.kick])
        XCTAssertEqual(retrigger.exited, [.kick])
    }

    func testOneFingerNeverBecomesMultiPointShape() {
        var memory = ShapedownGestureMemory()
        XCTAssertEqual(memory.update(points(1)).count, 1)
        XCTAssertEqual(memory.release().count, 1)
    }

    func testNearlyCoincidentTouchesHaveNoSoftwareDeadZone() {
        var memory = ShapedownGestureMemory()
        let close = [CGPoint(x: 100, y: 100), CGPoint(x: 100.01, y: 100.01)]
        XCTAssertEqual(memory.update(close).count, 2)
        XCTAssertEqual(memory.release(), close)
    }

    func testSinglePointFilterRejectsOneFrameTeleport() {
        var filter = ShapedownSinglePointFilter()
        let size = CGSize(width: 1_000, height: 1_000)
        XCTAssertEqual(filter.update(raw: CGPoint(x: 100, y: 100), canvasSize: size),
                       CGPoint(x: 100, y: 100))
        XCTAssertEqual(filter.update(raw: CGPoint(x: 900, y: 900), canvasSize: size),
                       CGPoint(x: 100, y: 100),
                       "an isolated raw jump must not throw the display")
    }

    func testSinglePointFilterAcceptsConfirmedFastMove() {
        var filter = ShapedownSinglePointFilter()
        let size = CGSize(width: 1_000, height: 1_000)
        _ = filter.update(raw: CGPoint(x: 100, y: 100), canvasSize: size)
        _ = filter.update(raw: CGPoint(x: 700, y: 700), canvasSize: size)
        let confirmed = filter.update(raw: CGPoint(x: 710, y: 710), canvasSize: size)
        XCTAssertGreaterThan(confirmed.x, 100)
        XCTAssertGreaterThan(confirmed.y, 100)
    }

    func testNotepatCKeySelectsBaseRed() throws {
        let color = try XCTUnwrap(ShapedownPalette.color(forKeyCode: 8)) // C
        assertRGB(color, 255, 50, 50)
    }

    func testNotepatNaturalsFollowSharedPalette() throws {
        assertRGB(try XCTUnwrap(ShapedownPalette.color(forKeyCode: 2)), 255, 160, 0) // D
        assertRGB(try XCTUnwrap(ShapedownPalette.color(forKeyCode: 3)), 50, 200, 50)  // F
        assertRGB(try XCTUnwrap(ShapedownPalette.color(forKeyCode: 5)), 50, 120, 255) // G
    }

    func testAccidentalKeyStaysBlack() throws {
        assertRGB(try XCTUnwrap(ShapedownPalette.color(forKeyCode: 9)), 0, 0, 0) // V = C#
    }

    private func transition(_ state: TrackpadPercussionPad.State,
                            _ touches: [CGPoint]) -> TrackpadPercussionPad.Transition {
        TrackpadPercussionPad.transition(from: state, touches: touches)
    }

    private func points(_ count: Int) -> [CGPoint] {
        (0..<count).map { CGPoint(x: CGFloat($0 * 20), y: CGFloat($0 * 10)) }
    }

    private func assertRGB(_ color: NSColor, _ r: Int, _ g: Int, _ b: Int,
                           file: StaticString = #filePath, line: UInt = #line) {
        let rgb = color.usingColorSpace(.deviceRGB)
        XCTAssertEqual(rgb?.redComponent ?? -1, CGFloat(r) / 255, accuracy: 0.001,
                       file: file, line: line)
        XCTAssertEqual(rgb?.greenComponent ?? -1, CGFloat(g) / 255, accuracy: 0.001,
                       file: file, line: line)
        XCTAssertEqual(rgb?.blueComponent ?? -1, CGFloat(b) / 255, accuracy: 0.001,
                       file: file, line: line)
    }
}
#endif
