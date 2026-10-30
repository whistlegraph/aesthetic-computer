import XCTest
@testable import MenuBand

final class CCDrumFamilyTests: XCTestCase {
    func testStrudelBanksFoldIntoFivePadFamilies() {
        let banks: [String: [String]] = [
            "noe_bd": ["/noe_bd/a.wav", "/noe_bd/b.wav", "/noe_bd/c.wav"],
            "noe_tom": ["/noe_tom/a.wav"],
            "noe_sd": ["/noe_sd/a.wav", "/noe_sd/b.wav"],
            "noe_hh": ["/noe_hh/a.wav", "/noe_hh/b.wav"],
            "noe_cp": ["/noe_cp/a.wav", "/noe_cp/b.wav"],
            "noe_perc": ["/noe_perc/a.wav"],
            "tr909_bd": ["/tr909_bd/1.wav"],
            "tr909_sd": ["/tr909_sd/1.wav"],
            "tr909_hh": ["/tr909_hh/1.wav"],
            "tr909_cp": ["/tr909_cp/1.wav"],
            "loops_only": ["/loop.wav"],
        ]
        let families = MenuBandCCDrums.buildFamilies(from: banks)
        XCTAssertEqual(families.map(\.name), ["noe", "tr909"])

        let noe = families[0]
        XCTAssertEqual(noe.pads.map(\.bank), ["noe_bd", "noe_tom", "noe_sd", "noe_hh", "noe_cp"])
        XCTAssertEqual(noe.pads[0].paths, ["/noe_bd/a.wav"])

        // No toms: the clap fills the tom zone, and the click borrows the hat.
        let tr909 = families[1]
        XCTAssertEqual(tr909.pads.map(\.bank), ["tr909_bd", "tr909_cp", "tr909_sd", "tr909_hh", "tr909_hh"])
    }

    func testFamilyWithoutHatIsSkipped() {
        let banks = ["x_bd": ["/a.wav"], "x_sd": ["/b.wav"], "x_cp": ["/c.wav"]]
        XCTAssertTrue(MenuBandCCDrums.buildFamilies(from: banks).isEmpty)
    }
}

extension CCDrumFamilyTests {
    func testBorrowedBankPlaysADifferentHitThanItsOwner() {
        let banks = [
            "x_bd": ["/bd/1.wav", "/bd/2.wav"],
            "x_sd": ["/sd/1.wav"],
            "x_hh": ["/hh/1.wav", "/hh/2.wav"],
            "x_cp": ["/cp/1.wav"],
        ]
        let pads = MenuBandCCDrums.buildFamilies(from: banks)[0].pads
        XCTAssertEqual(pads.map(\.paths), [["/bd/1.wav"], ["/cp/1.wav"], ["/sd/1.wav"],
                                           ["/hh/1.wav"], ["/hh/2.wav"]])
    }
}

extension CCDrumFamilyTests {
    func testKeysSpreadOverStandardNames() {
        let banks: [String: [String]] = [
            "k_bd": ["/bd/1.wav"], "k_sd": ["/sd/1.wav"], "k_hh": ["/hh/1.wav", "/hh/2.wav"],
            "k_oh": ["/oh/1.wav"], "k_cp": ["/cp/1.wav"], "k_rim": ["/rim/1.wav"],
            "k_cr": ["/cr/1.wav", "/cr/2.wav"], "k_rd": ["/rd/1.wav"], "k_lt": ["/lt/1.wav"],
            "k_cb": ["/cb/1.wav"], "k_tb": ["/tb/1.wav"],
        ]
        let keys = MenuBandCCDrums.buildFamilies(from: banks)[0].keys
        // kick crash snare splash clap snap cowbell hatC block hatO tambo ride
        XCTAssertEqual(keys.map(\.bank), ["k_bd", "k_cr", "k_sd", "k_cr", "k_cp", "k_rim",
                                          "k_cb", "k_hh", "k_lt", "k_oh", "k_tb", "k_rd"])
        // Crash and splash share the cr bank but not a file.
        XCTAssertEqual(keys[1].paths, ["/cr/1.wav"])
        XCTAssertEqual(keys[3].paths, ["/cr/2.wav"])
    }

    func testMinimalFamilyKeysFallBackToRelatives() {
        let banks = ["m_bd": ["/bd.wav"], "m_sd": ["/sd.wav"], "m_hh": ["/hh.wav"]]
        let keys = MenuBandCCDrums.buildFamilies(from: banks)[0].keys
        // Every relative is a single file, so keys repeat the nearest one.
        XCTAssertEqual(keys.map(\.bank), ["m_bd", "m_hh", "m_sd", "m_hh", "m_sd", "m_sd",
                                          "m_hh", "m_hh", "m_sd", "m_hh", "m_hh", "m_hh"])
    }

    func testSpentBankHandsTheKeyToTheNextName() {
        let banks = ["s_bd": ["/bd.wav"], "s_sd": ["/sd.wav"], "s_hh": ["/hh1.wav", "/hh2.wav"],
                     "s_oh": ["/oh.wav"]]
        let keys = MenuBandCCDrums.buildFamilies(from: banks)[0].keys
        // crash takes oh's only file; splash moves on to hh instead of repeating it.
        XCTAssertEqual(keys[1].paths, ["/oh.wav"])
        XCTAssertEqual(keys[3].paths, ["/hh1.wav"])
    }
}
