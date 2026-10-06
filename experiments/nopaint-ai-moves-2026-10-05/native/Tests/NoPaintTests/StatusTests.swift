import XCTest
import SwiftUI
@testable import NoPaint

@MainActor final class StatusTests: XCTestCase {
    func state(_ error: String) throws -> GameState {
        let json: [String: Any] = ["revision":1,"ready":true,"busy":false,
            "accepted":"/image/input.png","can_undo":false,"strength":0.5,
            "engine":"evolve","selection":"evolve","can_reject":false,"models":[],"trace":[:],
            "quote":["id":"quote","engine":"evolve","braincells":0,"estimated_seconds":4.0],
            "account":["connected":true,"handle":"@test","remaining":50000,"purchased":0,
                       "stale":true,"reconnecting":true,"error":error,"error_code":"offline"]]
        return try JSONDecoder().decode(GameState.self,from:JSONSerialization.data(withJSONObject:json))
    }
    func testConnectionDetailsStayOutOfPaintAndDoNotResizeTheCanvas() async throws {
        let game=GameStore();game.state=try state("Timed out")
        game.imagePath="/image/input.png";game.imageSettled=true
        XCTAssertEqual(game.generationStatus(Date()),["~4.0s","0 braincells"])
        XCTAssertTrue(game.canPaint)
        XCTAssertFalse(game.canDone)
        XCTAssertEqual(game.statusIssue?.title,"AC offline")
        let view=ContentView(game:game), size=CGSize(width:350,height:600)
        let before=view.canvasSide(size,status:view.status(Date()),compact:false)
        game.state=try state(String(repeating:"Long network error details. ",count:80))
        XCTAssertEqual(view.canvasSide(size,status:view.status(Date()),compact:false),before)
        XCTAssertEqual(game.generationStatus(Date()),["~4.0s","0 braincells"])
        game.actionError="An action failed with a detailed explanation"
        XCTAssertEqual(game.statusIssue?.title,"Action failed")
        XCTAssertEqual(game.generationStatus(Date()),["~4.0s","0 braincells"])
    }

    func testSmallSlabTileKeepsOneStatusLineAndRoomForThePainting() async throws {
        let game=GameStore(); game.state=try state("Timed out")
        let view=ContentView(game:game)
        let content=StatusBand.Content(groups:[
            [.init(text:"A very long remote image model name",size:13,weight:.semibold), .init(text:"AC cloud · OpenRouter")],
            [.init(text:"AC offline")],
            [.init(text:"@jeffrey"),.init(text:"5,430,687 left",numeric:true)]])
        for width: CGFloat in [220,350,465,700] {
            let layout=StatusBand.layout(content,width:width)
            XCTAssertEqual(layout.height,33)
            XCTAssertEqual(Set(layout.runs.map { $0.frame.minY }).count,1)
            for run in layout.runs {
                XCTAssertLessThanOrEqual(run.frame.maxX,width)
                XCTAssertLessThanOrEqual(run.frame.maxY,layout.height)
                XCTAssertGreaterThan(run.frame.width,0)
            }
        }
        let side=view.canvasSide(CGSize(width:465,height:252),status:content,compact:true)
        XCTAssertGreaterThan(side,160)
        XCTAssertGreaterThanOrEqual(252-side-StatusBand.lineHeight,52)
        XCTAssertEqual(side,view.canvasSide(CGSize(width:465,height:252),status:view.status(Date()),compact:true))
    }
    func testPaintsLeftUsesWholeAffordableMovesAndMarksLastKnownBalances() async throws {
        let game=GameStore();game.state=try state("Timed out")
        XCTAssertEqual(game.paintsLeft(8000),"~6")
        XCTAssertEqual(game.paintsLeft(60000),"~0")
        XCTAssertEqual(game.paintsLeft(0),"∞")
        XCTAssertEqual(game.paintsLeft(nil),"—")
    }
}
