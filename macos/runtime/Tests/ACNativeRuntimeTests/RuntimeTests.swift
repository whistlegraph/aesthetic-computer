import Foundation
import Testing
@testable import ACNativeRuntime

final class NullHost: ACGraphics, ACRenderer, ACSound, ACNetwork, ACStorage, ACTelemetrySink {
  var sampleRate = 48_000
  func wipe(_ color: ACColor) {}; func box(x: Float,y: Float,width: Float,height: Float,color: ACColor) {}
  func line(x1: Float,y1: Float,x2: Float,y2: Float,width: Float,color: ACColor) {}
  func write(_ text: String,x: Float,y: Float,size: Float,color: ACColor) {}
  func loadModel(bytes: Data, format: String) throws -> ACModel { .init(id: 1) }
  func loadTexture(bytes: Data, format: String) throws -> ACTexture { .init(id: 1) }
  func makeShader(source: String, entryPoint: String) throws -> ACShader { .init(id: 1) }
  func draw(model: ACModel, transform: ACTransform, texture: ACTexture?, shader: ACShader?) {}
  func synth(_ voice: ACSynthVoice) {}; func playImmediate(buffer: UInt64, volume: Float) {}; func stopAll() {}
  func request(_ request: URLRequest) async throws -> (Data, URLResponse) { fatalError() }
  func data(for key: String) throws -> Data? { nil }; func set(_ data: Data?, for key: String) throws {}
  func send(_ event: ACTelemetry) {}
}
final class FakePiece: ACJSPiece {
  let slug: String; let version: String; var fail = false; var sims = 0
  init(_ slug: String, _ version: String) { self.slug = slug; self.version = version }
  func sim(_ api: ACApi) throws { sims += 1; if fail { throw ACRuntimeError.callbackFailed } }
  func paint(_ api: ACApi) throws {}; func act(_ api: ACApi, event: ACEvent) throws {}
}
final class FakeEngine: ACJSEngine {
  var pieces: [FakePiece] = []
  func compile(_ bundle: ACPieceBundle, limits: ACJSLimits) throws -> any ACJSPiece {
    let piece = FakePiece(bundle.slug, bundle.version); pieces.append(piece); return piece
  }
}
func bundle(_ slug: String) -> ACPieceBundle {
  let data = Data("export function sim() {}".utf8)
  return .init(slug: slug, version: "1", source: data, sha256: ACDigest.sha256(data))
}

@MainActor @Test func validatesDigestAndMapsInput() async throws {
  let host = NullHost(), engine = FakeEngine(), supervisor = ACPieceSupervisor(engine: engine)
  let api = ACApi(graphics: host, renderer: host, sound: host, network: host, storage: host, telemetry: host)
  var bad = bundle("fight"); bad.sha256 = String(repeating: "0", count: 64)
  #expect(throws: ACRuntimeError.digestMismatch) { try supervisor.stage(bad, api: api) }
  #expect(ACInputMap.keyboard[0x7E] == "ArrowUp")
  #expect(ACInputMap.gameController["buttonX"] == "x")
}

@MainActor @Test func activatesAndRollsBackFailedCandidate() async throws {
  let host = NullHost(), engine = FakeEngine(), supervisor = ACPieceSupervisor(engine: engine)
  let api = ACApi(graphics: host, renderer: host, sound: host, network: host, storage: host, telemetry: host)
  try supervisor.stage(bundle("stable"), api: api); try supervisor.activate(api: api)
  try supervisor.stage(bundle("candidate"), api: api); try supervisor.activate(api: api)
  engine.pieces.last!.fail = true
  #expect(throws: ACRuntimeError.callbackFailed) { try supervisor.sim(api: api) }
  #expect(supervisor.generation == 3)
  try supervisor.sim(api: api)
  #expect(engine.pieces.first!.sims == 1)
}
