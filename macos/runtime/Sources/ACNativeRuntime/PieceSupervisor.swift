import CryptoKit
import Foundation

public struct ACPieceBundle: Sendable, Equatable {
  public var slug: String; public var version: String; public var source: Data; public var sha256: String
  public init(slug: String, version: String, source: Data, sha256: String) {
    self.slug = slug; self.version = version; self.source = source; self.sha256 = sha256
  }
}
public struct ACJSLimits: Sendable, Equatable {
  public var maxSourceBytes = 2 * 1024 * 1024; public var maxHeapBytes = 32 * 1024 * 1024
  public var maxCallbackMicroseconds: UInt64 = 8_000; public var probationCallbacks = 120
  public init() {}
}
public protocol ACJSPiece: ACPiece { var slug: String { get }; var version: String { get } }
public protocol ACJSEngine: AnyObject {
  // JavaScriptCore/QuickJS adapters expose only ACApi bindings, never DOM or arbitrary native APIs.
  func compile(_ bundle: ACPieceBundle, limits: ACJSLimits) throws -> any ACJSPiece
}
public protocol ACPieceBundleSource: AnyObject, Sendable { func fetch(slug: String) async throws -> ACPieceBundle }

public enum ACRuntimeError: Error, Equatable {
  case sourceTooLarge, digestMismatch, noStagedPiece, callbackBudgetExceeded, callbackFailed
}

public enum ACDigest {
  public static func sha256(_ data: Data) -> String {
    SHA256.hash(data: data).map { String(format: "%02x", $0) }.joined()
  }
}

// Main-actor isolation makes frame-boundary stage/activate/rollback atomic.
// Remote loaders hop to the render actor only after their network work finishes.
@MainActor public final class ACPieceSupervisor {
  private let engine: ACJSEngine; private let limits: ACJSLimits
  private var active: (any ACJSPiece)?; private var staged: (any ACJSPiece)?; private var fallback: (any ACJSPiece)?
  private var probationRemaining = 0
  public private(set) var generation: UInt64 = 0
  public init(engine: ACJSEngine, limits: ACJSLimits = .init()) { self.engine = engine; self.limits = limits }

  public func fetchAndStage(slug: String, from source: ACPieceBundleSource, api: ACApi) async throws {
    try stage(await source.fetch(slug: slug), api: api)
  }
  public func stage(_ bundle: ACPieceBundle, api: ACApi) throws {
    guard bundle.source.count <= limits.maxSourceBytes else { throw ACRuntimeError.sourceTooLarge }
    guard ACDigest.sha256(bundle.source) == bundle.sha256.lowercased() else { throw ACRuntimeError.digestMismatch }
    let candidate = try engine.compile(bundle, limits: limits)
    try candidate.boot(api)
    staged = candidate
  }
  public func activate(api: ACApi) throws {
    guard let candidate = staged else { throw ACRuntimeError.noStagedPiece }
    if let current = active { current.leave(api); fallback = current }
    active = candidate; staged = nil; probationRemaining = limits.probationCallbacks; generation &+= 1
  }
  public func rollback(api: ACApi) -> Bool {
    guard let previous = fallback else { return false }
    active?.leave(api); active = previous; fallback = nil; probationRemaining = 0; generation &+= 1
    return true
  }
  public func sim(api: ACApi) throws { try invoke(api: api) { try $0.sim(api) }; api.simCount &+= 1 }
  public func paint(api: ACApi) throws { try invoke(api: api) { try $0.paint(api) }; api.paintCount &+= 1 }
  public func act(api: ACApi, event: ACEvent) throws { try invoke(api: api) { try $0.act(api, event: event) } }

  private func invoke(api: ACApi, _ callback: (any ACJSPiece) throws -> Void) throws {
    guard let piece = active else { return }
    let start = ContinuousClock.now
    do { try callback(piece) } catch {
      _ = rollback(api: api); throw ACRuntimeError.callbackFailed
    }
    let elapsed = start.duration(to: .now)
    let microseconds = UInt64(max(0, elapsed.components.seconds * 1_000_000 + elapsed.components.attoseconds / 1_000_000_000_000))
    if microseconds > limits.maxCallbackMicroseconds {
      _ = rollback(api: api); throw ACRuntimeError.callbackBudgetExceeded
    }
    if probationRemaining > 0 { probationRemaining -= 1 }
    if probationRemaining == 0 { fallback = nil }
  }
}
