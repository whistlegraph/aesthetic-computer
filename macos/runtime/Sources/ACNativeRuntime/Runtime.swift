import Foundation

public struct ACEvent: Sendable, Equatable {
  public var name: String
  public var value: Float
  public var timestampMicroseconds: UInt64
  public init(_ name: String, value: Float = 1, timestampMicroseconds: UInt64 = 0) {
    self.name = name; self.value = value; self.timestampMicroseconds = timestampMicroseconds
  }
}

public struct ACScreen: Sendable, Equatable {
  public var width: Int; public var height: Int; public var scale: Float
  public init(width: Int = 1920, height: Int = 1080, scale: Float = 1) {
    self.width = width; self.height = height; self.scale = scale
  }
}

public struct ACClock: Sendable, Equatable {
  public var monotonicMicroseconds: UInt64 = 0
  public var unixMilliseconds: Int64 = 0
  public var seconds: Double = 0
  public init() {}
}

public struct ACSystem: Sendable, Equatable {
  public var platform = "macos"
  public var version = ""
  public var handle = ""
  public var dark = true
  public var online = false
  public init() {}
}

public struct ACGamepadState: Sendable, Equatable {
  public var leftX: Float = 0, leftY: Float = 0, rightX: Float = 0, rightY: Float = 0
  public var leftTrigger: Float = 0, rightTrigger: Float = 0
  public var down: Set<String> = []
  public init() {}
  public func pressed(_ button: String) -> Bool { down.contains(button) }
}

public struct ACColor: Sendable, Equatable {
  public var red: UInt8, green: UInt8, blue: UInt8, alpha: UInt8
  public init(_ red: UInt8, _ green: UInt8, _ blue: UInt8, _ alpha: UInt8 = 255) {
    self.red = red; self.green = green; self.blue = blue; self.alpha = alpha
  }
}

public struct ACTransform: Sendable, Equatable {
  public var matrix: [Float]
  public init(matrix: [Float] = [1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1]) { self.matrix = matrix }
}

public protocol ACGraphics: AnyObject {
  func wipe(_ color: ACColor)
  func box(x: Float, y: Float, width: Float, height: Float, color: ACColor)
  func line(x1: Float, y1: Float, x2: Float, y2: Float, width: Float, color: ACColor)
  func write(_ text: String, x: Float, y: Float, size: Float, color: ACColor)
}

// Handles are opaque native resources owned by the Metal host.
public struct ACModel: Sendable, Hashable { public let id: UInt64; public init(id: UInt64) { self.id = id } }
public struct ACTexture: Sendable, Hashable { public let id: UInt64; public init(id: UInt64) { self.id = id } }
public struct ACShader: Sendable, Hashable { public let id: UInt64; public init(id: UInt64) { self.id = id } }

public protocol ACRenderer: AnyObject {
  func loadModel(bytes: Data, format: String) throws -> ACModel
  func loadTexture(bytes: Data, format: String) throws -> ACTexture
  func makeShader(source: String, entryPoint: String) throws -> ACShader
  func draw(model: ACModel, transform: ACTransform, texture: ACTexture?, shader: ACShader?)
}

public struct ACSynthVoice: Sendable, Equatable {
  public var frequencyHz: Float = 440, durationSeconds: Float = 0.1, volume: Float = 0.25
  public var attackSeconds: Float = 0.001
  public var wave = "sine"
  public init() {}
}

public protocol ACSound: AnyObject {
  // Implementations submit immediately to a preallocated Core Audio render path.
  func synth(_ voice: ACSynthVoice)
  func playImmediate(buffer: UInt64, volume: Float)
  func stopAll()
  var sampleRate: Int { get }
}

public protocol ACNetwork: AnyObject {
  func request(_ request: URLRequest) async throws -> (Data, URLResponse)
}
public protocol ACStorage: AnyObject {
  func data(for key: String) throws -> Data?
  func set(_ data: Data?, for key: String) throws
}
public struct ACTelemetry: Sendable, Equatable {
  public var type: String; public var requestID: String; public var timestampMicroseconds: UInt64; public var json: String
  public init(type: String, requestID: String = "", timestampMicroseconds: UInt64 = 0, json: String = "{}") {
    self.type = type; self.requestID = requestID; self.timestampMicroseconds = timestampMicroseconds; self.json = json
  }
}
public protocol ACTelemetrySink: AnyObject { func send(_ event: ACTelemetry) }

public final class ACApi {
  public var screen: ACScreen; public var clock = ACClock(); public var system = ACSystem()
  public var gamepad = ACGamepadState(); public var simCount: UInt64 = 0; public var paintCount: UInt64 = 0
  public let graphics: ACGraphics; public let renderer: ACRenderer; public let sound: ACSound
  public let network: ACNetwork; public let storage: ACStorage; public let telemetry: ACTelemetrySink
  public init(screen: ACScreen = .init(), graphics: ACGraphics, renderer: ACRenderer, sound: ACSound,
              network: ACNetwork, storage: ACStorage, telemetry: ACTelemetrySink) {
    self.screen = screen; self.graphics = graphics; self.renderer = renderer; self.sound = sound
    self.network = network; self.storage = storage; self.telemetry = telemetry
  }
}

public protocol ACPiece: AnyObject {
  func boot(_ api: ACApi) throws
  func sim(_ api: ACApi) throws
  func paint(_ api: ACApi) throws
  func act(_ api: ACApi, event: ACEvent) throws
  func leave(_ api: ACApi)
}
public extension ACPiece { func boot(_ api: ACApi) throws {}; func leave(_ api: ACApi) {} }

public enum ACInputMap {
  // GameController and AppKit adapters emit these stable piece-level names.
  public static let keyboard: [UInt16: String] = [
    0x00: "a", 0x01: "s", 0x02: "d", 0x0D: "w", 0x7B: "ArrowLeft", 0x7C: "ArrowRight",
    0x7D: "ArrowDown", 0x7E: "ArrowUp", 0x24: "Enter", 0x31: "Space", 0x35: "Escape"
  ]
  public static let gameController: [String: String] = [
    "buttonA": "a", "buttonB": "b", "buttonX": "x", "buttonY": "y",
    "leftShoulder": "lb", "rightShoulder": "rb", "leftTrigger": "lt", "rightTrigger": "rt",
    "dpad.up": "ArrowUp", "dpad.down": "ArrowDown", "dpad.left": "ArrowLeft", "dpad.right": "ArrowRight",
    "buttonMenu": "Enter"
  ]
}
