import Foundation
import SwiftUI

public struct ActionSpec: Decodable, Identifiable {
    public let id: String
    public let label: String
    public let key: String?
    public let enabled: String
    public let help: String?
}
public struct ComponentSpec: Decodable, Identifiable {
    public let id: String
    public let kind: String
    public let label: String?
    public let limit: Int?
    public let actions: [ActionSpec]?
}
public struct MenuSpec: Decodable, Identifiable {
    public let id: String
    public let label: String
    public let actions: [ActionSpec]
}
public struct InterfaceSpec: Decodable {
    public struct Layout: Decodable {
        public let canvasPixels, compactHeight, compactControls, controls, statusHeight: Double
        public func paintingSide(width: Double, height: Double) -> Double {
            max(1, floor(min(width, height - (height < compactHeight ? compactControls : controls) - statusHeight)))
        }
    }
    public struct Strength: Decodable { public let label: String; public let value: Double }
    public let version: Int
    public let name: String
    public let layout: Layout
    public let colors: [String: [String]]
    public let components: [ComponentSpec]
    public let strengths: [Strength]
    public let brushes: [Int]
    public let menus: [MenuSpec]
    public static let noPaint: InterfaceSpec = {
        let url = Bundle.module.url(forResource: "nopaint", withExtension: "json")!
        return try! JSONDecoder().decode(InterfaceSpec.self, from: Data(contentsOf: url))
    }()
}

// Platform controls keep their own accessibility and input behavior. The
// component, action order, enablement keys and labels come from one document.
public struct DecisionBar<Content: View>: View {
    let spec: InterfaceSpec
    let content: (ActionSpec) -> Content
    public init(spec: InterfaceSpec = .noPaint, @ViewBuilder content: @escaping (ActionSpec) -> Content) {
        self.spec = spec; self.content = content
    }
    public var body: some View {
        HStack(spacing: 0) {
            ForEach(spec.components.first { $0.kind == "decisions" }?.actions ?? []) { action in content(action) }
        }
    }
}
