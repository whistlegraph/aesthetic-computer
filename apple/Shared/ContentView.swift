//
//  ContentView.swift
//  Shared
//
//  Created by Jeffrey Alan Scudder on 1/2/22.
//

import SwiftUI

// TODO: https://developer.apple.com/documentation/uikit/pencil_interactions/handling_double_taps_from_apple_pencil

struct ContentView: View {
    var body: some View {
    // Add query parameter for iPad frame spacing.
        
//        WebView(url: "https://192.168.1.245:8080").frame(minWidth: 0, maxWidth: .infinity, minHeight: 0, maxHeight: .infinity)
//            .background(Color.black)
//            .edgesIgnoringSafeArea(.all)
//
        WebView(url: "https://192.168.1.245:8080")
            .padding(30)
            .background(Color.black)
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        Group {
            ContentView()
.previewInterfaceOrientation(.landscapeLeft)
        }
    }
}
