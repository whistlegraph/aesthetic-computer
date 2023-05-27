import SwiftUI
import WebKit

let grey: CGFloat = 32 / 255;

struct WebView: UIViewRepresentable {
    var url: String

    func makeUIView(context: Context) -> WKWebView {
        let webConfiguration = WKWebViewConfiguration()
        let webView = WKWebView(frame: .zero, configuration: webConfiguration)
        webView.backgroundColor = UIColor(red: grey, green: grey, blue: grey, alpha: 1)
        webView.isOpaque = false
        return webView
    }
    
    func updateUIView(_ webView: WKWebView, context: Context) {
        let request = URLRequest(url: URL(string: url)!)
        webView.load(request)
    }
}

struct ContentView: View {
    var body: some View {
        VStack {
            WebView(url: "https://www.aesthetic.computer")
        }
        .padding(4)
        .ignoresSafeArea(.keyboard)
        .background(Color(red: grey, green: grey, blue: grey))
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
