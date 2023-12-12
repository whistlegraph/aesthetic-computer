import SwiftUI
import WebKit
import Network

let grey: CGFloat = 32/255;

class Coordinator: NSObject, WKScriptMessageHandler {
    // Handle JavaScript messages here
    func userContentController(_ userContentController: WKUserContentController, didReceive message: WKScriptMessage) {
        if message.name == "iOSAppLog" {
                print("JavaScript Log: \(message.body)")
            } 
    }
}

struct WebView: UIViewRepresentable {
    var url: String
    var isOnline: Bool
    
    func makeCoordinator() -> Coordinator {
        return Coordinator()
    }
    
    func makeUIView(context: Context) -> WKWebView {
        let config = WKWebViewConfiguration()
        let userScript = WKUserScript(source: "console.log = function() { window.webkit.messageHandlers.iOSAppLog.postMessage([...arguments].join(' ')); }",
                                      injectionTime: .atDocumentStart,
                                      forMainFrameOnly: false)
        config.allowsInlineMediaPlayback = true
        config.userContentController.addUserScript(userScript)
        config.userContentController.add(context.coordinator, name: "iOSAppLog")
        config.userContentController.add(context.coordinator, name: "iOSApp")
        let webView = WKWebView(frame: .zero, configuration: config)
        webView.backgroundColor = UIColor(red: grey, green: grey, blue: grey, alpha: 1)
        webView.isOpaque = false
        
        webView.customUserAgent = "Aesthetic"
        
        
        // Add a script message handler to handle messages from JavaScript
        AppDelegate.shared?.appWebView = webView // Set the shared appWebView
        return webView
    }
    
    func updateUIView(_ webView: WKWebView, context: Context) {
        print("URL: ", url)
        let request = URLRequest(url: URL(string: url)!)
        webView.load(request)
        
        // Call JavaScript function if online
        if isOnline {
            let script = "shareImage();" // Assuming the JavaScript function is named 'shareImage'
            webView.evaluateJavaScript(script, completionHandler: nil)
        }
    }
}

struct ContentView: View {
    @State private var isOnline: Bool? = nil
    
    var body: some View {
        GeometryReader { geometry in
            VStack {
                if let isOnline = isOnline {
                    if isOnline {
                        WebView(url: "https://aesthetic.computer", isOnline: true)
                    } else {
                        let test = (Bundle.main.url(forResource: "offline", withExtension: "html", subdirectory: "html")?.absoluteString ?? "")
                        WebView(url: test, isOnline: false)
                        
                    }
                } else {
                    Color(red: grey, green: grey, blue: grey)
                        .frame(maxWidth: .infinity, maxHeight: .infinity)
                }
            }
            .onAppear {
                let monitor = NWPathMonitor()
                monitor.pathUpdateHandler = { path in
                    self.isOnline = path.status == .satisfied
                    if self.isOnline == true {
                        monitor.cancel() //stops monitoring once you are online
                    }
                }
                let queue = DispatchQueue(label: "NetworkMonitor")
                monitor.start(queue: queue)
            }
            .padding(.bottom, geometry.safeAreaInsets.bottom > 0 ? 24 : 0)
            .background(Color(red: grey, green: grey, blue: grey))
            .ignoresSafeArea(.keyboard, edges: .bottom)
        }
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
