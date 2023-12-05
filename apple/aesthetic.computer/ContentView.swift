import SwiftUI
import WebKit
import Network


let grey: CGFloat = 32/255;

struct WebView: UIViewRepresentable {
    var url: String
    
    func makeUIView(context: Context) -> WKWebView {
        let webConfiguration = WKWebViewConfiguration()
        webConfiguration.allowsInlineMediaPlayback = true
        let webView = WKWebView(frame: .zero, configuration: webConfiguration)
        webView.backgroundColor = UIColor(red: grey, green: grey, blue: grey, alpha: 1)
        webView.isOpaque = false
        
        webView.customUserAgent = "Aesthetic"
        return webView
    }
    
    func updateUIView(_ webView: WKWebView, context: Context) {
        print("URL: ", url)
        let request = URLRequest(url: URL(string: url)!)
        webView.load(request)
    }
}

struct ContentView: View


{
    
    @State private var isOnline: Bool? = nil
    
    var body: some View {
        GeometryReader { geometry in
            VStack {
                if let isOnline = isOnline {
                    if isOnline {
                        WebView(url: "https://aesthetic.computer")
                    } else {
                        let test = (Bundle.main.url(forResource: "offline", withExtension: "html", subdirectory: "html")?.absoluteString ?? "")
                        WebView(url: test)
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
