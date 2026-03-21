import SwiftUI
import WebKit
import Network

let grey: CGFloat = 32/255;

class Coordinator: NSObject, WKScriptMessageHandler, WKUIDelegate {
   
    //@available(iOS 15.0, *)
    func webView(_ webView: WKWebView,
        decideMediaCapturePermissionsFor origin: WKSecurityOrigin,
        initiatedBy frame: WKFrameInfo,
        type: WKMediaCaptureType) async -> WKPermissionDecision {
            return .grant;
    }
    
    // Handle JavaScript messages here
    func userContentController(_ userContentController: WKUserContentController, didReceive message: WKScriptMessage) {
        if message.name == "iOSAppLog" {
            print("JavaScript Log: \(message.body)")
        }
        else if message.name == "iOSApp", let jsonString = message.body as? String {
            
            // Convert JSON string to Dictionary
            print("JavaScript Log: \(message.body)")
            if let data = jsonString.data(using: .utf8) {
                do {
                    if let dictionary = try JSONSerialization.jsonObject(with: data, options: []) as? [String: Any] {
                        // Now 'dictionary' is a Swift dictionary
                        print("JSON as dictionary: \(dictionary)")
                        
                        // Handle the dictionary as needed
                        if let type = dictionary["type"] as? String {
                            switch type {
                            case "notifications":
                                if let body = dictionary["body"] as? Bool, body == true {
                                    AppDelegate.shared?.triggerSubscribe()
                                    showAlert(title: "SUBSCRIBED", message: "You have successfully subscribed to notifications. Type \"nonotifs\" to unsubscribe.")
                                } else if let body = dictionary["body"] as? Bool, body == false {
                                    AppDelegate.shared?.triggerUnsubscribe()
                                    showAlert(title: "UNSUBSCRIBED", message: "You have successfully unsubscribed to notifications. Type \"notifs\" to subscribe.")
                                }

                            case "url":
                                if let urlString = dictionary["body"] as? String, let url = URL(string: urlString) {
                                    UIApplication.shared.open(url, options: [:], completionHandler: nil)
                                }

                            default:
                                print("Unhandled type: \(type)")
                            }
                        }
                    }
                } catch {
                    print("Error parsing JSON: \(error)")
                }
            }
        }
    }
    
    func showAlert(title: String, message: String) {
        let alert = UIAlertController(title: title, message: message, preferredStyle: .alert)
        alert.addAction(UIAlertAction(title: "OK", style: .default, handler: nil))

        // Find the active window scene
        if let windowScene = UIApplication.shared.connectedScenes.first as? UIWindowScene,
           let rootViewController = windowScene.windows.first?.rootViewController {
            
            // Present the alert from the top-most view controller
            var currentController = rootViewController
            while let presentedController = currentController.presentedViewController {
                currentController = presentedController
            }
            currentController.present(alert, animated: true, completion: nil)
        }
    }
    
//     func webView(_ webView: WKWebView, decidePolicyFor navigationAction: WKNavigationAction, decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
//         print("link clicked...");
//         if navigationAction.targetFrame == nil, let url = navigationAction.request.url {
//             print("opening...")
//             UIApplication.shared.open(url, options: [:], completionHandler: nil)
//             decisionHandler(.cancel)
//             return
//         }
//         decisionHandler(.allow)
//     }
//    
//    func webView(_ webView: WKWebView, createWebViewWith configuration: WKWebViewConfiguration, for navigationAction: WKNavigationAction, windowFeatures: WKWindowFeatures) -> WKWebView? {
//          if let url = navigationAction.request.url {
//              webView.load(URLRequest(url: url))
//          }
//          return nil
//      }
    
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
//            webView.navigationDelegate = context.coordinator
            webView.uiDelegate = context.coordinator
            webView.customUserAgent = "Aesthetic"
            
            // Add a script message handler to handle messages from JavaScript
            AppDelegate.shared?.appWebView = webView // Set the shared appWebView
            return webView
        }
        
        func updateUIView(_ webView: WKWebView, context: Context) {
//            let testHTML = "<html><script>window.ontouchstart = () => { console.log('hi'); const a = document.createElement('a'); a.href = 'https://example.com'; a.innerText = 'OKAY'; document.body.appendChild(a); a.click(); }</script><body></body></html>"
//            webView.loadHTMLString(testHTML, baseURL: nil)
             let request = URLRequest(url: URL(string: url)!)
             webView.load(request)
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
