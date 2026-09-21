// Hidden native WebKit regression; never orders a window or touches saved notebooks.
// swiftc -parse-as-library apple/aesel/Tests/NotebookLayoutChecks.swift -o /tmp/aesel-notebook-check
// /tmp/aesel-notebook-check apple/aesel/Resources/Session
import AppKit
import WebKit

final class Files: NSObject, WKURLSchemeHandler {
    let root: URL
    init(_ path: String) { root=URL(fileURLWithPath:path) }
    func webView(_ webView: WKWebView, start task: WKURLSchemeTask) {
        let url=task.request.url!, file=root.appendingPathComponent(url.path)
        do {
            let data=try Data(contentsOf:file)
            let types=["html":"text/html","css":"text/css","js":"text/javascript","woff2":"font/woff2","png":"image/png"]
            task.didReceive(URLResponse(url:url,mimeType:types[file.pathExtension] ?? "application/octet-stream",expectedContentLength:data.count,textEncodingName:"utf-8"));task.didReceive(data);task.didFinish()
        } catch { task.didFailWithError(error) }
    }
    func webView(_ webView: WKWebView, stop task: WKURLSchemeTask) {}
}
@MainActor final class Check: NSObject, WKNavigationDelegate {
    let web: WKWebView
    let window: NSWindow
    init(root: String) {
        let config=WKWebViewConfiguration();config.setURLSchemeHandler(Files(root),forURLScheme:"aesel-bundle")
        web=WKWebView(frame:NSRect(x:0,y:0,width:500,height:720),configuration:config)
        window=NSWindow(contentRect:web.frame,styleMask:.borderless,backing:.buffered,defer:false)
        super.init();window.contentView=web;web.navigationDelegate=self
    }
    func start(){web.load(URLRequest(url:URL(string:"aesel-bundle://app/easel/phone/notebook.html")!))}
    func webView(_ webView: WKWebView,didFinish navigation: WKNavigation!) {
        Task { @MainActor in
            do {
                _=try await web.evaluateJavaScript("""
                Object.defineProperty(document, 'hidden', {get: () => false});
                document.body.style.background='#463264';
                document.body.style.backgroundImage='repeating-linear-gradient(to bottom,transparent 0,transparent 23px,rgba(255,255,255,.12) 23px,rgba(255,255,255,.12) 24px)';
                window.updatePhoneNotebook({entries:[{id:'1',kind:'user',text:'This input should wrap across several notebook lines with consistent spacing.\\nAnd this is a second paragraph.'},{id:'2',kind:'assistant',text:'The **output** follows the same rules. Long lines wrap naturally while staying aligned to the paper.\\n\\n### A heading\\n\\n- First item\\n- Second item\\n\\n```js\\nconst radius = 24;\\nwipe("blue");\\n```\\n\\nLast paragraph.'}],handle:'jeffrey',busy:true,activity:'writing'});
                """)
                for width in [500,220,360] {
                    window.setContentSize(NSSize(width:width,height:720))
                    try await Task.sleep(nanoseconds:400_000_000)
                    let result=try await web.evaluateJavaScript("""
                    (()=>{let out=[];for(const b of document.querySelectorAll('article:not(.rich-reply), .rich-reply p, .rich-reply pre, .rich-reply h1, .rich-reply h2, .rich-reply h3, .rich-reply li:not(:has(p))')){let s=document.createElement('span');s.style='display:inline-block;width:0;height:0;vertical-align:baseline';b.prepend(s);let y=s.getBoundingClientRect().top;s.remove();out.push({tag:b.tagName,y,mod:y%24});}return JSON.stringify(out)})()
                    """)
                    let json = result as! String
                    let rows = try JSONSerialization.jsonObject(with: Data(json.utf8)) as! [[String: Any]]
                    precondition(rows.count >= 7)
                    precondition(rows.allSatisfy { abs(($0["mod"] as! Double)) < 0.01 }, "Off-grid output at width \(width): \(json)")
                    print("Aligned input, rich prose, headings, lists and code at width \(width)")
                    let png=try await web.takeSnapshot(configuration:nil)
                    try NSBitmapImageRep(data:png.tiffRepresentation!)!.representation(using:.png,properties:[:])!.write(to:URL(fileURLWithPath:"/tmp/aesel-grid-\(width).png"))
                }
                window.setContentSize(NSSize(width:555,height:720))
                _ = try await web.evaluateJavaScript("""
                window.updatePhoneNotebook({entries:[{id:'single',kind:'user',text:'Make a flower'}],handle:'jeffrey',busy:true,activity:'connecting',exclusion:{width:202,height:98,top:0}});
                """)
                try await Task.sleep(nanoseconds:400_000_000)
                let companion = try await web.evaluateJavaScript("""
                (()=>{const canvas=document.getElementById('notebook-thinking-donkey'),rect=canvas.getBoundingClientRect(),text=document.querySelector('article').getBoundingClientRect();return {hidden:canvas.hidden,top:rect.top,bottom:rect.bottom,left:rect.left,right:rect.right,state:canvas.dataset.state,textTop:text.top,height:reported};})()
                """) as! [String: Any]
                precondition(companion["hidden"] as? Bool == false, "Busy donkey is hidden")
                precondition(companion["state"] as? String == "listening")
                precondition((companion["top"] as! Double) >= 0, "Donkey clipped above notebook")
                precondition((companion["bottom"] as! Double) <= (companion["height"] as! Double), "Donkey clipped below notebook")
                precondition((companion["left"] as! Double) >= 14 && (companion["right"] as! Double) <= 555-14)
                precondition((companion["textTop"] as! Double) < 24, "Text displaced beneath corner preview")
                let png = try await web.takeSnapshot(configuration:nil)
                try NSBitmapImageRep(data:png.tiffRepresentation!)!.representation(using:.png,properties:[:])!.write(to:URL(fileURLWithPath:"/tmp/aesel-donkey-flow.png"))
                print("Busy donkey fits its notebook; first user text flows beside the preview")
                let short = try await web.evaluateJavaScript("window.updatePhoneNotebook({entries:[]}); reported") as! Double
                precondition(short == 24, "Empty transcript did not shrink")
                print("Empty transcript shrinks to one row")
                NSApp.terminate(nil)
            } catch {print(error);exit(1)}
        }
    }
}
@main struct Main {
    @MainActor static func main(){let app=NSApplication.shared;app.setActivationPolicy(.prohibited);let check=Check(root:CommandLine.arguments[1]);check.start();withExtendedLifetime(check){app.run()}}
}
