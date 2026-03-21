//
//  MessagesViewController.swift
//  aesthetic
//
//  Created by Rumi Tarighian on 12/5/23.
//

import UIKit
import Messages
import WebKit

class MessagesViewController: MSMessagesAppViewController, WKScriptMessageHandler{
    func userContentController(_ userContentController: WKUserContentController, didReceive message: WKScriptMessage) {
        if message.name == "iMessageExtension", let jsonString = message.body as? String {
            print("Received iMessageExtension: \(jsonString)")
            
            // Convert JSON string to Dictionary
            if let data = jsonString.data(using: .utf8) {
                do {
                    if let dictionary = try JSONSerialization.jsonObject(with: data, options: []) as? [String: Any] {
                        // Now 'dictionary' is a Swift dictionary
                        print("JSON as dictionary: \(dictionary)")
                        
                        // Handle the dictionary as needed
                        if let type = dictionary["type"] as? String, type == "tap" {
                            if let image = convertDataUrlToImage(dataUrl: dictionary["body"] as! String) {
                                displayFixedImageSticker(picture: image)
                            } else {
                                print("Image not found in the assets.")
                            }                        }
                    }
                } catch {
                    print("Error parsing JSON: \(error)")
                }
            }
        }
        else if message.name == "iMessageExtensionLog" {
            print("JavaScript Log: \(message.body)")
        }
    }
    
    func convertDataUrlToImage(dataUrl: String) -> UIImage? {
        // Check if the string is a valid data URL
        guard dataUrl.hasPrefix("data:image/png;base64,"),
              let base64String = dataUrl.split(separator: ",").last,
              let imageData = Data(base64Encoded: String(base64String)) else {
            print("Invalid data URL")
            return nil
        }

        // Create an image from the data
        let image = UIImage(data: imageData)
        return image
    }
    var webView: WKWebView!
    let grey: CGFloat = 32/255;
    
    override func viewDidLoad() {
        super.viewDidLoad()
        let config = WKWebViewConfiguration()
        let userScript = WKUserScript(source: "console.log = function() { window.webkit.messageHandlers.iMessageExtensionLog.postMessage([...arguments].join(' ')); }",
                                      injectionTime: .atDocumentStart,
                                      forMainFrameOnly: false)
        config.userContentController.addUserScript(userScript)
        config.userContentController.add(self, name: "iMessageExtension")
        config.userContentController.add(self, name: "iMessageExtensionLog")
        
        webView = WKWebView(frame: view.bounds, configuration: config)
        webView.load(URLRequest(url: URL(string: "https://aesthetic.computer/imessage")!))
        webView.customUserAgent = "AestheticExtension"
        webView.backgroundColor = UIColor(red: grey, green: grey, blue: grey, alpha: 1)
        webView.isOpaque = false
        
        view.addSubview(webView)
        
        webView.backgroundColor = .black
    }
    
    func displayFixedImageSticker(picture: UIImage) {
        // Ensure there is an active conversation
        guard let conversation = activeConversation else {
            print("Active conversation is nil")
            return
        }

        let tempDirectory = NSTemporaryDirectory()
        let tempFileURL = URL(fileURLWithPath: tempDirectory).appendingPathComponent("tempImage.png")

        do {
            // Write the image data to the temporary file
            if let pngData = picture.pngData() {
                try pngData.write(to: tempFileURL)
                
                // Insert the attachment
                conversation.insertAttachment(tempFileURL, withAlternateFilename: nil, completionHandler: { error in
                    if let error = error {
                        print("Error inserting attachment: \(error)")
                    }
                })
            } else {
                print("Failed to get PNG data from image")
            }
        } catch {
            print("Failed to write image to temporary file: \(error)")
        }
    }
    override func willBecomeActive(with conversation: MSConversation) {
        // Called when the extension is about to move from the inactive to active state.
        // This will happen when the extension is about to present UI.
        
        // Use this method to configure the extension and restore previously stored state.
    }
    
    override func didResignActive(with conversation: MSConversation) {
        // Called when the extension is about to move from the active to inactive state.
        // This will happen when the user dismisses the extension, changes to a different
        // conversation or quits Messages.
        
        // Use this method to release shared resources, save user data, invalidate timers,
        // and store enough state information to restore your extension to its current state
        // in case it is terminated later.
    }
    
    override func didReceive(_ message: MSMessage, conversation: MSConversation) {
        // Called when a message arrives that was generated by another instance of this
        // extension on a remote device.
        
        // Use this method to trigger UI updates in response to the message.
    }
    
    override func didStartSending(_ message: MSMessage, conversation: MSConversation) {
        // Called when the user taps the send button.
    }
    
    override func didCancelSending(_ message: MSMessage, conversation: MSConversation) {
        // Called when the user deletes the message without sending it.
        // Use this to clean up state related to the deleted message.
    }
    
    override func willTransition(to presentationStyle: MSMessagesAppPresentationStyle) {
        // Called before the extension transitions to a new presentation style.
        
        // Use this method to prepare for the change in presentation style.
    }
    
    override func didTransition(to presentationStyle: MSMessagesAppPresentationStyle) {
        super.didTransition(to: presentationStyle)
        
        switch presentationStyle {
        case .compact:
            print("Extension has contracted")
            webView.evaluateJavaScript("iMessageExtensionResize('contract')", completionHandler: nil)
        case .expanded:
            print("Extension has expanded")
            webView.evaluateJavaScript("iMessageExtensionResize('expand')", completionHandler: nil)
        case .transcript:
            print("whatever...")
        @unknown default:
            fatalError("Unknown presentation style encountered")
        }
    }
    
}
