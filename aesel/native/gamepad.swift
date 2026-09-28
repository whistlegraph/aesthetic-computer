import Foundation
import IOKit.hid

// Match controllers only. Never open keyboard or pointer devices.
let manager=IOHIDManagerCreate(kCFAllocatorDefault,IOOptionBits(kIOHIDOptionsTypeNone))
IOHIDManagerSetDeviceMatchingMultiple(manager,[[kIOHIDDeviceUsagePageKey:1,kIOHIDDeviceUsageKey:5],[kIOHIDDeviceUsagePageKey:1,kIOHIDDeviceUsageKey:4]] as CFArray)
IOHIDManagerScheduleWithRunLoop(manager,CFRunLoopGetMain(),CFRunLoopMode.defaultMode.rawValue)
guard IOHIDManagerOpen(manager,IOOptionBits(kIOHIDOptionsTypeNone)) == kIOReturnSuccess else { exit(1) }
var devices:[(IOHIDDevice,[IOHIDElement],String)] = []
var lastScan=Date.distantPast,lastSent=Date.distantPast,lastData=Data()
func refresh() {
 devices=(IOHIDManagerCopyDevices(manager) as? Set<IOHIDDevice> ?? []).map { device in
  (device,IOHIDDeviceCopyMatchingElements(device,nil,IOOptionBits(kIOHIDOptionsTypeNone)) as? [IOHIDElement] ?? [],IOHIDDeviceGetProperty(device,kIOHIDProductKey as CFString) as? String ?? "Gamepad")
 }.sorted{$0.2 < $1.2}
}
func tick() {
 if Date().timeIntervalSince(lastScan)>1 { refresh();lastScan=Date() }
 var controllers:[[String:Any]]=[]
 for (index,item) in devices.enumerated() {
  let (device,elements,name)=item
  var axes=[Double](repeating:0,count:4),buttons=[Double](repeating:0,count:16)
  for element in elements {
   let kind=IOHIDElementGetType(element)
   if kind != kIOHIDElementTypeInput_Axis && kind != kIOHIDElementTypeInput_Button && kind != kIOHIDElementTypeInput_Misc {continue}
   let empty=IOHIDValueCreateWithIntegerValue(kCFAllocatorDefault,element,0,0)
   var ref=Unmanaged.passUnretained(empty)
   guard IOHIDDeviceGetValue(device,element,&ref)==kIOReturnSuccess else {continue}
   let value=Double(IOHIDValueGetIntegerValue(ref.takeUnretainedValue()))
   let page=IOHIDElementGetUsagePage(element),usage=IOHIDElementGetUsage(element)
   let low=Double(IOHIDElementGetLogicalMin(element)),high=Double(IOHIDElementGetLogicalMax(element))
   if page==9 && usage>=1 && usage<=16 { buttons[Int(usage)-1]=value>0 ? 1:0 }
   if page==1 && usage==57 {
    let hat=Int(value-low)
    if value>=low && value<=high && hat<8 {
     let direction=[(0.0,-1.0),(1,-1),(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1)][hat]
     axes[0]=direction.0;axes[1]=direction.1
     buttons[12]=direction.1<0 ? 1:0;buttons[13]=direction.1>0 ? 1:0
     buttons[14]=direction.0<0 ? 1:0;buttons[15]=direction.0>0 ? 1:0
    }
   }
   // M30 reports unused analog axes at zero; its D-pad is the hat switch.
   if page==1 && !name.lowercased().contains("m30") && [48,49,51,52].contains(usage) && high>low {
    let axis=[48,49,51,52].firstIndex(of:usage)!
    let normalized=max(-1,min(1,(value-low)/(high-low)*2-1))
    axes[axis]=abs(normalized)<0.15 ? 0:normalized
   }
  }
  controllers.append(["index":index,"id":name,"axes":axes,"buttons":buttons])
 }
 guard let data=try? JSONSerialization.data(withJSONObject:controllers,options:[.sortedKeys]) else{return}
 if data != lastData || Date().timeIntervalSince(lastSent)>1 {
  FileHandle.standardOutput.write(data);FileHandle.standardOutput.write(Data([10]));lastData=data;lastSent=Date()
 }
}
let timer=Timer.scheduledTimer(withTimeInterval:1.0/60,repeats:true){_ in tick()}
RunLoop.main.run()
