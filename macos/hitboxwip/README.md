# hitboxwip

A native visual input tester and jamboard for the 8BitDo Arcade Controller for
Xbox (`2dc8:202c`). `hitboxwip` is the working identifier for this experiment.
Its transparent overlay follows 8BitDo's official physical layout, highlights
raw Xbox GIP input, reports live event timing, and plays a dry percussion plus
pentatonic Whistle mapping.

```sh
cd macos/hitboxwip
swift run hitboxwip
```

Requires `libusb` (`brew install libusb`). The Xbox edition uses Xbox GIP over
a vendor-specific USB interface and is not currently returned by macOS's
`GameController` framework.

Layout references:

- https://www.8bitdo.com/arcade-controller-xbox/
- https://www.8bitdo.com/images/2025/arcade-controller-xbox/03-l.jpg
- https://download.8bitdo.com/Manual/Controller/Xbox/8BItDo-Arcade-Controller-for-Xbox-EN.pdf

The straight-on `03-l.jpg` product shot is bundled as a calibration resource
and supplies the normalized control centers, relative cap sizes, and
pixel-traced P1/P2 contours. The manual supplies the official control names.
