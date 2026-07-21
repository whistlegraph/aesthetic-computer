# Xbox

Treat Xbox as an early hardware and publishing spike. Test the current
`xbox/dynamic-shell/` and `xbox/native-bios/` experiments for controller latency,
60 Hz pacing, audio, network availability, suspend/resume, memory pressure, and
the ID@Xbox distribution path.

Do not make the simulation depend on WebView2. If the approved host cannot run
the browser WebRTC path, provide a native unreliable-datagram transport behind
the same packet interface and keep the JS rollback/session semantics intact.
For crossplay with browser clients, either embed a compatible WebRTC data-channel
implementation or connect the native secure-datagram path to a low-latency edge
gateway that forwards the shared Menu Fighter packet protocol.

Relevant Microsoft starting points:

- [Xbox application architecture](https://learn.microsoft.com/en-us/windows/uwp/apps-for-xbox/application-architecture)
- [ID@Xbox](https://www.xbox.com/en-US/publish)
- [GDK networking APIs](https://learn.microsoft.com/en-us/xbox/gdk/docs/features/console/networking/introduction-networking)
