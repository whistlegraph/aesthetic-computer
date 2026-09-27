# Xbox input send queue

`NetSendQueue` coalesces only adjacent, same-session input packets waiting for
`DataWriter::StoreAsync`. The in-flight write is unchanged. Simulation and the
configured input frequency are unchanged; the paired build remains at 30 Hz
until a separate device trial establishes a benefit at 60 Hz.

Every frame from both packets survives in the merged history. Overlapping
masks must agree, ranges cannot move backwards or leave a gap, and the union
cannot exceed the receiver's existing 20-frame limit (`NET_REDUNDANCY * 2`).
Newest acknowledgement, simulation-frame, lead, timestamp and echo fields
remain verbatim. Hashes, controls, unknown extensions, different session
origins and malformed/unrecognized packets remain FIFO barriers. Queue
capacity stays 32; a full queue may accept an input only by safely merging its
tail. This does not change existing socket-error recovery or retry semantics.

`xbox/runtime/tests/net_send_queue_contract.cpp` checks parsing boundaries,
both JSON field orders, conflicting overlaps, gaps, session changes, barriers,
maximum packet size, queue capacity, and complete frame preservation through a
100-packet burst. It passes with C++17, AddressSanitizer and UndefinedBehaviorSanitizer.
The Windows native preflight also compiles and runs it.

A deterministic 30-second simulation with 60 Hz input and a 25 ms serialized
writer produced these results:

| Queue | Maximum queued packets | Rejected packets | Mean newest-input age at completion |
| --- | ---: | ---: | ---: |
| Previous FIFO | 32 | 568 | 789.4 ms |
| Complete-history merge | 1 | 0 | 37.2 ms |

These are synthetic results. They do not establish Xbox RTT, game FPS, or
improved performance on the current LAN. Native package **1.0.0.44** passed
Windows preflight, GDK and UWP builds (AppVeyor 1.0.462) and was installed with
shared game v169, photographic assets and the LAN relay verified. A matched
device profile is still required before raising the configured 30 Hz send
frequency.
