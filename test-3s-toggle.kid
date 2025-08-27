// Test case for 3s toggle bug fix
// This should alternate between $neio and $pif every 3 seconds

(3s... ($neio 0 0 w h/2)
       ($pif 0 h/2 w h/2))
(blur 4)
;(1s (zoom 0.5))
;(scroll 0 -frame)
