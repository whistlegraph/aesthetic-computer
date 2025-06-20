; Simple paste test
(wipe "black")

; Draw a bright red square
(ink "red")
(box 10 10 10 10)

; Copy it
(copy 10 10 10 10)

; Check if copy worked
(log "Copy worked:" (copy?))

; Paste it at 50, 50
(paste 50 50)

; Draw a white pixel at the paste location as a marker
(ink "white")
(plot 50 50)
