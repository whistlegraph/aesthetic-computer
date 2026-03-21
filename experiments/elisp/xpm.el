                                                                                         
 ;; Define the XPM image as a string                                                     
 (setq my-xpm-image "\                                                                   
 /* XPM */                                                                               
 static char * example_xpm[] = {                                                         
 \"4 4 2 1\",                                                                            
 \"  c None\",                                                                           
 \"X c #000000\",                                                                        
 \"    \",                                                                               
 \" XX \",                                                                               
 \" XX \",                                                                               
 \"    \"};                                                                              
 ")                                                                                      
                                                                                         
 ;; Insert the XPM image string into the scratch buffer                                  
 (with-current-buffer "*scratch*"                                                        
   (insert my-xpm-image)                                                                 
   (image-mode))  ;; Switch to image mode to view the image                              
                                                                                         
 (defun update-xpm-image (x y color)                                                     
   "Update the XPM image at position X Y with COLOR."                                    
   (let ((image-lines (split-string my-xpm-image "\n")))                                 
     ;; Calculate the line number based on the XPM header size                           
     (let ((line-number (+ y 4)))  ;; Adjust 4 for the XPM header size                   
       ;; Replace the character at the specified position                                
       (let ((line (nth line-number image-lines)))                                       
         (when line                                                                      
           (let ((new-line (concat (substring line 0 x)                                  
                                   color                                                 
                                   (substring line (1+ x)))))                            
             (setf (nth line-number image-lines) new-line)                               
             ;; Update the image string                                                  
             (setq my-xpm-image (string-join image-lines "\n"))                          
             ;; Update the buffer content                                                
             (with-current-buffer "*scratch*"                                            
               (erase-buffer)                                                            
               (insert my-xpm-image)                                                     
               (image-mode)))))))                                                        
                                                                                         
 ;; Example usage: Set the pixel at (1, 1) to black                                      
 (update-xpm-image 1 1 "X") 
