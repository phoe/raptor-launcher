;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; OUR METHOD
;;;;
;;;; 1. Login into Furcadia CMS.
;;;;   a. GET the login page.
;;;;   b. Extract the authentication secret from HTML.
;;;;   c. POST the login which includes the authentication secret.
;;;; 2. Get the furc:// login string.
;;;;   a. GET the FurEd page.
;;;;   b. Extract the character login secret from HTML.
;;;;   c. Fetch the character we want to login as.
;;;;   d. POST the character with the character login secret.
;;;;   e. Receive the login string from the HTTP response.
;;;; 3. Launch Furcadia.
;;;;   a. Launch the Furcadia client with the login string as cmdline argument.
;;;;
