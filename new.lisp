#|
16:18 < ogamita> phoe: you can use whatever logical pathname you like.
Then add a translation for it.
16:20 < ogamita> (setf (logical-pathname-translations "APP")
'(("FOO;" (merge-pathnames #+ccl #P".foo/" #-ccl (something-else)
(user-homedir-pathname)))))
16:21 < ogamita> Or more probably, you put your logical pathname translations
in an external configuration file, depending on the implementation and
use (load-logical-pathname-translations)
|#
