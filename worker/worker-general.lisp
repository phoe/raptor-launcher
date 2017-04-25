;;;; worker-general.lisp

(in-package :furcadia-launcher)

(defun finalize-thread-alist (alist)
  "Provided an alist, in which CDRs are threads, blocks and waits for each
thread to finish before destructively replacing each CDR with the resulting
value of the thread's function."
  ;; We assume that JOIN-THREAD returns the value of the function the thread
  ;; was called with. In the event of BORDEAUX-THREADS being fixed in order
  ;; to provide a documented function that returns the return value of the
  ;; thread by *definition*, not by *accident*, please fix this code to use
  ;; the new function. In the event an implementation does not conform to
  ;; this behaviour, please fix your impleme^W^W^Wcreate a bug on this
  ;; project.
  (flet ((finalize-cdr (cons)
           (setf (cdr cons) (join-thread (cdr cons)))))
    (mapc #'finalize-cdr alist)))
