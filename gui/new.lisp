(:DEBUG #.(progn
            (defun furcadia-launcher::save-config-file (&rest x)
              (declare (ignore x)))
            (setf drakma:*drakma-default-external-format* :utf-8)
            (setf *debugger-hook*
                  (lambda (c x)
                    (declare (ignore c x))
                    (sb-debug:print-backtrace :count most-positive-fixnum)))
            t)
        :ACCOUNTS (("gemeinhardt2000@yahoo.com" "sdfdfs"))
        :FURCADIA-PATH "C:\\Program Files (x86)\\Furcadia\\")
