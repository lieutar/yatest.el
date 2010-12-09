;;; yatest.el --- simple test framework
;; Copyright (C) 2008  Free Software Foundation, Inc.

;; Author:  <lieutar at 1dk.jp>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:


;;; Code:

(defconst yatest-util::popup::popup-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "q"
      (lambda ()
        (interactive)
	(kill-buffer (current-buffer))
	(top-level)))
    m))

(defun yatest-util::popup::popup-mode ()
  (use-local-map yatest-util::popup::popup-mode-map))

(defmacro yatest-util::popup (name initialize &rest body)
  ""
  (let ((initialize (or initialize '(yatest-util::popup::popup-mode))))
  `(save-window-excursion
     (save-excursion
       (let ((*buffer* (get-buffer-create ,name)))
         (set-buffer    *buffer*)
         ,initialize
         (toggle-read-only -1)
         (delete-region (point-min) (point-max))
         ,@body
         (toggle-read-only 1)
         (goto-char (point-min))
         (pop-to-buffer *buffer* t t)
         (recursive-edit))))))

(put 'yatest-util::popup 'lisp-indent-function 'defun)

(defun yatest-util::a (&rest args)
  "Prints args to minibuffer."
  (read-char (format "%S" args))
  nil)

(defsubst yatest-util::plist-to-alist (plist)
  "Makes alist from PLIST."
  (let ((retval ()))
    (while plist
      (setq retval (cons (cons (car  plist)
			       (cadr plist))
			 retval))
      (setq plist (cddr plist)))
    (reverse retval)))

(defsubst yatest-util::text-with-properties (str &rest props)
  ""
  (set-text-properties 0 (length str) props str)
  str)


(defvar yatest-failed nil)
(defvar yatest-report nil)

(defconst yatest::tests (make-hash-table :test 'eq) "yatest test db.")

(defface  yatest::ok-face
  '((((class color) (background light))
     (:foreground "black" :background "green"))
    (((class color) (background dark))
     (:foreground "black" :background "green"))
    (t ()))
  ""
  :group 'yatest)

(defface  yatest::faild-face 
  '((((class color) (background light))
     (:foreground "white" :background "red"))
    (((class color) (background dark))
     (:foreground "white" :background "red"))
    (t ()))
  ""
  :group 'yatest)


(defface  yatest::print-face 
  '((((class color) (background light))
     (:foreground "white" :background "blue"))
    (((class color) (background dark))
     (:foreground "white" :background "blue"))
    (t ()))
  ""
  :group 'yatest)

(defface yatest::trace-odd-face
  '((((class color) (background light))
     (:foreground "blue" ))
    (((class color) (background dark))
     (:foreground "cyan" ))
    (t ()))
  ""
  :group 'yatest)

(defface yatest::trace-even-face
  '((((class color) (background light))
     (:foreground "red" ))
    (((class color) (background dark))
     (:foreground "yellow" ))
    (t ()))
  ""
  :group 'yatest)

(defmacro yatest::p (name &rest x)
  ""
  `(let ((*ret* (progn ,@x)))
     (setq yatest-report (cons (cons :print (cons ,name *ret*)) yatest-report))
     *ret*))

(defun yatest::build-traced-line (trace)
  ""
  (format "%S" (cdr trace)))

(defun yatest::backtrace1 (cont debugger-args skip)
  (format "%S\nbacktrace:%s"
          debugger-args
          (let ((drop  t)
                (depth 0)
                (trace t)
                (skip (or skip 0))
                (all  ""))
            (while trace
              (unless (eq trace t)
                (if drop
                    (if (eq (cadr trace) 'yatest::backtrace)
                        (setq drop nil))
                  (if (> 1 skip)
                      (setq all
                            (concat
                             all
                             "\n"
			     (propertize
                              (yatest::build-traced-line  trace)
			      'face (if (= 0 (mod depth 2))
                                        'yatest::trace-even-face
                                      'yatest::trace-odd-face))))
                    (setq skip (1- skip)))))
              (setq trace (backtrace-frame depth))
              (setq depth (1+ depth))
              (when (and (eq    (cadr  trace) 'catch)
                         (equal (caddr trace) (list 'quote cont)))
                (setq trace nil)))
            all)))

(defun yatest::backtrace (cont debugger-args &optional skip)
  ""
  (let* ((old-local-map (current-local-map))
	 (tmp-map (make-keymap))
	 (len     (length (cadr tmp-map)))
	 (tbl     (make-vector len (lambda ()
				     (interactive)
				     (message "Continue ...")
				     (run-with-timer 0.125 nil
                                                     'exit-recursive-edit))))
	 (pos     1))
    (aset tbl 0 t)
    (setcar (cdr tmp-map) tbl)
    (use-local-map tmp-map)
    (message "\"yatest\" caught an error. Push any key...")
    (recursive-edit)
    (use-local-map old-local-map)
    (throw cont (yatest::backtrace1 cont debugger-args skip))))


(defsubst yatest::assert1 (name x)
  ""
  (let ((result (let ((debugger (lambda (&rest args)
				  (yatest::backtrace 'yatest->eval
							args
							1))))
		  (catch 'yatest->eval (eval (cons 'progn x))))))
    (setq yatest-report
	  (cons (if (eq result t)
                    `(:ok ,name)
		  (progn (setq yatest-failed (1+ yatest-failed))
			 `(:failed ,name ,(or result "yatest-failed"))))
		yatest-report))))

(defmacro yatest (name &rest x)
  ""
  `(yatest::assert1 ',name ',x))

(defun yatest::run-test::debugger (&rest debugger-args)
  (setq yatest-failed (1+ yatest-failed))
  (yatest::backtrace 'yatest::run-test->eval
                     debugger-args
                     1))

(defconst yatest::-old-debugger nil)
(defconst yatest::-report-ready nil)
(defconst yatest::-waiting-continuation
(defmacro yatest::-waiting (cont)
  `(let ((yatest::-waiting-continuation
          (lambda ()
            (if yatest::-report-ready
                (progn
                  ,@cont)
              (run-at-time "0.5 sec" nil yatest::-waiting-continuation)
              )))
         (funcall yatest::-waiting-continuation))))
(defun yatest::done ()
  (setq yatest::-report-ready t))

"
TODO 非同期テストの実装
"
(defun yatest::run-test (reporter project name all)
  "Runs provided test."
  (let*
      (
       (yatest-report  ())
       (yatest-failed  0)
       (errors         ())
       (debugger       'yatest::run-test::debugger)
       (err            nil)
       )
    (setq yatest::-report-ready nil)
    (setq err
          (catch 'yatest::run-test->eval
              (eval (append '(progn)
                            all
                            '(nil)))))

    (if err (setq yatest-report
		  (cons (list :failed "*** FATAL ERROR ***" err)
                        yatest-report)))

    (yatest::-waiting
     (apply reporter (list project
                           name
                           yatest-failed
                           (reverse yatest-report))))))



(defconst yatest::report-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "q" 'top-level)
    m))

(defun yatest::report-mode ()
  (use-local-map yatest::report-mode-map))


(defmacro yatest::async (&rest body)
  ""
  )

(defmacro yatest::define-async-test (project name &rest body)
  ""
  `(puthash ',name
            ,@body
            ,(or (gethash ',project yatest::tests)
                 (let ((hash (make-hash-table :test 'eq)))
                   (puthash ',project hash yatest::tests)
                   hash))))

(defmacro yatest::define-test (project name &rest body)
  "Defines new test as belongs the PROJECT."
  `(yatest::define-async-test project name (progn ,@body (yatest::done))))

(put 'yatest::define-test       'lisp-indent-function 'defun)
(put 'yatest::define-async-test 'lisp-indent-function 'defun)
(put 'yatest::async             'lisp-indent-function 'defun)

(defmacro yatest::define-async-test (project name &rest body)
  "Defines new asynchronous test."
  ;; TODO 非同期テストの定義を書く
  )

(defun yatest::report-single (project name failed report)
  "Reports result of a test."
  (yatest-util::popup  "*yatest*" (yatest::report-mode)
    (insert
     (mapconcat
      (lambda (r)
	(case (car r)
	  ((:ok)
	   (propertize
            (format "%s ... ok!" (cadr r)) 'face 'yatest::ok-face ))

	  ((:print)
           (concat 
            (propertize
             (format "%s :" (cadr r)) 'face 'yatest::print-face)
	    (format "%S" (cddr r))))

	  ((:failed)
	   (concat
	    (propertize
             (format "%s :" (cadr r)) 'face   'yatest::faild-face)
	    (format " %s" (caddr r)))))
	)
      report
      "\n"))
    (if (= failed 0)
	(message "all tests successful.")
      (message (format "The test has %d or more errors." failed)))))

(defun yatest::project-alist ()
  "Returns all project names as alist."
  (let((ret ()))
    (maphash
     (lambda (key val)
       (setq ret (cons (list key val) ret)))
     yatest::tests)
    ret))

(defun yatest::test-alist (project)
  "Returns alist of test that bound PROJECT."
  (let ((ret ()))
    (maphash
     (lambda (key val)
       (setq ret (cons (cons key val) ret)))
     (or (gethash project yatest::tests)
         (make-hash-table)))
    ret))

(defun yatest::run (project &optional name reporter)
  "Runs all tests what belongs the PROJECT."

  (interactive
   (let* ((prj (intern (completing-read "project: "
				       (yatest::project-alist) nil t)))
	  (cands (mapcar (lambda (x) (list (symbol-name (car x))))
			 (yatest::test-alist prj)))
	  (name (completing-read
		 "test: "  cands nil t)))
     (list prj (if (equal name "") nil (intern name)))))

  (let ((alist (yatest::test-alist project)))
    (if name
	(progn
	  (yatest::run-test (or reporter
                                (function yatest::report-single))
                            project
                            name
                            (cdr (assoc name alist))))
      (let ((result ())
	    (yatest-failed 0))

	(mapcar (lambda (x)
		  (yatest::run-test
		   (lambda (project name fail report)
		     (setq result
			   (cons
                            (list
                             (if (> fail 0) 
                                 (progn (setq yatest-failed (1+ yatest-failed))
                                        :failed)
                               :ok) name) result)))
		   project
		   (car x)
		   (cdr x)))
		alist)

	(when (interactive-p)
	    (yatest::report-single project '*ALL* yatest-failed result))
	  (cons yatest-failed result)))))

(provide 'yatest)
;;; yatest.el ends here.
;;; Local Variables:
;;; coding: utf-8
;;; End:
