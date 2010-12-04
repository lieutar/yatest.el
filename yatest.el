;;; yatest.el --- simple test framework
;; -*- coding: utf-8 -*-
;; Copyright (C) 2008  Free Software Foundation, Inc.

;; Author:  <lieutar@1dk.jp>
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

;; TODO symbol-plist を使ってハッシュテーブルがわりにしているものをハッシュテーブルにする

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
         (recursive-edit)))
     )))
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

(defun yatest-util::stext (&rest text-spec)
  "Builds structed text that specified by TEXT-SPEC."
  (let* ((stack          (list text-spec))
         (retval         "")
         (context        (make-symbol "*context*"))
         (context-stack  (list context)))

    (while stack
      (let ((src (car stack)))
        (setq stack (cdr stack))
        (while src
          (let ((node (car src)))
            (setq src (cdr src))

            (cond ((stringp node)
                   (let ((plist (symbol-plist context)))
                     (setq retval
                           (concat retval
                                   (if plist
                                       (apply
                                        'yatest-util::text-with-properties
                                              (cons node plist))
                                     node)))))

                  ((symbolp node)
                   (let ((val (car src)))
                     (setq src (cdr src))
                     (when (eq val '<<)
                       (setq val (get (car context-stack) node)))
                     (put context node val)))

                  ((listp   node)
                   (setq stack         (cons src    stack))
                   (setq context-stack (cons context context-stack))
                   (setq context       (make-symbol "*context*"))
                   (setq src           node)))))

        (setq context       (car context-stack))
        (setq context-stack (cdr context-stack))))
    retval))

(defvar failed nil)
(defvar report nil)

(defconst yatest::tests () "symbol for plist of tests.")

(defface  yatest::ok-face
  '((((class color) (background light))
     (:foreground "black" :background "green"))
    (((class color) (background dark))
     (:foreground "black" :background "green"))
    (t ()))
  "")

(defface  yatest::faild-face 
  '((((class color) (background light))
     (:foreground "white" :background "red"))
    (((class color) (background dark))
     (:foreground "white" :background "red"))
    (t ()))
  "")


(defface  yatest::print-face 
  '((((class color) (background light))
     (:foreground "white" :background "blue"))
    (((class color) (background dark))
     (:foreground "white" :background "blue"))
    (t ()))
  "")

(defface yatest::trace-odd-face
  '((((class color) (background light))
     (:foreground "blue" ))
    (((class color) (background dark))
     (:foreground "cyan" ))
    (t ()))
  "")

(defface yatest::trace-even-face
  '((((class color) (background light))
     (:foreground "red" ))
    (((class color) (background dark))
     (:foreground "yellow" ))
    (t ()))
  "")



(defmacro yatest::p (name &rest x)
  ""
  `(let ((*ret* (progn ,@x)))
     (setq report (cons (cons :print (cons ,name *ret*)) report))
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
                            (yatest-util::stext
                             all
                             "\n"
			     (list
			      'face
			      (if (= 0 (mod depth 2))
				  'yatest::trace-even-face
				 'yatest::trace-odd-face)
			      (yatest::build-traced-line
			       trace))))
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
    (setq report
	  (cons (if (eq result t)
                    `(:ok ,name)
		  (progn (setq failed (1+ failed))
			 `(:failed ,name ,(or result "failed"))))
		report))))

(defmacro yatest (name &rest x)
  ""
  `(yatest::assert1 ',name ',x))

(defun yatest::run-test::debugger (&rest debugger-args)
  (setq failed (1+ failed))
  (yatest::backtrace 'yatest::run-test->eval
                            debugger-args
			    1))

(defun yatest::run-test (reporter project name all)
  "Runs provided test."
  (let* ((report  ())
	 (errors  ())
	 (failed  0)
         (debugger 'yatest::run-test::debugger)
         (err (catch 'yatest::run-test->eval
                (eval (append '(progn)
                              all
                              '(nil))))))
    (if err (setq report
		  (cons (list :failed "*** FATAL ERROR ***" err) report)))
    (apply reporter (list project name failed (reverse report)))))



(defconst yatest::report-mode-map
  (let ((m (make-keymap)))
    (define-key m "q" 'top-level)
    m))


(defun yatest::report-mode ()
  (use-local-map yatest::report-mode-map))

(defmacro yatest::define-test (project name &rest body)
  "Defines new test as belongs the PROJECT."
  `(progn
     (let ((dic (or (get 'yatest::tests ',project)
		      (let ((sym (make-symbol (symbol-name ',project))))
			(put 'yatest::tests ',project sym)
			sym))))
       (put dic ',name ',body))))
(put 'yatest::define-test 'lisp-indent-function 'defun)

(defun yatest::report-single (project name failed report)
  "Reports result of a test."
  (yatest-util::popup  "*yatest*" (yatest::report-mode)
    (insert
     (mapconcat
      (lambda (r)
	(case (car r)
	  ((:ok)
	   (yatest-util::stext
	    `(face yatest::ok-face ,(format "%s ... ok!" (cadr r)))))

	  ((:print)
	   (yatest-util::stext
	    `(face yatest::print-face ,(format "%s :" (cadr r)))
	    (format "%S" (cddr r))))

	  ((:failed)
	   (yatest-util::stext
	    `(face   yatest::faild-face
	     ,(format "%s :" (cadr r)))
	    (format " %s" (caddr r)))))
	)
      report
      "\n"))
    (if (= failed 0)
	(message "all tests successful.")
      (message (format "The test has %d or more errors." failed)))))

(defun yatest::project-alist ()
  "Returns all project names as alist."
  (let((src (symbol-plist 'yatest::tests))
       (ret ()))
    (while src
      (setq ret (cons  (list (symbol-name (car src))) ret))
      (setq src (cdr (cdr src))))
    ret))

(defun yatest::test-alist (project)
  "Returns alist of test that bound PROJECT."
  (yatest-util::plist-to-alist
   (symbol-plist (get 'yatest::tests project))))

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
	    (failed 0))

	(mapcar (lambda (x)
		  (yatest::run-test
		   (lambda (project name fail report)
		     (setq result
			   (cons (list (if (> fail 0) 
					 (progn (setq failed (1+ failed))
                                                :failed)
					 :ok) name) result)))
		   project
		   (car x)
		   (cdr x)))
		alist)

	(when (interactive-p)
	    (yatest::report-single project '*ALL* failed result))
	  (cons failed result)))))

(provide 'yatest)
;;; yatest.el ends here.
