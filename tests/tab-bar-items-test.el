;;; tab-bar-items-test.el --- Tests for my/tab-bar-items -*- lexical-binding: t; -*-

;; `my/tab-bar-items' is the `:content' function for the tab-bar `svg-line'
;; install (see the Mode-line section in config.org).  Its shape must match
;; what `svg-line-wrap-image' expects: a list of (LABEL . STATE), where STATE
;; is a plist carrying `:current' (bold + `:current-background') plus the
;; hover/click keys svg-line's tab-bar wiring dispatches on --
;; `:help'/`:action'/`:menu'.
;;
;; Two of those keys have a real behavioural contract, not just a shape:
;;
;;   * `:action' is run via `call-interactively' unconditionally by
;;     `svg-line--tab-bar-mouse-down-advice' -- it must be a genuine
;;     interactive command (a lambda with `(interactive)'), or the click
;;     handler errors instead of falling back to the default tab-bar binding.
;;   * `:menu' items are run through `commandp' then either
;;     `call-interactively' or `funcall' by `svg-line--popup-menu' -- a plain
;;     non-interactive closure is fine there.
;;
;; `tab-bar-tabs' is stubbed rather than exercised against a real frame's
;; tabs: its shape is `(tab (name . NAME) ...)' for an inactive tab and
;; `(current-tab (name . NAME) ...)' for the active one, and `alist-get' reads
;; `name' off of that -- a real multi-tab frame in batch Emacs would be more
;; setup than signal.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(defmacro tab-bar-items-test--with-stub-tabs (tabs &rest body)
  "Run BODY with `tab-bar-tabs' stubbed to return TABS."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'tab-bar-tabs) (lambda (&optional _frame) ,tabs)))
     ,@body))

(ert-deftest tab-bar-items/one-entry-per-tab-with-current-flagged ()
  "Returns one (LABEL . STATE) per tab, `:current' only on the active one."
  ;; Arrange
  (cfg-test-load-defun 'my/tab-bar-items)
  ;; Act
  (tab-bar-items-test--with-stub-tabs
      '((tab (name . "one"))
        (current-tab (name . "two")))
    (let ((items (my/tab-bar-items)))
      ;; Assert
      (should (= (length items) 2))
      (should (string-match-p "one" (car (nth 0 items))))
      (should (string-match-p "two" (car (nth 1 items))))
      (should-not (plist-get (cdr (nth 0 items)) :current))
      (should (plist-get (cdr (nth 1 items)) :current))
      (should (= (plist-get (cdr (nth 0 items)) :id) 1))
      (should (= (plist-get (cdr (nth 1 items)) :id) 2)))))

(ert-deftest tab-bar-items/action-is-an-interactive-select ()
  "`:action' is a genuine command (required by `call-interactively') that
selects the right tab by its 1-based position."
  ;; Arrange
  (cfg-test-load-defun 'my/tab-bar-items)
  (let (selected)
    (cl-letf (((symbol-function 'tab-bar-select-tab)
               (lambda (n) (setq selected n))))
      (tab-bar-items-test--with-stub-tabs
          '((tab (name . "one")) (tab (name . "two")))
        (let* ((items (my/tab-bar-items))
               (action (plist-get (cdr (nth 1 items)) :action)))
          ;; Assert -- a real interactive command, not a plain closure.
          (should (commandp action))
          ;; Act
          (call-interactively action)
          ;; Assert
          (should (= selected 2)))))))

(ert-deftest tab-bar-items/menu-renames-and-closes-by-position ()
  "`:menu' offers rename/close, funcalled against the right tab position."
  ;; Arrange
  (cfg-test-load-defun 'my/tab-bar-items)
  (let (renamed-to renamed-at closed-at)
    (cl-letf (((symbol-function 'tab-bar-rename-tab)
               (lambda (name n) (setq renamed-to name renamed-at n)))
              ((symbol-function 'tab-bar-close-tab)
               (lambda (n) (setq closed-at n)))
              ((symbol-function 'read-string)
               (lambda (_prompt &optional _default) "renamed")))
      (tab-bar-items-test--with-stub-tabs
          '((tab (name . "one")) (current-tab (name . "two")))
        (let* ((items (my/tab-bar-items))
               (menu (plist-get (cdr (nth 0 items)) :menu))
               (rename (cdr (assoc "Rename tab" menu)))
               (close (cdr (assoc "Close tab" menu))))
          ;; Assert -- plain closures, funcalled by `svg-line--popup-menu'.
          (should-not (commandp rename))
          (should-not (commandp close))
          ;; Act
          (funcall rename)
          (funcall close)
          ;; Assert
          (should (equal renamed-to "renamed"))
          (should (= renamed-at 1))
          (should (= closed-at 1)))))))

(provide 'tab-bar-items-test)
;;; tab-bar-items-test.el ends here
