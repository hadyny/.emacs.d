;;; org-timegrid-test.el --- Tests for the org-timegrid setup -*- lexical-binding: t; -*-

;; `org-timegrid' is not in nixpkgs, so flake.nix builds it from source with
;; `trivialBuild' (the `zk4e' pattern) rather than pulling it from the
;; `emacsPackages' set.  These tests are structural: they parse the tangled
;; config.el and flake.nix, so they run anywhere.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(defun ot-test--use-package-form (name)
  "Return the `use-package NAME' form from config.el, or nil."
  (catch 'found
    (dolist (form (cfg-test-read-forms))
      (dolist (up (cfg-test-find-all form 'use-package))
        (when (eq (nth 1 up) name)
          (throw 'found up))))
    nil))

(defun ot-test--init-value (form var)
  "Return the value the `:init' `setq' block of FORM gives VAR."
  (let* ((printed (prin1-to-string form))
         (regexp (concat "\\b" (regexp-quote (symbol-name var))
                          "\\b[[:space:]\n]*")))
    (with-temp-buffer
      (insert printed)
      (goto-char (point-min))
      (if (re-search-forward regexp nil t)
          (let ((value (read (current-buffer))))
            ;; `read' turns a printed "'agenda" into (quote agenda); unwrap it
            ;; so callers compare against the plain symbol/string/number.
            (if (and (consp value) (eq (car value) 'quote))
                (cadr value)
              value))
        'unset))))

(ert-deftest org-timegrid/package-is-in-the-closure ()
  "flake.nix builds org-timegrid from source, like zk4e."
  ;; Arrange / Act
  (let ((packages (cfg-test-nix-list "dotemacsPackageList")))
    ;; Assert
    (should (member "org-timegrid" packages))))

(ert-deftest org-timegrid/week-view-is-configured ()
  "org-timegrid reads org-agenda-files and captures into calendar.org."
  ;; Arrange / Act
  (let ((form (ot-test--use-package-form 'org-timegrid)))
    ;; Assert
    (should form)
    (should (eq (ot-test--init-value form 'org-timegrid-org-files) 'agenda))
    (should (equal (ot-test--init-value form 'org-timegrid-org-capture-file)
                    "~/notes/org/calendar.org"))
    (should (string-match-p "org-timegrid-week"
                            (prin1-to-string form)))))

(ert-deftest org-timegrid/agenda-strip-is-enabled-after-org-agenda ()
  "The optional Agenda day-strip loads after org-agenda and is turned on."
  ;; Arrange / Act
  (let* ((form (ot-test--use-package-form 'org-timegrid-agenda))
         (printed (prin1-to-string form)))
    ;; Assert
    (should form)
    (should (eq (cadr (memq :after form)) 'org-agenda))
    (should (string-match-p "org-timegrid-agenda-mode 1" printed))))

(ert-deftest org-timegrid/week-command-is-bound ()
  "<leader> o w opens the week calendar."
  ;; Arrange / Act
  (let ((code (mapconcat #'identity
                          (mapcar #'prin1-to-string
                                  (cfg-test-find-all
                                   (cons 'progn (cfg-test-read-forms))
                                   'evil-define-key))
                          " ")))
    ;; Assert
    (should (string-match-p "<leader> o w" code))
    (should (string-match-p "org-timegrid-week" code))))

(provide 'org-timegrid-test)
;;; org-timegrid-test.el ends here
