;;; browse-pull-request-test.el --- Tests for my/browse-pull-request -*- lexical-binding: t; -*-

;; `my/browse-pull-request' (`<leader> g p') takes the current branch straight
;; to GitHub's "compare" view for a new pull request, reading the `origin'
;; remote and branch via Magit's own (autoloaded) `magit-git-string' /
;; `magit-get-current-branch' rather than adding a `forge'/`gh' dependency.
;;
;; The `origin' URL is accepted in both the SSH and HTTPS forms; anything that
;; is not a GitHub remote, or not on a branch at all, is a `user-error' rather
;; than a wrong URL silently opened.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'config-test-helper (expand-file-name "config-test-helper.el"
                                               (file-name-directory
                                                (or load-file-name buffer-file-name))))

(defmacro bpr-test--with-stubs (remote branch &rest body)
  "Run BODY with `origin' resolving to REMOTE and the current branch to BRANCH.
`browse-url' is captured rather than actually invoked; callers read it back
via the dynamically bound `bpr-test--browsed' variable."
  (declare (indent 2))
  `(let (bpr-test--browsed)
     (cl-letf (((symbol-function 'magit-git-string)
                (lambda (&rest _) ,remote))
               ((symbol-function 'magit-get-current-branch)
                (lambda () ,branch))
               ((symbol-function 'browse-url)
                (lambda (url) (setq bpr-test--browsed url))))
       ,@body)))

(ert-deftest browse-pull-request/ssh-remote-opens-compare-view ()
  "An SSH `origin' (`git@github.com:owner/repo.git') resolves to owner/repo."
  ;; Arrange
  (cfg-test-load-defun 'my/browse-pull-request)
  (bpr-test--with-stubs "git@github.com:owner/repo.git" "feature-branch"
    ;; Act
    (my/browse-pull-request)
    ;; Assert
    (should (equal bpr-test--browsed
                   "https://github.com/owner/repo/pull/new/feature-branch"))))

(ert-deftest browse-pull-request/https-remote-opens-compare-view ()
  "An HTTPS `origin' (`https://github.com/owner/repo.git') also resolves."
  ;; Arrange
  (cfg-test-load-defun 'my/browse-pull-request)
  (bpr-test--with-stubs "https://github.com/owner/repo.git" "feature-branch"
    ;; Act
    (my/browse-pull-request)
    ;; Assert
    (should (equal bpr-test--browsed
                   "https://github.com/owner/repo/pull/new/feature-branch"))))

(ert-deftest browse-pull-request/https-remote-without-dot-git-suffix ()
  "The `.git' suffix on the remote URL is optional."
  ;; Arrange
  (cfg-test-load-defun 'my/browse-pull-request)
  (bpr-test--with-stubs "https://github.com/owner/repo" "main"
    ;; Act
    (my/browse-pull-request)
    ;; Assert
    (should (equal bpr-test--browsed
                   "https://github.com/owner/repo/pull/new/main"))))

(ert-deftest browse-pull-request/non-github-remote-errors ()
  "A non-GitHub `origin' is reported, not silently opened."
  ;; Arrange
  (cfg-test-load-defun 'my/browse-pull-request)
  (bpr-test--with-stubs "git@gitlab.com:owner/repo.git" "feature-branch"
    ;; Act / Assert
    (should-error (my/browse-pull-request) :type 'user-error)))

(ert-deftest browse-pull-request/detached-head-errors ()
  "No current branch (detached HEAD) is reported, not silently opened."
  ;; Arrange
  (cfg-test-load-defun 'my/browse-pull-request)
  (bpr-test--with-stubs "git@github.com:owner/repo.git" nil
    ;; Act / Assert
    (should-error (my/browse-pull-request) :type 'user-error)))

(provide 'browse-pull-request-test)
;;; browse-pull-request-test.el ends here
