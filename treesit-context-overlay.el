;;; treesit-context-overlay.el --- Show overlays at ending of containing scopes using Tree-sitter -*- lexical-binding: t; -*-

;; Author: samwdp
;; Maintainer: samwdp
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/samwdp/treesit-context-overlay
;; Keywords: Package Emacs

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(require 'treesit)

(defgroup treesit-context-overlay nil
  "Show overlays at ending of containing scopes using Tree-sitter."
  :group 'convenience
  :prefix "treesit-context-overlay-")

(defcustom treesit-context-overlay-delimiter "==>"
  "String prepended to the overlay display text, before the context label."
  :type 'string
  :group 'treesit-context-overlay)

(defvar-local treesit-context-overlay--overlays nil
  "List of overlays displaying context at scope endings.")

(defconst treesit-context-overlay--scope-node-types
  '("function_declaration" "function" "method_definition" "arrow_function"
    "generator_function" "function_expression"
    "class_declaration" "class"
    "object" "object_literal" "object_pattern"
    "if_statement" "else_clause"
    "for_statement" "for_in_statement" "for_of_statement"
    "while_statement" "do_statement"
    "try_statement" "catch_clause" "finally_clause"
    "variable_declaration" "lexical_declaration" "variable_statement"
    "statement_block" "block"
    "namespace_declaration")
  "Tree-sitter node types considered as code scopes (including loops, try/catch, blocks, and namespace declarations).")

(defun treesit-context-overlay--clear-overlays ()
  "Remove all treesit-context-overlay overlays."
  (when treesit-context-overlay--overlays
    (mapc #'delete-overlay treesit-context-overlay--overlays)
    (setq treesit-context-overlay--overlays nil)))

(defun treesit-context-overlay--get-scope-name (node)
  "Get a context name for NODE. Does not include delimiter."
  (when node
    (let ((type (treesit-node-type node)))
      (cond
       ;; Statements with condition/parameters
       ((member type '("if_statement" "while_statement" "for_statement" "for_in_statement" "for_of_statement" "do_statement"))
        (let* ((kw (cond
                    ((string= type "if_statement")
                     (if (treesit-node-child-by-field-name node "alternative")
                         "else if"
                       "if"))
                    ((string= type "while_statement") "while")
                    ((string= type "for_statement") "for")
                    ((string= type "for_in_statement") "for")
                    ((string= type "for_of_statement") "for")
                    ((string= type "do_statement") "do while")
                    (t type)))
               (header
                (cond
                 ;; for loops: reconstruct full header
                 ((string= type "for_statement")
                  (let ((init (treesit-node-child-by-field-name node "initializer"))
                        (condn (treesit-node-child-by-field-name node "condition"))
                        (upd (treesit-node-child-by-field-name node "update")))
                    (concat
                     "for ("
                     (when init (string-trim (treesit-node-text init)))
                     (when condn (concat (if init "; " "") (string-trim (treesit-node-text condn))))
                     (when upd (concat "; " (string-trim (treesit-node-text upd))))
                     ")")))
                 ;; for-in/of: left and right
                 ((or (string= type "for_in_statement") (string= type "for_of_statement"))
                  (let ((left (treesit-node-child-by-field-name node "left"))
                        (right (treesit-node-child-by-field-name node "right")))
                    (concat "for ("
                            (when left (string-trim (treesit-node-text left)))
                            (if right (concat " in " (string-trim (treesit-node-text right))) "")
                            ")")))
                 ;; if, while, do while: use condition
                 (t
                  (let ((cond-node (treesit-node-child-by-field-name node "condition")))
                    (if cond-node
                        (concat kw " (" (string-trim (treesit-node-text cond-node)) ")")
                      kw))))))
          header))
       ;; else: handled as alternative field of if_statement, or as else_clause
       ((string= type "else_clause")
        "else")
       ;; function/class/object/namespace: use name or key if available
       ((member type '("function_declaration" "function" "method_definition"
                       "arrow_function" "generator_function" "function_expression"
                       "class_declaration" "class"
                       "object" "object_literal" "object_pattern"
                       "namespace_declaration"))
        (let ((name-node (or (treesit-node-child-by-field-name node "name")
                             (treesit-node-child-by-field-name node "key"))))
          (when name-node (string-trim (treesit-node-text name-node)))))
       ;; try/catch/finally: show keyword
       ((member type '("try_statement" "catch_clause" "finally_clause"))
        (replace-regexp-in-string "_" " " (replace-regexp-in-string "_statement\\|_clause$" "" type)))
       ;; variable/lexical declarations (let/const/var)
       ((member type '("variable_declaration" "lexical_declaration" "variable_statement"))
        (let ((name-node (treesit-node-child-by-field-name node "name")))
          (if name-node
              (string-trim (treesit-node-text name-node))
            (pcase type
              ("variable_declaration" "var")
              ("lexical_declaration" "let/const")
              (_ "var/let/const")))))
       ;; statement_block or block: use parent node's keyword if available
       ((member type '("statement_block" "block"))
        (let* ((parent (treesit-node-parent node))
               (parent-type (when parent (treesit-node-type parent))))
          (cond
           ((member parent-type '("if_statement" "while_statement" "for_statement"
                                  "for_in_statement" "for_of_statement" "do_statement"))
            (treesit-context-overlay--get-scope-name parent))
           ((member parent-type '("try_statement" "catch_clause" "finally_clause"))
            (replace-regexp-in-string "_" " " (replace-regexp-in-string "_statement\\|_clause$" "" parent-type)))
           ((member parent-type '("function_declaration" "function" "method_definition"
                                  "arrow_function" "generator_function" "function_expression"))
            (let ((name-node (or (treesit-node-child-by-field-name parent "name")
                                 (treesit-node-child-by-field-name parent "key"))))
              (when name-node (string-trim (treesit-node-text name-node)))))
           ((member parent-type '("class_declaration" "class"))
            (let ((name-node (treesit-node-child-by-field-name parent "name")))
              (when name-node (string-trim (treesit-node-text name-node)))))
           ((member parent-type '("namespace_declaration"))
            (let ((name-node (treesit-node-child-by-field-name parent "name")))
              (when name-node (string-trim (treesit-node-text name-node)))))
           (t "{}"))))
       (t
        (treesit-node-type node))))))

(defun treesit-context-overlay--enclosing-scope-nodes (pt)
  "Return all enclosing scope nodes for point PT, innermost first."
  (let ((nodes '())
        (node (when (fboundp 'treesit-node-at) (treesit-node-at pt))))
    (while node
      (when (member (treesit-node-type node) treesit-context-overlay--scope-node-types)
        (push node nodes))
      (setq node (treesit-node-parent node)))
    (nreverse nodes)))

(defun treesit-context-overlay--format-display (name)
  "Format overlay display string with delimiter prepended to NAME."
  (when (and name (not (string-empty-p name)))
    (format " %s %s" treesit-context-overlay-delimiter name)))

(defun treesit-context-overlay--add-overlays ()
  "Add overlays at the end of all containing scopes at point."
  (treesit-context-overlay--clear-overlays)
  (when (treesit-parser-list)
    (let* ((pts (if (and (> (point) (point-min))
                         (let* ((char-now (char-after (point)))
                                (char-prev (char-before (point))))
                           (or (eq char-now ?}) (eq char-prev ?}))))
                    (list (point) (1- (point)))
                  (list (point))))
           (ends-seen '()))
      (dolist (pt pts)
        (dolist (scope (treesit-context-overlay--enclosing-scope-nodes pt))
          (let* ((end (treesit-node-end scope))
                 (name (treesit-context-overlay--get-scope-name scope))
                 (display (treesit-context-overlay--format-display name)))
            ;; Only show one overlay per scope end, and only if point is within the scope.
            (when (and (not (memq end ends-seen))
                       (<= (treesit-node-start scope) pt)
                       (< pt (treesit-node-end scope)))
              (when display
                (let ((ov (make-overlay end end)))
                  (overlay-put ov 'after-string
                               (propertize display 'face 'font-lock-comment-face))
                  (push ov treesit-context-overlay--overlays)
                  (push end ends-seen))))))))))

(defun treesit-context-overlay--post-command-hook ()
  (condition-case err
      (treesit-context-overlay--add-overlays)
    (error (message "[treesit-context-overlay] Error: %S" err))))

;;;###autoload
(define-minor-mode treesit-context-overlay-mode
  "Display overlays at endings of all containing scopes using Tree-sitter.
Overlays show at the } (or end) of any scope (function, class, object, if, loop, try/catch, block, namespace) containing point.
No overlays at property ends or for scopes not containing point."
  :lighter " CtxVT"
  (if treesit-context-overlay-mode
      (progn
        (add-hook 'post-command-hook #'treesit-context-overlay--post-command-hook nil t)
        (treesit-context-overlay--add-overlays))
    (remove-hook 'post-command-hook #'treesit-context-overlay--post-command-hook t)
    (treesit-context-overlay--clear-overlays)))

(provide 'treesit-context-overlay)
;;; treesit-context-overlay.el ends here
