;;; kql-mode.el --- Major mode for highlighting KQL -*- lexical-binding: t -*-

;; Author: Aimé Bertrand
;; Version: 0.2
;; Package-Requires: ((emacs "26.1"))
;; Created: 2023-10-13
;; Keywords: files languages azure entra kql faces syntax major-mode
;; Homepage: https://gitlab.com/aimebertrand/kql-mode
;; This file is NOT part of GNU Emacs.

;; The MIT License (MIT)
;;
;; Copyright (c) 2023 Aimé Bertrand
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:
;; kql-mode provides syntax highlighting and basic indentation for Kusto Query Language (KQL).
;; To use, add the following to your Emacs configuration:
;;
;; I. Installation
;;   A. Manual installation
;;     1. Download the `kql-mode.el' file and add it to your `custom-load-path'.
;;     2. In your `~/.emacs.d/init.el' or `~/.emacs':
;;       (add-to-list 'auto-mode-alist '("\\.kql\\'" . kql-mode))
;;
;;   B. From Melpa
;;     1. M-x package-install RET kql-mode RET.
;;     2. In your `~/.emacs.d/init.el' or `~/.emacs':
;;       (add-to-list 'auto-mode-alist '("\\.kql\\'" . kql-mode))
;;
;;   C. With `use-package'
;;     (use-package kql-mode
;;       :ensure t
;;       :config
;;       (add-to-list 'auto-mode-alist '("\\.kql\\'" . kql-mode)))
;;
;; II.  Activating the mode interactively
;;    M-x kql-mode RET


;;; Code:

(defconst kql-mode-keywords
  '("let" "project" "summarize" "order" "by" "asc" "desc" "where"
    "ago" "and" "or" "join" "on" "union" "parse" "extend" "partition"
    "top" "top-hitters" "limit" "sample" "sort" "render"))

(defconst kql-mode-keywords-regexp
  (regexp-opt kql-mode-keywords 'words))

(defconst kql-variable-regexp
  "\\b\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-?=[^=]")

(defconst kql-functions
  '("count" "sum" "avg" "min" "max" "stdev" "make-series" "hll"
    "dcount" "percentile" "between" "coalesce" "strcat" "toupper"
    "tolower" "trim" "extract" "replace" "round" "floor" "ceiling"
    "abs" "strlen" "split" "getyear" "getmonth" "getday" "gethour"
    "getminute" "getsecond" "now" "datetime_diff" "startofmonth"
    "startofyear" "bin"))

(defconst kql-functions-regexp
  (regexp-opt kql-functions 'words))

(defconst kql-operators-regexp
  "\\(\\+\\|-\\|*\\|/\\|=\\|<\\|>\\|!\\)")

(defconst kql-array-regexp
  "\\[\\([0-9]+\\)\\]")

(defconst kql-property-regexp
  "\\.\\([a-zA-Z_][a-zA-Z0-9_]*\\)")

(defconst kql-mode-font-lock-keywords
  `((,kql-mode-keywords-regexp . font-lock-keyword-face)
    (,kql-variable-regexp 1 font-lock-variable-name-face)
    (,kql-functions-regexp . font-lock-function-name-face)
    (,kql-operators-regexp . font-lock-builtin-face)
    (,kql-array-regexp . font-lock-type-face)
    (,kql-property-regexp . font-lock-function-name-face)))

(defvar kql-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; KQL-style comments
    (modify-syntax-entry ?/ ". 12b" table)
    (modify-syntax-entry ?\n "> b" table)
    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    ;; Variables
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?- "w" table)
    ;; Operators
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?! "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?= "." table)
    ;; Brackets and parentheses
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    table))

(define-derived-mode kql-mode prog-mode "KQL"
  "Major mode for highlighting Kusto Query Language."
  :syntax-table kql-mode-syntax-table
  (setq font-lock-defaults '(kql-mode-font-lock-keywords)))

(provide 'kql-mode)

;;; kql-mode.el ends here
