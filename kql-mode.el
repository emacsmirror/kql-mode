;;; kql-mode.el --- Major mode for highlighting KQL -*- lexical-binding: t -*-

;; Author: Aimé Bertrand
;; Version: 0.1
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
;; A Major mode for syntax highlighting of KQL code/files


;;; Code:


;;; Define a syntax table
(defvar kql-mode-syntax-table nil
  "Syntax table for `kql-mode'.")

(setq kql-mode-syntax-table
      (let ((syn-table (make-syntax-table)))
        ;; KQL-style comments
        (modify-syntax-entry ?/ ". 12b" syn-table)
        (modify-syntax-entry ?\n "> b"   syn-table)

        ;; Strings
        (modify-syntax-entry ?\" "\"" syn-table)

        ;; Variables can start with underscore or a letter
        (modify-syntax-entry ?_ "w" syn-table)
        (modify-syntax-entry ?- "w" syn-table)

        ;; Operators are punctuation
        (modify-syntax-entry ?+ "." syn-table)
        (modify-syntax-entry ?* "." syn-table)
        (modify-syntax-entry ?% "." syn-table)
        (modify-syntax-entry ?& "." syn-table)
        (modify-syntax-entry ?| "." syn-table)
        (modify-syntax-entry ?! "." syn-table)
        (modify-syntax-entry ?< "." syn-table)
        (modify-syntax-entry ?> "." syn-table)
        (modify-syntax-entry ?= "." syn-table)

        ;; Brackets and parentheses
        (modify-syntax-entry ?\( "()" syn-table)
        (modify-syntax-entry ?\) ")(" syn-table)
        (modify-syntax-entry ?\[ "(]" syn-table)
        (modify-syntax-entry ?\] ")[" syn-table)
        (modify-syntax-entry ?{ "(}" syn-table)
        (modify-syntax-entry ?} "){" syn-table)

        syn-table))


;;; Regexp for syntax
(setq kql-mode-keywords
      '("let" "project" "summarize" "order" "by" "asc" "desc" "where"
        "ago" "and" "or" "join" "on" "union" "parse" "extend" "partition"
        "top" "top-hitters" "limit" "sample" "sort" "render"))

(setq kql-mode-keywords-regexp
      (regexp-opt kql-mode-keywords 'words))

(setq kql-variable-regexp
      "\\b\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-?=[^=]")

(setq kql-functions
      '("count" "sum" "avg" "min" "max" "stdev" "make-series" "hll"
        "dcount" "percentile" "between" "coalesce" "strcat" "toupper"
        "tolower" "trim" "extract" "replace" "round" "floor" "ceiling"
        "abs" "strlen" "split" "getyear" "getmonth" "getday" "gethour"
        "getminute" "getsecond" "now" "datetime_diff" "startofmonth"
        "startofyear" "bin"))

(setq kql-functions-regexp
      (regexp-opt kql-functions 'words))

(setq kql-operators-regexp
      "\\(\\+\\|-\\|*\\|/\\|=\\|<\\|>\\|!\\)")

(setq kql-array-regexp
      "\\[\\([0-9]+\\)\\]")

(setq kql-property-regexp
      "\\.\\([a-zA-Z_][a-zA-Z0-9_]*\\)")


;;; Faces for syntax
(setq kql-mode-keyword-highlight
      `(,kql-mode-keywords-regexp . font-lock-keyword-face))

(setq kql-mode-variable-highlight
      `(,kql-variable-regexp 1 font-lock-variable-name-face))

(setq kql-mode-function-highlight
      `(,kql-functions-regexp . font-lock-function-name-face))

(setq kql-mode-operator-highlight
      `(,kql-operators-regexp . font-lock-builtin-face))

(setq kql-mode-array-highlight
      `(,kql-array-regexp . font-lock-type-face))

(setq kql-mode-property-highlight
      `(,kql-property-regexp . font-lock-function-name-face))


;;; Variable for highlight rules
(setq kql-mode-font-lock-keywords
      (list kql-mode-keyword-highlight
            kql-mode-variable-highlight
            kql-mode-function-highlight
            kql-mode-operator-highlight
            kql-mode-array-highlight
            kql-mode-property-highlight))


(define-derived-mode kql-mode prog-mode "KQL"
  "Major mode for highlighting Kusto Query Language."
  :syntax-table kql-mode-syntax-table
  ;; Syntax highlighting
  (setq-local font-lock-defaults '(kql-mode-font-lock-keywords)))


(provide 'kql-mode)

;;; kql-mode.el ends here
