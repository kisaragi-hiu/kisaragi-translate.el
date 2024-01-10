;;; test-kisaragi-translate.el --- Tests -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:
(require 'buttercup)
(require 'gettext-parser)

(describe "PO parser"
  ;; Fails because it's trying to parse a key name on the header for some reason
  (it "should parse all known headers"
    (gettext-parser-po-parse
     (with-temp-buffer
       (insert-file-contents "fixtures/headers-known.po")
       (buffer-string)))))

;;; test-kisaragi-translate.el ends here
