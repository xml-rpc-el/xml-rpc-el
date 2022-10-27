;;; xml-rpc-test.el --- Tests for xml-rpc.el  -*- lexical-binding:t -*-

;; Copyright (C) 2015-2022 xml-rpc.el contributors

;; Maintainer: Mark A. Hershberger <mah@everybody.org>

;; This file is NOT (yet) part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ert)

(load-file "xml-rpc.el")

(ert-deftest test-xml-rpc-value-structp ()
  "Test whether xml-rpc-value-structp operates correctly"
  (should (eq (xml-rpc-value-structp ()) t))
  (should (eq (xml-rpc-value-structp '(("foo"))) t))
  (should (eq (xml-rpc-value-structp '(("foo" . "bar"))) t))
  (should (eq (xml-rpc-value-structp '(("foo" :datetime (12345 12345)))) t)))

(defconst xml-rpc-test-http-data
  "HTTP/1.1 200 OK
Date: Sun, 06 Sep 2020 00:48:09 GMT
Server: Apache/2.4.46 (Debian)
Vary: Accept-Encoding
Content-Length: 123
Connection: close
Content-Type: text/xml

<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<methodResponse>
<params>
<param><value><string>0.9.8</string></value></param>
</params>
</methodResponse>")

(defconst xml-rpc-test-scgi-data
  "Status: 200 OK
Content-Type: text/xml
Content-Length: 152

<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<methodResponse>
<params>
<param><value><string>0.9.8</string></value></param>
</params>
</methodResponse>")

(defconst xml-rpc-test-result
  '((methodResponse nil (params nil (param nil (value nil (string nil "0.9.8")))))))

(ert-deftest test-xml-rpc-request-process-buffer/xml.el ()
  (let ((xml-rpc-parse-region-function #'xml-parse-region))
    (dolist (data (list xml-rpc-test-http-data
                        xml-rpc-test-scgi-data))
      (with-temp-buffer
        (insert data)
        (should (equal (xml-rpc-request-process-buffer (current-buffer))
                       xml-rpc-test-result))))))

(ert-deftest test-xml-rpc-request-process-buffer/libxml ()
  (skip-unless (and (fboundp 'libxml-available-p)
                    (libxml-available-p)))
  (let ((xml-rpc-parse-region-function #'libxml-parse-xml-region))
    (dolist (data (list xml-rpc-test-http-data
                        xml-rpc-test-scgi-data))
      (with-temp-buffer
        (insert data)
        (should (equal (xml-rpc-request-process-buffer (current-buffer))
                       xml-rpc-test-result))))))

;;; xml-rpc-test.el ends here
