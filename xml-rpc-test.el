(require 'ert)

(load-file "xml-rpc.el")

(ert-deftest test-xml-rpc-value-structp ()
  "Test whether xml-rpc-value-structp operates correctly"
  (should (eq (xml-rpc-value-structp ()) t))
  (should (eq (xml-rpc-value-structp '(("foo"))) t))
  (should (eq (xml-rpc-value-structp '(("foo" . "bar"))) t))
  (should (eq (xml-rpc-value-structp '(("foo" :datetime (12345 12345)))) t)))
