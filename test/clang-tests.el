(require 'company-tests)
(require 'company-clang)

(ert-deftest company-clang-objc-templatify ()
  (with-temp-buffer
    (let ((text "createBookWithTitle:andAuthor:"))
      (insert text)
      (company-clang-objc-templatify text)
      (should (equal "createBookWithTitle:arg0 andAuthor:arg1" (buffer-string)))
      (should (looking-at "arg0"))
      (should (null (overlay-get (company-template-field-at) 'display))))))

(ert-deftest company-clang-simple-annotation ()
  (let ((str (propertize
              "foo" 'meta
              "wchar_t * wmemchr(wchar_t *__p, wchar_t __c, size_t __n)")))
    (should (equal (company-clang 'annotation str)
                   "(wchar_t *__p, wchar_t __c, size_t __n)"))))

(ert-deftest company-clang-generic-annotation ()
  (let ((str (propertize
              "foo" 'meta
              "shared_ptr<_Tp> make_shared<typename _Tp>(_Args &&__args...)")))
    (should (equal (company-clang 'annotation str)
                   "<typename _Tp>(_Args &&__args...)"))))
