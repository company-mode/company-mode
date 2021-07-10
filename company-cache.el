;;; company-cache.el --- company-mode cache for other backends -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2011, 2013-2016  Free Software Foundation, Inc.

;; Author: Nikolaj Schumacher

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;; Intended for use in other company backends.

;;; Code:

(require 'cl-lib)

(cl-defstruct company-lru-cache max-size size newest oldest table)

(cl-defstruct company-lru-item key value next prev)

(defun company--lru-unlink-item (item lru)
  "Unlink ITEM from the linked list maintained in LRU."
  (let ((next (company-lru-item-next item))
        (prev (company-lru-item-prev item)))
    (if next (setf (company-lru-item-prev next) prev)
      (setf (company-lru-cache-newest lru) prev))
    (if prev (setf (company-lru-item-next prev) next)
      (setf (company-lru-cache-oldest lru) next))))

(defun company--lru-linkin-item (item lru)
  "Link ITEM into the front of the linked list maintained in LRU."
  (let ((newest (company-lru-cache-newest lru)))
    (setf (company-lru-item-next item) nil (company-lru-item-prev item) newest)
    (if newest (setf (company-lru-item-next newest) item)
      (setf (company-lru-cache-oldest lru) item))
    (setf (company-lru-cache-newest lru) item)))

(cl-defun company-lru-new (&key (size 1000) (test 'equal))
  "Create a new least-recently-used cache and return it.
Takes keyword arguments
:SIZE the maximum number of entries.
:TEST a hash table test."
  (make-company-lru-cache
   :max-size size
   :size 0
   :newest nil
   :oldest nil
   :table (make-hash-table :size size :test test)))

(defun company-lru-get (key lru &optional default)
  "Look up KEY in least-recently-used cache LRU and return its associated value.
If KEY is not found, return DEFAULT."
  (let ((item (gethash key (company-lru-cache-table lru))))
    (if item
        (progn
          (company--lru-unlink-item item lru)
          (company--lru-linkin-item item lru)
          (company-lru-item-value item))
      default)))

(defun company-lru-rem (key lru)
  "Remove KEY from least-recently-used cache LRU."
  (let ((item (gethash key (company-lru-cache-table lru))))
    (when item
      (remhash (company-lru-item-key item) (company-lru-cache-table lru))
      (company--lru-unlink-item item lru)
      (cl-decf (company-lru-cache-size lru)))))

(defun company-lru-put (key value lru)
  "Associate KEY with VALUE in LRU.
If KEY is already present in LRU, replace its current value with VALUE."
  (let ((item (gethash key (company-lru-cache-table lru))))
    (if item
        (setf (company-lru-item-value item) value)
      (when (= (company-lru-cache-size lru) (company-lru-cache-max-size lru))
        (company-lru-rem (company-lru-item-key (company-lru-cache-oldest lru)) lru))
      (let ((newitem (make-company-lru-item :key key :value value)))
        (company--lru-linkin-item newitem lru)
        (puthash key newitem (company-lru-cache-table lru))
        (cl-incf (company-lru-cache-size lru)))))
  value)

(provide 'company-cache)
;;; company-cache.el ends here
