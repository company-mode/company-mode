# History of user-visible changes

## 2013-03-19 (0.6)

* Across-the-board bugfixing.
* `company-pysmell` is not used by default anymore.
* Loading of `nxml`, `semantic`, `pymacs` and `ropemacs` is now deferred.
* Candidates from grouped back-ends are merged more conservatively: only
  back-ends that return the same prefix at point are used.
* `company-clang` now shows meta information, too.
* Some performance improvements.
* Fixed two old tooltip annoyances.
* Instead of `overrriding-terminal-local-map`, we're now using
  `emulation-mode-map-alists` (experimental).  This largely means that when the
  completion keymap is active, other minor modes' keymaps are still used, so,
  for example, it's not as easy to accidentally circumvent `paredit-mode`
  when it's enabled.
* `company-elisp` has seen some improvements.
* Added `company-capf`: completion adapter using
  `completion-at-point-functions`.  (Stefan Monnier)
* Clang completions now include macros and are case-sensitive.
* Switching between tag files now works correctly with `company-etags`.

## 2010-02-24 (0.5)

* `company-ropemacs` now provides location and docs.  (Fernando H. Silva)
* Added `company-with-candidate-inserted` macro.
* Added `company-clang` back-end.
* Added new mechanism for non-consecutive insertion.
  (So far only used by clang for ObjC.)
* The semantic back-end now shows meta information for local symbols.
* Added compatibility for CEDET in Emacs 23.2 and from CVS.  (Oleg Andreev)

## 2009-05-07 (0.4.3)

* Added `company-other-backend`.
* Idle completion no longer interrupts multi-key command input.
* Added `company-ropemacs` and `company-pysmell` back-ends.

## 2009-04-25 (0.4.2)

* In C modes . and -> now count towards `company-minimum-prefix-length`.
* Reverted default front-end back to `company-preview-if-just-one-frontend`.
* The pseudo tooltip will no longer be clipped at the right window edge.
* Added `company-tooltip-minimum`.
* Windows compatibility fixes.

## 2009-04-19 (0.4.1)

* Added `global-company-mode`.
* Performance enhancements.
* Added `company-eclim` back-end.
* Added safer workaround for Emacs `posn-col-row` bug.

## 2009-04-18 (0.4)

* Automatic completion is now aborted if the prefix gets too short.
* Added option `company-dabbrev-time-limit`.
* `company-backends` now supports merging back-ends.
* Added back-end `company-dabbrev-code` for generic code.
* Fixed `company-begin-with`.

## 2009-04-15 (0.3.1)

* Added 'stop prefix to prevent dabbrev from completing inside of symbols.
* Fixed issues with tabbar-mode and line-spacing.
* Performance enhancements.

## 2009-04-12 (0.3)

* Added `company-begin-commands` option.
* Added abbrev, tempo and Xcode back-ends.
* Back-ends are now interactive.  You can start them with M-x backend-name.
* Added `company-begin-with` for starting company from elisp-code.
* Added hooks.
* Added `company-require-match` and `company-auto-complete` options.

## 2009-04-05 (0.2.1)

* Improved Emacs Lisp back-end behavior for local variables.
* Added `company-elisp-detect-function-context` option.
* The mouse can now be used for selection.

## 2009-03-22 (0.2)

* Added `company-show-location`.
* Added etags back-end.
* Added work-around for end-of-buffer bug.
* Added `company-filter-candidates`.
* More local Lisp variables are now included in the candidates.

## 2009-03-21 (0.1.5)

* Fixed elisp documentation buffer always showing the same doc.
* Added `company-echo-strip-common-frontend`.
* Added `company-show-numbers` option and M-0 ... M-9 default bindings.
* Don't hide the echo message if it isn't shown.

## 2009-03-20 (0.1)

* Initial release.
