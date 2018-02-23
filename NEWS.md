# History of user-visible changes

## 2018-02-23 (0.9.6)

* Workaround for Emacs' ([bug#23980](https://debbugs.gnu.org/23980)) triggered
  in combination with Flyspell.

## 2018-02-18 (0.9.5)

* The most common case of tooltip flickering with asynchronous backends (and
  disabled built-in cache) is fixed
  ([#510](https://github.com/company-mode/company-mode/issues/510),
  [#654](https://github.com/company-mode/company-mode/issues/654)).
* `company-keywords` added entries for `go-mode`, `swift-mode` and
  `kotlin-mode`.
* Native line numbers compatibility fixes.
* `company-dabbrev` and `company-dabbrev-code` are more responsive when user
  input is pending
  ([#720](https://github.com/company-mode/company-mode/pull/720)).
* New feature `company-tng`. It contains a frontend and some helper code.
  The frontend triggers insertion of the candidate as soon as it's selected, so
  you only need to press TAB. Add `(company-tng-configure-default)` to your
  init script to give it a try
  ([#706](https://github.com/company-mode/company-mode/issues/706)).
* New user option `company-tooltip-maximum-width`.

## 2017-07-15 (0.9.4)

* Compatibility with native line numbers display in Emacs 26.
* `company-files` allows completion after `=`.
* `company-template` has a new shortcut (`C-d`) for deleting an unmodified
  template field while cursor is on it.

## 2017-03-29 (0.9.3)

* New user option `company-echo-truncate-lines`.
* `company-auto-complete` improved compatibility with `electric-pair-mode`.
* Use of `overriding-terminal-local-map` does not disable completion.
* `company-clang` and `company-gtags` can work over Tramp.
* New frontend `company-preview-common-frontend`.
* `company-clang` calls Clang using a pipe instead of pty.
* The minimum required version of Emacs is now 24.3.

## 2016-11-14 (0.9.2)

* Miscellaneous fixes and docstring improvements.

## 2016-11-12 (0.9.1)

* `company-indent-or-complete-common` skips trying to indent if
  `indent-line-function` is `indent-relative` or `indent-relative-maybe`.
* Better visualization of search matches. New face `company-tooltip-search-selection`.
* New user option `company-files-exclusions`.
* `company-next-page` and `company-previous-page` adhere to
  `company-selection-wrap-around` docstring more closely and only wrap around
  when the selection is at the start of the end of the list.
* `company-pseudo-tooltip-unless-just-one-frontend-with-delay` handles custom
  frontends derived from `company-preview-frontend` better.
* `company-idle-delay` is automatically adjusted to a non-zero value.

## 2016-06-23 (0.9.0)

* Group of backends can now contain keyword `:separate`, which makes candidates
  from different backends sorted separately in the combined list.
* New frontend `company-pseudo-tooltip-unless-just-one-frontend-with-delay`.
* New transformer `company-sort-prefer-same-case-prefix`.
* The value of `company-dabbrev-ignore-buffers` can also be a function.
* `company-files` has been moved to right after `company-capf` in
  `company-backends`
  ([#463](https://github.com/company-mode/company-mode/issues/463)).
* `company-semantic-insert-arguments`: New option. Like in `company-clang`.
* `company-semantic-begin-after-member-access`: New option. Similar to the one
  in `company-clang`.
* `company-capf` accepts `:company-prefix-length` property value.
* New face `company-tooltip-annotation-selection`, used for the annotation in
  the selected tooltip line.
* `company-clang-objc-templatify` has been renamed to
  `company-template-objc-templatify`.
* New user option `company-etags-everywhere`.
* `company-yasnippet` supports `yas-key-syntaxes` better. But we use them in the
  reverse order, preferring the longest key prefix that matches anything. And we
  only consider trigger key prefixes that are at least as long as the symbol at
  point, which effectively means skipping the `"w"` element
  ([#422](https://github.com/company-mode/company-mode/issues/422)).
* New user option `company-search-regexp-function`.
* Completion is not started automatically when a keyboard macro is being
  recorded ([#374](https://github.com/company-mode/company-mode/issues/374)).
* New command `company-indent-or-complete-common`.
* Backend command `doc-buffer` now can also return a cons of buffer and window
  start position.
* Backend command `ignore-case` has been documented.
* `company-template-c-like-templatify` does not replace the default argument
  values with `argN` anymore
  ([#336](https://github.com/company-mode/company-mode/issues/336)). This
  affects `company-clang` and all third-party backends that use this function.
* Likewise for `company-clang-objc-templatify`.
* `company-template-add-field` calling convention has changed.
* New user option `company-dabbrev-ignore-invisible`.
* `company-ropemacs` was removed. `ropemacs` supports completion via
  `completion-at-point-functions` starting with version 0.8.
* `company-pysmell` was removed.
* `company-select-next`, `company-select-previous`,
  `company-select-next-or-abort`, `company-select-previous-or-abort` and
  `company-complete-common-or-cycle` accept a numeric argument.
* The documentation buffer window can be scrolled with the mouse wheel.
* New command `company-diag`. Use it in bug reports.

## 2015-02-02 (0.8.10)

* New variable `company-lighter-base`.
* Better tracking of the current selection.
* Pressing `M-0`...`M-9` works in the search mode.
* Pressing `<up>` or `<down>` doesn't quit the search mode.

## 2015-01-23 (0.8.9)

* New commands `company-next-page` and `company-previous-page`, remapping
  `scroll-up-command` and `scroll-down-command` during completion.

## 2015-01-13 (0.8.8)

* Pressing `M-n` or `M-p` doesn't quit the search mode.
* New command `company-complete-common-or-cycle`. No default binding.
* `company-search-toggle-filtering` replaced `company-search-kill-others`.
* Quitting the search mode resets the filtering.
* Pressing `backspace` in the search mode deletes the character at the end of
  the search string.
* `company-semantic` displays function arguments as annotations.
* New user option, `company-bbdb-modes`.
* `company-show-numbers` and `company-complete-number` now use visual numbering
  of the candidates, taking into account only the ones currently displayed.
* `company-complete-number` can be bound to keypad numbers directly, with or
  without modifiers.
* `company-cmake` expands `<LANG>` and `<CONFIG>` placeholders inside variable
  names.

## 2014-10-15 (0.8.6)

* `company-clang` and `company-template-c-like-templatify` support templated
  functions and arguments.
* `company-dabbrev` ignores "uninteresting" buffers by default. Depends on the
  new user option, `company-dabbrev-ignore-buffers`.
* `company-files` checks directory's last modification time.
* `company-files` supports relative paths and Windows drive letters.

## 2014-08-13 (0.8.4)

* `company-ropemacs` is only used when `ropemacs-mode` is on.
* `company-gtags` is enabled in all `prog-mode` derivatives by default.
* `company-end-of-buffer-workaround` is not used anymore.
* `company-begin-commands` includes some of `cc-mode` commands.

## 2014-08-27 (0.8.3)

* On Emacs 24.4 or newer, tooltip positioning takes line-spacing into account.
* New face `company-tooltip-search`, used for the search string in the tooltip.
* The default value of `company-dabbrev-minimum-length` is set to 4, independent
  of the `company-minimum-prefix-length` value.

## 2014-07-26 (0.8.2)

* New user option `company-occurrence-weight-function`, allowing to tweak the
  behavior of the transformer `company-sort-by-occurrence`.
* Setting `company-idle-delay` to `t` is deprecated. Use the value 0 instead.

## 2014-07-01 (0.8.1)

* `company-require-match` is not in effect when the new input doesn't continue
  the previous prefix, and that prefix was a match.
* The meaning of `company-begin-commands` value t has slightly changed.
* New transformer, `company-sort-by-backend-importance`.
* When grouped back-ends are used, the back-end of the current candidate is
  indicated in the mode-line, enclosed in angle brackets.
* New user option `company-gtags-insert-arguments`, t by default.
* `company-css` knows about CSS3.
* `company-gtags` supports `meta` and `annotation`.
* User option `company-dabbrev-code-other-buffers` can have a new value: `code`.
* New user option `company-tooltip-flip-when-above`.
* `company-clang` uses the standard header search paths by default.
* `C-h` is bound to `company-show-doc-buffer` (like `f1`).

## 2014-04-19 (0.8.0)

* `company-capf` is included in `company-backends` in any supported Emacs
  version (>= 24.1). `company-elisp` goes before it if Emacs version is < 24.4.
* New user option `company-clang-insert-arguments`, by default t.
* Default value of `company-idle-delay` lowered to `0.5`.
* New user option `company-tooltip-minimum-width`, by default 0.
* New function `company-grab-symbol-cons`.
* `company-clang` fetches completion candidates asynchronously.
* Added support for asynchronous back-ends (experimental).
* Support for back-end command `crop` dropped (it was never documented).
* Support for Emacs 23 dropped.
* New user option `company-abort-manual-when-too-short`.

## 2014-03-25 (0.7.3)

* New user option `company-etags-ignore-case`.

## 2014-03-19 (0.7.2)

* Support for Emacs 22 officially dropped.
* `company-clang` supports `indent-tabs-mode` and multibyte chars before point.

## 2014-03-18 (0.7.1)

* Group of back-ends can now contain keyword `:with`, which makes all back-ends
  after it to be skipped for prefix calculation.
* New function `company-version`.
* New bundled back-end `company-yasnippet`.
* Completion candidates returned from grouped back-ends are tagged to remember
  which back-end each came from.
* New user option `company-tooltip-align-annotations`, off by default.
* New bundled back-end `company-bbdb`.

## 2014-02-18 (0.7)

* New back-end command, `match`, for non-prefix completion.
* New user option `company-continue-commands`. The default value aborts
  completion on buffer saving commands.
* New back-end command, `annotation`, for text displayed inline in the popup
  that's not a part of completion candidate.
* `company-capf`, `company-clang` and `company-eclim` use `annotation`.
* `company-preview*` faces inherit from `company-tooltip-selection` and
  `company-tooltip-common-selection` on light themes.
* New user option `company-transformers`.
* First transformer, `company-sort-by-occurrence`.
* New user options controlling `company-dabbrev` and `company-dabbrev-code`.

## 2014-01-25 (0.6.14)

* The tooltip front-end is rendered with scrollbar, controlled by the user
  option `company-tooltip-offset-display`.
* The tooltip front-end is rendered with margins, controlled by the user option
  `company-tooltip-margin`.

## 2014-01-14 (0.6.13)

* Experimental support for non-prefix completion.
* Starting with Emacs version 24.4, `company-capf` is included in
  `company-backends` and replaces `company-elisp`.
* `company-capf` supports completion tables that return non-default boundaries.
* `company-elisp` is enabled in `inferior-emacs-lisp-mode`.

## 2013-09-28 (0.6.12)

* Default value of `company-begin-commands` changed to `(self-insert-command)`.
* Futher improvement in `org-indent-mode` compatibility.

## 2013-08-18 (0.6.11)

* `company-template-c-like-templatify` removes all text after closing paren, for
  use in backends that display additional info there.
* `company-cmake` is now bundled.
* Better `linum` compatibility in Emacs <= 24.2.
* `company-global-modes`: New option.

## 2013-05-26 (0.6.10)

* Plays nicer with `org-indent-mode`.
* Works in horizontally scrolled windows.

## 2013-05-10 (0.6.9)

* `company-capf` respects `:exit-function` completion property.
* `company-backends`: `prefix` command can return `t` in the cdr.
* `company-clang-begin-after-member-access`: New option.
* Mouse click outside the tooltip aborts completion.
* `company-clang` uses standard input to pass the contents of current buffer to
  Clang 2.9+, otherwise saves the buffer and passes the path to the file.
* `company-clang-auto-save` option has been removed.
* Better interaction with `outline-minor-mode`.
* `company-dabbrev-code` supports all `prog-mode` derivatives.

## 2013-04-16 (0.6.8)

* `company-auto-complete` is disabled by default.
* `company-auto-complete-chars` default value includes fewer syntax classes.
* In expanded function calls, arguments skipped by the user default to "argN".
* `company-eclim` and `company-clang` do not strip argument types from fields.
* `company-clang` expands function calls for all three modes now.
* `company-clang` supports `c++-mode` by default.

## 2013-04-05 (0.6.7)

* Two `company-elisp` tweaks.

## 2013-04-01 (0.6.6)

* `company-elisp` doesn't offer completions when typing the name and the
  arguments of a new function or macro definition, allowing to fall back to
  other back-ends like `company-dabbrev-code`.

## 2013-03-30 (0.6.5)

* Fixed keybindings when running in a terminal.
* `company-elisp-show-locals-first`: new customizable variable.
* `company-elisp` shows more accurate and comprehensive candidates list.

## 2013-03-26 (0.6.4)

* `company-eclim` shows valid completions after an opening paren.
* Expanded template does not get removed until the point leaves it.  After your
  input the last argument in a method call expanded by `company-eclim`, you can
  press `<tab>` once more, to jump after the closing paren.  No other bundled
  back-ends are affected.

## 2013-03-25 (0.6.3)

* New tooltip face colors used on themes with light background.
* Pseudo-tooltip stays up-to-date when text is inserted after the point.
* Fixed `company-require-match` mechanics.

## 2013-03-24 (0.6.2)

* `global-company-mode` is now autoloaded.

## 2013-03-23 (0.6.1)

* Documented `init` and `post-completion` back-end commands.
* `company-eclim` and `company-clang` only expand the template on explicit user
  action (such as `company-complete-{selection,number,mouse}`).
* `company-template` has some breaking changes.  When point is at one of the
  fields, it's displayed at the beginning, not right after it; `<tab>` jumps to
  the next field, `forward-word` and `subword-forward` remappings are removed;
  when you jump to the next field, if the current one hasn't been edited, the
  overlay gets removed but the text remains.
* `company-eclim` shows method overloads and expands templates for calls.
* `company-clang-objc-templatify` does not insert spaces after colons anymore.
* `company-clang` is now only initialized in supported buffers.
  So, no error messages if you don't have Clang until you open a C file.
* `company-clang` recognizes Clang included in recent Xcode.
* New commands `company-select-previous-or-abort` and
  `company-select-next-or-abort`, bound to `<up>` and `<down>`.

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
