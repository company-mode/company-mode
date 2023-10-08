# History of user-visible changes

## 2023-10-08 (0.10.2)

* More `company-auto-update-doc`-related fixes.
* Better handling of `C-g` performed inside a `doc-buffer` handler
  ([#1408](https://github.com/company-mode/company-mode/issues/1408)).

## 2023-10-06 (0.10.1)

* Fix upgrading from 0.9.13 when the package is already loaded
  ([#1406](https://github.com/company-mode/company-mode/issues/1406)).

## 2023-10-04 (0.10.0)

* `company-echo-show` (and related featuers, most importantly,
  `company-echo-metadata-frontend`) now should stop interfering with the echo
  area and ElDoc when the current backend returns no `meta`.
* New user option `company-tooltip-annotation-padding`
  ([#1376](https://github.com/company-mode/company-mode/discussions/1376)).
* When a snippet name is typed in full, completion does not abort now (only
  affects completions which have `snippet` kind),
  ([#205](https://github.com/company-mode/company-mode/issues/205)).
* `company-show-doc-buffer` now can accept a prefix argument to toggle a new
  variable `company-auto-update-doc`.  When this variable is non-nil, it keeps
  the documentation buffer up-to-date whenever the selection changes
  ([#1331](https://github.com/company-mode/company-mode/discussions/1331)).
* `company-auto-commit` and `company-auto-commit-chars` have been renamed to
  `company-insertion-on-trigger` and `company-insertion-triggers` respectively
  ([#1270](https://github.com/company-mode/company-mode/pull/1270)).
* New command `company-complete-common-or-show-delayed-tooltip`
  ([#1214](https://github.com/company-mode/company-mode/discussions/1214)).
* Faces `company-scrollbar-fg` and `company-scrollbar-bg` have been renamed to
  `company-tooltip-scrollbar-thumb` and `company-tooltip-scrollbar-track`
  respectively.
* Better compatibility with `visual-line-mode`
  ([#1257](https://github.com/company-mode/company-mode/issues/1257)).
* Better compatibility with `org-indent-mode`
  ([#1252](https://github.com/company-mode/company-mode/issues/1252)).
* New backend command, `deprecated`. It returns whether the completion item is
  deprecated or not.
* `company-tooltip-common` highlightings with non-prefix and prefix matching
  backends are more compatible: if the non-prefix matching backend's completions
  all have a common part, and so the current prefix can be expanded with
  `company-complete-common`, that part is now also highlighted with that face
  ([#519](https://github.com/company-mode/company-mode/issues/519)).
* `company-yasnippet` respects the `condition` directive in snippets
  ([#1179](https://github.com/company-mode/company-mode/issues/1179)).
* New user option `company-icon-margin`.
* `company-show-numbers` has been renamed to `company-show-quick-access`
  ([#1115](https://github.com/company-mode/company-mode/pull/1115)).
  New user options `company-quick-access-keys` and
  `company-quick-access-modifier`.
  New command `company-complete-quick-access`.
  `company-show-numbers-function` has been deprecated and its default
  value changed to `nil`. Use `company-quick-access-hint-function`
  instead. `company--show-numbers` has been deprecated.
* `company-complete-number` has been renamed to
  `company-complete-tooltip-row`
  ([#1118](https://github.com/company-mode/company-mode/pull/1118)).
* New faces `company-tooltip-quick-access` and
  `company-tooltip-quick-access-selection`
  ([#303](https://github.com/company-mode/company-mode/issues/303)).
* Default colors for dark themes have been changed
  ([#949](https://github.com/company-mode/company-mode/issues/949)).
* Default key bindings have been changed, moving `company-select-next` and
  `company-select-previous` from `M-n` and `M-p` to `C-n` and `C-p`
  ([#1098](https://github.com/company-mode/company-mode/pull/1098)). The bound
  commands are also changed: `company-select-next-or-abort` and
  `company-select-previous-or-abort`, to match the `<up>` and `<down>`
  bindings. The previous bindings still work, but show a warning and will be
  disabled soon. To undo that change locally, do:

```el
(with-eval-after-load 'company
  (dolist (map (list company-active-map company-search-map))
    (define-key map (kbd "C-n") nil)
    (define-key map (kbd "C-p") nil)
    (define-key map (kbd "M-n") #'company-select-next)
    (define-key map (kbd "M-p") #'company-select-previous)))
```

* New user option `company-files-chop-trailing-slash`
  ([#1042](https://github.com/company-mode/company-mode/issues/1042)).
* Improved visual responsiveness with (but not limited to) async backends
  ([#1073](https://github.com/company-mode/company-mode/issues/1073)). New user
  option `company-async-redisplay-delay`.
* `company-idle-delay` default reduced to 0.2 (seconds).
* The minimum required version of Emacs is now 25.1.
* Added support for icons
  ([#1070](https://github.com/company-mode/company-mode/pull/1070)).
  New user option `company-format-margin-function`. New backend command
  `kind`. There are two built-in SVG icon sets, one for light and another for
  dark icons. The default behavior is to choose the best one for the current
  theme automatically, or if the current frame is non-graphical or simply does
  not support rendering SVG images, fall back to text-based "icons".
* User options `company-text-icons-mapping` and
  `company-text-icons-add-background` control the looks and additional
  decoration for the latter
  ([#1088](https://github.com/company-mode/company-mode/issues/1088)).
* New user option `company-abort-on-unique-match`
  ([#1046](https://github.com/company-mode/company-mode/issues/1046)).
* `company-select-mouse` is a new frontend action
  ([#1045](https://github.com/company-mode/company-mode/pull/1045)).
* `company-gtags` on remote hosts is improved
  ([#1037](https://github.com/company-mode/company-mode/pull/1037)).
* New commands `company-select-first` and `company-select-last`.
* `company-tng-mode` has been added to replace both
  `company-tng-configure-default` and the manual method of enabling
  `company-tng-frontend` (see also `company-tng-auto-configure`). Also,
  `company-selection` can now have `nil` value, which means no selection.
* `company-auto-complete` and `company-auto-complete-chars` have been renamed to
  `company-auto-commit` and `company-auto-commit-chars` respectively.
* `company-clang` filters out duplicates
  ([#841](https://github.com/company-mode/company-mode/issues/841)).
* New user option `company-tooltip-width-grow-only`.
* `company-xcode` has been removed. It has not been useful for years now.
* `company-clang` has been moved to after `company-capf` in the default value of
  `company-backends`. So now if there is an active completion function in
  `completion-at-point-functions`, it will have priority over
  `company-clang`. Unless it's `tags-completion-at-point-function` (this one is
  still skipped explicitly).
* `company-eclim` has been removed. Eclim is generally not recommended for Emacs
  users these days, with
  ([emacs-eclim](https://github.com/emacs-eclim/emacs-eclim/)) declared obsolete
  in favor of `lsp-java`. Though it used its own backend anyway.

## 2020-07-26 (0.9.13)

* `company-clang`: error handling is more permissive.
* `company-tng` stops disabling `post-completion` in backends
  ([#946](https://github.com/company-mode/company-mode/pull/946)). Instead,
  `company-tng-configure-default` disables snippet expansion in most popular
  backends. If a backend you use needs this and is not covered, and you use
  `company-tng`, disable snippet insertion by customizing a relevant option
  provided by the backend. The result is better compatibility with LSP backends
  because they currently depend on `post-completion` in all cases.
* `company-keywords`: additions for C and C++.
* `company-yasnippet` supports the `doc-buffer` action.
* `company-bbdb` supports more headers.

## 2020-02-07 (0.9.12)

* Tooltip rendering bugfix.
* `company-indent-or-complete-common` is better compatible with
  `indent-for-tab-command`
  ([comment](https://github.com/company-mode/company-mode/issues/94#issuecomment-571265393)).

## 2020-01-03 (0.9.11)

* New value for option `company-show-numbers` to show numbers on the left.
* `company-gtags` has some minor fixes.
* Face definitions have moved to a separate group: `company-faces`.
* `company-capf`'s `:exit-function` handling has been improved
  ([#935](https://github.com/company-mode/company-mode/issues/935)).
* New user option `company-clang-use-compile-flags-txt`
  ([#933](https://github.com/company-mode/company-mode/issues/933)).
* Support for completion style specific sorting (Emacs 27 feature).
* Snippet/template field interaction is inhibited while completion is active
  (where by default `TAB` calls `company-complete-common`, clashing with snippet
  map binding `TAB` to "jump to the next field"). Affects both
  `company-template` and `yasnippet` (requires version 0.14.0).

## 2019-04-15 (0.9.10)

* `company-clang`: better compatibility with Clang 8
  ([#885](https://github.com/company-mode/company-mode/issues/885)).
* The change in `company-clang` regarding identity #defines is reverted because
  it affected other completions as well
  ([#884](https://github.com/company-mode/company-mode/issues/884)).
* `company-idle-delay` now accepts a function which generates the idle time or
  nil indicating no idle completion.
* Add custom variable `company-show-numbers-function` to make numbers of
  candidates customizable.
* When a symbol is already typed in full, calling `M-x company-complete` will
  now run its post-completion action (e.g. inserting method parameters
  template). Calling `M-x company-manual-begin` or invoking a backend command
  directly will show the popup
  ([#150](https://github.com/company-mode/company-mode/issues/150),
  [#476](https://github.com/company-mode/company-mode/issues/476)).

## 2018-12-13 (0.9.9)

* Fix for the changes in the previous release.
* New hook `company-after-completion-hook`.
* `company-clang` removes identity preprocessor #defines from completions
  ([#841](https://github.com/company-mode/company-mode/issues/841)).

## 2018-12-08 (0.9.8)

* CAPF backend fixed to use the right `:exit-function`. It can now safely be a
  closure with lexical context capturing the buffer state at the moment when the
  completion table was returned
  ([#845](https://github.com/company-mode/company-mode/pull/845)).

## 2018-11-06 (0.9.7)

* For more sophisticated highlighting in non-prefix completion, a backend may
  now respond to a `match` request with a list of regions.  See
  `company-backends`.
  ([#798](https://github.com/company-mode/company-mode/issues/798),
  [#762](https://github.com/company-mode/company-mode/issues/762))
* The `company-capf` backend will pick up on a `:company-match` metadata element
  on the capf function (similar to `:company-location` or `:company-doc-buffer`)
  and use it as a response to aforementioned `match` request.
* `company-cmake` supports completion inside string interpolations
  ([#714](https://github.com/company-mode/company-mode/pull/714)).
* Workaround for the conflict between `inferior-python-mode`'s completion code
  and `company-sort-by-occurrence`.
* In Emacs 26 and newer, `company-css` is removed from `company-backends`.
  `company-capf` is used instead.
* Same for `company-nxml`.

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
* Further improvement in `org-indent-mode` compatibility.

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
