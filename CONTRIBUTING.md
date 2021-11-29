# Contributing to Company

### Fixes and Improvements

You're welcome to open a [`pull request`](https://docs.github.com/en/github/collaborating-with-pull-requests)
or send a patch with the changes.  
For non-trivial updates, please clearly describe the problem and the solution.

If you're looking for *a challenge*, go grab an Issue with the
[`wishlist` label](https://github.com/company-mode/company-mode/issues?q=is%3Aissue+is%3Aopen+label%3Awishlist).

If you have *a question* on where to start implementing a feature,
ask in a related [Issue](https://github.com/company-mode/company-mode/issues)
or create a new [Discussion](https://github.com/company-mode/company-mode/discussions).


### Documentation

Help on improving, fixing, and writing documentation is also wanted.  
See these ideas on where to start:
- Add and edit pages in [`Company` Wiki](https://github.com/company-mode/company-mode/wiki).
- Share your findings in [`Discussions`](https://github.com/company-mode/company-mode/discussions/categories/show-and-tell).
- Improve the [user manual](https://github.com/company-mode/company-mode/issues/926).


### Backend Integration

New backends are rarely accepted for inclusion into `Company` at this stage.

Our common recommendation for new backends is one of the following:
- Publish a backend to (M)ELPA.
- Create a `*-completion-at-point` function for a call by `completion-at-point-functions`.

Feel free to [ask](https://github.com/company-mode/company-mode/discussions)
if you're hesitating which approach to choose.


### Guidelines for Third-Party Packages

This section was born as a result of the question asked in
[Issue #923](https://github.com/company-mode/company-mode/issues/923).

There are two preferred ways to integrate a third-party package with `Company`.

1. Use `completion-at-point-functions` and convey extra information through the
`:company-*` additional properties (like e.g. `elisp-completion-at-point` does).
In this scenario, some minor mode can and should call `add-hook`.

2. Define a `Company` backend (as described in `Commentary` at the top of
`company.el` and in `company-backends` docstring).
Then, in the documentation, describe how you recommend it to be used.


Buffer-local values are allowed, but usually not necessary.  
If a backend is added globally, checking a major mode in the prefix function is a must.

Examples:
- https://github.com/pythonic-emacs/company-anaconda#usage
- https://github.com/vspinu/company-math#activation


### Copyright Assignment

`Company` is subject to the same [copyright assignment](https://www.fsf.org/licensing/contributor-faq)
policy as `GNU Emacs`.

Any [legally significant](https://www.gnu.org/prep/maintain/html_node/Legally-Significant.html#Legally-Significant)
contributions can only be merged after the author has completed their paperwork.
Please ask for the request form, and we'll send it to you.
