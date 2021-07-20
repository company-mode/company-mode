---
name: Bug report
about: Report errors or unexpected behavior
title: ''
labels: ''
assignees: ''

---

**Please make sure to search in [existing issues](https://github.com/company-mode/company-mode/issues?q=) before filing a new report!**

[Feel free to remove pre-filled text **after** reading it.]

#### Output of the command `M-x company-diag`
Before posting, look carefully at the output of this command.
You may find important clues in it for figuring out the cause of the issue.

In particular, note the value on the line starting with `Used backend:`
- Compare the value to the names of the [third-party backends](https://github.com/company-mode/company-mode/wiki/Third-Party-Packages).
  -  It's highly probable that the issue should be reported to the corresponding third-party package.
- If the value is `company-capf`, then one more clue is on the line starting with `Value of c-a-p-f:`
  -  The issue could have been caused by a function listed there.
  -  Use `M-x find-function RET <function-name> RET` to identify to which package a function belongs.

#### Describe the issue
Provide a clear and concise description of what the issue is.  
Copy/paste the exact error message: lookup for it in the **\*Messages\*** buffer.

#### Steps to reproduce
List the steps to reproduce the behavior.

#### Expected behavior
Provide a clear and concise description of what you expected to happen.

#### Screenshots
If applicable, add screenshots (not huge ones, please) to help explain your problem.

#### The error backtrace
If applicable, provide the backtrace of the error.  
To enable backtracing, run the command: `M-x toggle-debug-on-error` *before* reproducing the error.  
Copy/paste the text of the debugger’s backtrace. To disable backtracing, run the same command.

#### Additional context
Add any other context about the problem here.
