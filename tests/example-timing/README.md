# Example Timing Inventory

Run from the package root with Rscript from the R installation containing the
package dependencies, `pkgload`, and `processx`:

```powershell
& 'C:/Program Files/R/R-4.6.1/bin/Rscript.exe' tests/example-timing/run-example-timings.R
```

All generated help pages containing examples are included, including documented
helpers. Each complete example block runs in a separate process with a temporary
working directory, PDF graphics, default RCPA4 pauses, and a 90-second timeout.
`donttest` blocks execute; `dontrun` blocks and interactive guards do not.

The CSV records each run's example execution time, setup time, warnings, and
errors. The summary CSV and Markdown report classify pages using the slowest run.
Pages initially between 3 and 7 seconds run three times. The timer excludes source
package loading via pkgload; this is a development inventory, not a replacement
for timing an installed package during R CMD check.

Pass a package-root path and an optional output-directory path as arguments to
write a separate inventory. This script is a development tool and is not called
by the automated test suite.
