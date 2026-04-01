This is a resubmission of fluorojip.



Following the previous feedback, the package was rebuilt with a recent version of R using R CMD build.



Test environment:

\- Local Windows 11 x64

\- R 4.5.3



R CMD check --as-cran results:

\- 0 errors

\- 0 warnings

\- 2 notes



Notes:

\- This is a new submission.

\- The note "unable to verify current time" appeared during "checking for future file timestamps" and seems to be related to the local Windows environment rather than to the package itself.



The remaining note

"Package has a VignetteBuilder field but no prebuilt vignette index"

also appeared in the previous incoming pre-tests. The package uses non-Sweave vignettes, the vignette metadata are present, and the package passes locally with:

\- checking installed files from 'inst/doc' ... OK

\- checking package vignettes ... OK

\- checking re-building of vignette outputs ... OK

\- checking PDF version of manual ... OK

\- checking HTML version of manual ... OK

