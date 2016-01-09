## Test environments
* local OS X install, 3.2.3
* win-builder (devel and release)

## Comment:
This submission aims to address concerns Prof. Ripley raised with certain unit-tests not being portable across platforms.


## R CMD check results
There were no ERRORs or WARNINGs. 

There were 1 NOTE (Windows build only):

* *Package suggested but not available for checking: 'doMC'.* This is because the suggested package is only available for Windows.

## Downstream dependencies
There are currently no downstream dependencies for this package