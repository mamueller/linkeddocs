## linkeddocs
This R-package is an inlinedocs fork, that aims at compatibility in terms of the syntax of the documentation creating comments but also on a complete reimplementation.

## Differences in the implementation:
 * The package uses `devtools` to load the package to be documented in the same way as `library` or `require` would do,
   especially honoring the `NAMESPACE` file. The starting point of all documentation is the resulting environment 
   containing all the objects.
 * For every type of object there is a specific method to produce the Rd file. 
   There are different methods for:
   * S4 classes
   * Generic functions
   * S4 methods
   * normal functions
   * whatever will be implemented

 * In contrast to `inlinedocs` which uses `package.skeleton' to produce Rd files, parses and extends them afterwards, `linkeddocs` produces all the Rd files from the code objects directly. 
  
 * In contrast to `inlinedocs` and `roxygen2` `linkeddocs` allows S4methods to have their own Rd files which are **linked** to boht classes and Generics. 
 * It is also planned to link automatically to calling and called functions
