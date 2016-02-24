#|
 This file is a part of documentation-utils
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.documentation-utils)

(docs:define-docs
  (defvar *documentation-tests*
    "Holds an alist of documentation types to test functions.

The function should take one argument, the specifier, and
return non-NIL if the symbol is bound for the given type.")

  (defun documentation-test
    "Access the documentation test function for the given type.

See *DOCUMENTATION-TESTS*")
  
  (defun remove-documentation-test
    "Remove the documentation test function for the given type.

See *DOCUMENTATION-TESTS*")

  (defmacro define-documentation-test
    "Shorthand to define a documentation test function.

See *DOCUMENTATION-TESTS*")

  (defvar *documentation-translators*
    "Holds an alist of documentation types to translator functions.

The function should take one argument, the specifier expression, and
return a documentation form suitable to access the documentation
for the given type.")

  (defun documentation-translator
    "Access the documentation translator function for the given type.

See *DOCUMENTATION-TRANSLATORS*")

  (defun remove-documentation-translator
    "Remove the documentation translator function for the given type.

See *DOCUMENTATION-TRANSLATORS*")

  (defmacro define-documentation-translator
    "Shorthand to define a documentation translator function.

See *DOCUMENTATION-TRANSLATORS*")

  (defmacro define-documentation-alias
    "Shorthand to define an alias to a translator.

This simply sets a delegating function that refers to the given type.

See *DOCUMENTATION-TRANSLATORS*")

  (defun check
    "Checks whether all symbols have documentation for all known types.

If documentation is not set for a given symbol and type combination, a
warning is signalled.

See *DOCUMENTATION-TESTS*")

  (defmacro define-docs
    "Allows you to comfortably and easily set the documentation for your library.

Each expression in the body can either take a two or many argument structure.
In the two argument structure, the type is implicitly assumed to be 
FUNCTION. The first argument is then the specifier, and the second the
docstring. In the many argument structure the first argument is the
type, the last is the docstring, and everything in between the specifier.

The expansion of the documentation accessor --and thus the structure of
the specifier-- is dependant on the applicable documentation translator.
By default, the expansion is simply (DOCUMENTATION SPECIFIER TYPE).

See *DOCUMENTATION-TRANSLATORS*"))
