## About documentation-utils
This is a small library to help you with managing the docstrings for your library.

## How To
The central element is the `define-docs` macro. It takes a body of expressions to define the documentation. In the simplest form, this looks like so:

    (docs:define-docs
      (my-function "Some documentation"))

If you need a different type of documentation, or want to be explicit, prepend its type to the expression.

    (docs:define-docs
      (function my-function "Some documentation")
      (variable *my-variable* "Something else"))

In order to make things look more homely, aliases exist that can be used instead:

    (docs:define-docs
      (defun my-function
        "Some documentation")
      (defvar *my-variable*
        "Something else"))

Aliases exist for most of the `def*` expressions. Some expressions can take multiple arguments for the specifier, but the last in the expression is always the docstring:

    (docs:define-docs
      (defmethod foo :append ((num integer) other)
        "stuff"))

You can also extend this system for your own documentation translators. If you need more complex behaviour than the default of `(documentation specifier type)`, see `define-documentation-translator`. If you are defining a new documentation type, you should also add a `documentation-test` to ensure that `check` can verify that you actually did set a documentation.
