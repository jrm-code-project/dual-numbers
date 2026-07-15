# DUAL-NUMBERS - Instruction and Workspace Context

This document provides architectural context, building/running instructions, testing procedures, and development conventions for the `dual-numbers` library.

## Project Overview

`dual-numbers` is a Common Lisp library for working with **dual numbers** (numbers of the form $a + b\epsilon$ where $\epsilon^2 = 0$). It supports basic arithmetic, trigonometric, hyperbolic, and transcendental functions, with its primary application being **automatic differentiation**.

### Main Technologies and Dependencies
- **Common Lisp** (specifically tested with Steel Bank Common Lisp - SBCL)
- **ASDF** (Another System Definition Facility) for system definitions
- **Quicklisp** for dependency management
- **alexandria** (utility library)
- **generic-arithmetic** (for overloading standard arithmetic and math operators with generic methods)
- **named-let** (for tail-recursive local let loops)

### Directory and Files Structure
- `dual-numbers.asd`: The ASDF system definition.
- `package.lisp`: Contains the package definitions and shadowing imports.
- `dual-numbers.lisp`: The core implementation of the `dual-number` and `dual-float` structs, math operations, automatic differentiation via `derivative`, and root finding via Newton's method.
- `dual-numbers-tests.lisp`: The test suite. It runs automatically upon loading.
- `dual1.lisp`: An older/experimental partial implementation, not part of the active ASDF system.
- `README.md`: Basic library documentation.

---

## Package Architecture & Shadowing Imports

Because standard operators like `cl:+`, `cl:-`, and `cl:*` are standard functions and cannot be overridden/customized for new classes without re-defining or overriding them globally, this project depends on `generic-arithmetic`.

- **`generic-arithmetic`** redefines standard mathematical functions (like `+`, `sin`, `cos`) to call corresponding multi-method generic functions (`add2`, `cos`, etc.).
- **`DUAL-NUMBERS` Package**:
  - Shadowing-imports all standard math operators and functions from `GENERIC-ARITHMETIC`.
  - Defines methods on these generic functions for the custom `dual-number` struct.
  - Shadowing-imports `LET` and `NAMED-LAMBDA` from `NAMED-LET`.
  - Exports `MAKE-DUAL`, `DERIVATIVE`, `DUAL-NUMBER`, `DUAL?`, `STANDARD-PART`, `INFINITESIMAL-PART`, `DUAL-FLOAT`, and `MAKE-DUAL-FLOAT`.
- **`DUAL-NUMBERS-TESTS` Package**:
  - Shadowing-imports standard math symbols from `GENERIC-ARITHMETIC`.
  - Uses both `COMMON-LISP` and `DUAL-NUMBERS`.

---

## Building and Running Tests

### Important Environment Warning: `.sbclrc` Conflicts

The local machine's `~/.sbclrc` file automatically loads a suite of global libraries (e.g. `linear-fractional-transformation`, `series`, etc.) and sets up shadowing-imports that conflict with `generic-arithmetic` definitions (e.g., re-binding `cos` or `sin` to standard compiled functions rather than generic functions).

To compile or load `dual-numbers` cleanly in the terminal, **always** bypass the user initialization file by specifying `--no-userinit` and load Quicklisp manually.

### Cleansing the ASDF/FASL Cache
If you encounter package conflicts or stale cache errors (e.g., `SERIES package did not get defined` or `GENERIC-ARITHMETIC:COS already names an ordinary function`), clear the FASL cache directory:

```powershell
sbcl --noinform --no-userinit --non-interactive --load "C:/Users/bitdi/quicklisp/setup.lisp" --eval "(push *default-pathname-defaults* asdf:*central-registry*)" --eval "(uiop:delete-directory-tree (asdf:apply-output-translations *default-pathname-defaults*) :validate t :if-does-not-exist :ignore)"
```

### Running the Test Suite
To load the system and run all tests, run the following command from the project root:

```powershell
sbcl --noinform --no-userinit --non-interactive --load "C:/Users/bitdi/quicklisp/setup.lisp" --eval "(push *default-pathname-defaults* asdf:*central-registry*)" --eval "(ql:quickload :dual-numbers)" --load dual-numbers-tests.lisp
```

A successful run will print:
```
To load "dual-numbers":
  Load 1 ASDF system:
    dual-numbers
; Loading "dual-numbers"
...All tests passed!

All tests passed!
```

---

## Development Conventions

### Adding New Math Functions
To support extra functions, ensure they are first shadowing-imported from `GENERIC-ARITHMETIC` in `package.lisp`, and then define corresponding methods in `dual-numbers.lisp` on the generic function.

Example:
```lisp
(defmethod tan ((obj dual-number))
  (let ((c (cos (standard-part obj))))
    (make-dual (tan (standard-part obj))
               (/ (infinitesimal-part obj) (* c c)))))
```

### Automatic Differentiation
The function `(derivative function)` is curried and can be used on any function composed of the overloaded mathematical operators:
```lisp
(funcall (derivative (lambda (x) (* x x))) 3)
;; => 6
```

### Using `dual-float` for High Performance

`dual-float` is a specialized subtype of `dual-number` designed for maximum performance in single-float numerical calculations. 

It is defined with explicitly typed slots:
```lisp
(defstruct (dual-float
            (:include dual-number
                      (standard-part 0.0 :type single-float)
                      (infinitesimal-part 0.0 :type single-float))
            (:constructor %make-dual-float (standard-part infinitesimal-part))))
```

Because of slot type declarations, SBCL can compile math operations on `dual-float` with no boxing or unboxing of floating-point numbers.

To create `dual-float` instances, use the fast `make-dual-float` constructor:
```lisp
(make-dual-float 2.0f0 3.0f0)
;; => #<DUAL-NUMBER 2.0 + 3.0ε>
```

### Testing Guidelines
When introducing a new math function or modifying root-solving / automatic-differentiation routines:
1. Always add corresponding assertions inside the relevant test function in `dual-numbers-tests.lisp` (e.g., `test-transcendental`, `test-arithmetic`).
2. Run the tests using the clean SBCL invocation mentioned in the "Running the Test Suite" section.
3. Verify that the output shows `All tests passed!`.
