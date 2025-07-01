# Dual Numbers for Common Lisp

This is a Common Lisp library for working with dual numbers. Dual numbers are a simple extension to the real numbers, of the form `a + bε`, where `a` and `b` are real numbers and `ε` is a nilpotent element such that `ε² = 0`.

This library provides a `dual-number` class and implements various arithmetic and transcendental functions for it. The primary application of this library is automatic differentiation.

## Features

*   Basic arithmetic operations: `+`, `-`, `*`, `/`
*   Trigonometric functions: `sin`, `cos`, `tan`
*   Hyperbolic functions: `sinh`, `cosh`, `tanh`
*   Transcendental functions: `exp`, `log`, `sqrt`
*   Automatic differentiation via the `derivative` function.

## Dependencies

This library depends on:
*   `alexandria`
*   `generic-arithmetic`

These dependencies are managed by ASDF.

## Installation

Clone the repository into a directory known to ASDF (e.g., `~/common-lisp/`). You can then load the system using Quicklisp:

```lisp
(ql:quickload :dual-numbers)
```

## Usage

### Creating Dual Numbers

You can create a dual number using the `make-dual` function:

```lisp
(use-package :dual-numbers)

(make-dual 2 3)
;; => #<DUAL-NUMBER 2 + 3ε>

;; If the infinitesimal part is zero, a regular number is returned
(make-dual 5 0)
;; => 5
```

### Arithmetic

The library uses `generic-arithmetic` to overload the standard arithmetic functions.

```lisp
(let ((d1 (make-dual 2 1))
      (d2 (make-dual 3 4)))
  (+ d1 d2))
;; => #<DUAL-NUMBER 5 + 5ε>

(* d1 d2)
;; => #<DUAL-NUMBER 6 + 11ε>
```

### Automatic Differentiation

The core feature of this library is automatic differentiation. The `derivative` function is a curried function that takes a function and returns a new function that computes the derivative.

```lisp
(use-package :dual-numbers)

;; The derivative of x^2 at x=3 is 2*3=6
(funcall (derivative (lambda (x) (* x x))) 3)
;; => 6

;; The derivative of sin(x) at x=pi is cos(pi)=-1
(funcall (derivative #'sin) pi)
;; => -1.0

;; The derivative of exp(x) at x=0 is exp(0)=1
(funcall (derivative #'exp) 0)
;; => 1.0
```

## API Reference

*   `make-dual standard infinitesimal`
    Creates a dual number.

*   `standard-part number`
    Returns the standard (real) part of a dual number.

*   `infinitesimal-part number`
    Returns the infinitesimal (dual) part of a dual number.

*   `derivative function`
    Returns a new function that computes the derivative of `function`.

## Running Tests

The tests can be run by loading the `dual-numbers-tests.lisp` file in a Common Lisp implementation.

```lisp
(load "dual-numbers-tests.lisp")
```
This will run the test suite and print "All tests passed!" if successful.

## License

This library is released under the MIT License.
