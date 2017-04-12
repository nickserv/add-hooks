# `add-hooks`
[![MELPA](https://melpa.org/packages/add-hooks-badge.svg)](https://melpa.org/#/add-hooks)
[![MELPA Stable](https://stable.melpa.org/packages/add-hooks-badge.svg)](https://stable.melpa.org/#/add-hooks)

Emacs functions for setting multiple hooks.

Typically, you would need to call `add-hook` multiple times with
similar arguments to declare multiple functions for one hook, or
vice versa.  `add-hooks-pair` is a variant that takes multiple
hooks or functions that apply to each other.  The `add-hooks`
function tidies up duplicate hook and function names further into a
single declarative call (inspired by the
[`bind-key`](https://github.com/jwiegley/use-package/blob/master/bind-key.el)
 package).
