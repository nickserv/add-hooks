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

## Documentation

### `(add-hooks-pair HOOKS FUNCTIONS)`
Call `add-hook` for each combined pair of items in *HOOKS* and *FUNCTIONS*.

Either value can be a single symbol or a list of symbols, in
which case a function can be added to multiple hooks and/or
multiple functions can be added to a hook.  This behaves like
`add-hook` when both values are atoms.

#### Example
```emacs
(add-hooks-pair '(css-mode-hook sgml-mode-hook) 'emmet-mode)
```

#### Result
```emacs
ELISP> css-mode-hook
(emmet-mode)

ELISP> sgml-mode-hook
(emmet-mode)
```

### `(add-hooks PAIRS)`
Call `add-hooks-pair` on each cons pair in *PAIRS*.

Each pair has a `car` for setting hooks and a `cdr` for setting
functions to add to those hooks.  Either side of the pair can be
a single symbol or a list of symbols, as passed to
`add-hooks-pair`.

#### Usage

```emacs
(add-hooks '((hook-or-hooks . function-or-functions)...))
```

#### Example
```emacs
(add-hooks '(((css-mode-hook sgml-mode-hook) . emmet-mode)))
```

#### Result
```emacs
ELISP> css-mode-hook
(emmet-mode)

ELISP> sgml-mode-hook
(emmet-mode)
```
