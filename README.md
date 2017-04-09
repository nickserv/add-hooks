# `add-hooks`
Emacs macro for setting multiple hooks.

Typically, you would need to call `add-hook` multiple times with
similar arguments to declare multiple functions for one hook, or
vice versa.  The `add-hooks` function tidies up duplicate hook and
function names into a single declarative call (inspired by the
[`bind-key`](https://github.com/jwiegley/use-package/blob/master/bind-key.el)
package).

## Documentation
`add-hooks` is an autoloaded Lisp function in `add-hooks.el`.

```emacs
(add-hooks PAIRS)
```

Call `add-hook` on each cons pair in `PAIRS`.

Each pair has a `car` for setting hooks and a `cdr` for setting
functions to add to those hooks.  Either side of the pair can be a
single symbol or a list of symbols, in which case a function can be
added to multiple hooks and/or multiple functions can be added to a
hook.

### Usage
```
(add-hooks '((hook-or-hooks . function-or-functions)...))
```

### Example
```
(add-hooks '(((css-mode-hook sgml-mode-hook) . emmet-mode)))
```

### Result
```
ELISP> css-mode-hook
(emmet-mode)

ELISP> sgml-mode-hook
(emmet-mode)
```
