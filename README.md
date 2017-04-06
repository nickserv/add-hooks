# `add-hooks`
Emacs macro for setting multiple hooks without duplication.

Typically, you would need to call `add-hook` multiple times with similar arguments to declare multiple functions for one hook, or vice versa. The `add-hooks` macro tidies up duplicate hook and function names, with syntax inspired by [`bind-key`](https://github.com/jwiegley/use-package/blob/master/bind-key.el).

## Usage
It takes several cons pairs, where either the hook or function can be a single symbol or a list of symbols, in which case the pair will automatically apply to multiple functions and/or hooks.

## Example

### Before
Using the built in `add-hook` function:
```emacs
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
```

### After
Using the `add-hooks` macro:
```emacs
(add-hooks ((css-mode-hook sgml-mode-hook) . emmet-mode))
```
