# isend-mode [![MELPA](http://melpa.milkbox.net/packages/isend-mode-badge.svg)](http://melpa.milkbox.net/#/isend-mode)

`isend-mode` is an Emacs extension allowing interaction with code interpreters
in `ansi-term`/`term` or `vterm` buffers. Some language-specific modes
(e.g. `python.el`) already provide similar features; `isend-mode` does the same
in a language-agnostic way.

![screencast](https://raw.githubusercontent.com/ffevotte/isend-mode.el/refs/heads/gh-pages/screencast/screencast.svg)

## Installation

### From MELPA

The easiest (and recommended) way to get up and running with `isend-mode` is to
install it through [MELPA](http://melpa.milkbox.net/#/isend-mode). If you're not
already using MELPA,
[it's quite easy to setup.](http://melpa.milkbox.net/#/getting-started)

### From sources

Just clone the repository. For example:

```shell
git clone https://github.com/ffevotte/isend-mode.el.git /path/to/isend-mode
```

Then, add the following lines in your Emacs initialization file (`.emacs` or `.emacs.d/init.el`):

```lisp
(add-to-list 'load-path "/path/to/isend-mode")
(require 'isend-mode)
```


## Usage

### Getting started

The following example demonstrates using `isend-mode` to interact with a shell in an `ansi-term`
buffer. Please note that any other interpreter could have been used (e.g. python, perl or anything
else) and `term` would have worked as well.


1. Open an `ansi-term` buffer where the interpeter will live. For example:

   `M-x ansi-term RET /bin/sh RET`


2. Open a buffer with the code you want to execute, and associate it to the interpreter buffer using
   the `isend-associate` command (or `isend`, which is a shorter alias). For example:

   `M-x isend RET *ansi-term* RET`


3. Hitting <kbd>C-RET</kbd> will send the current line to the interpreter. If a region is active, all lines
   spanned by the region will be sent (i.e. no line will be only partially sent). Point is then
   moved to the next non-empty line (but see configuration variable `isend-skip-empty-lines`).


### Use cases

- **Interactive demo of a text-based program:** you prepare all the commands you want to run in a
  buffer and interactively send them to the interpreter as if you had typed them.

- **Running interpreted code step by step:** this is for example useful if you often run the same
  list of shell commands but don't want to formally handle all possible errors in a script.


### Advanced usage

Apart from `isend-send`, bound by default to <kbd>C-RET</kbd> and described
above, `isend-mode` defines a few other commands that you are free to use
interactively and bind to custom keys:

- `isend-send-buffer`: sends the whole buffer. This is functionnally equivalent
  to calling `mark-whole-buffer` (<kbd>C-x</kbd><kbd>h</kbd>), then `isend-send`.

- `isend-send-defun`: sends the current defun. See the configuration variable
  `isend-mark-defun-function` for how to mark a function definition.

- `isend-display-buffer`: display the buffer associated to the current one.


## Customization

`isend-mode` can be customized with `M-x customize-group RET isend RET`

The variables which can be set to customize `isend`'s behaviour are:

- `isend-forward-line`: if non-nil (default), `isend` advances to the next line after having sent
  some content using <kbd>C-RET</kbd>.

- `isend-skip-empty-lines`: if non-nil (default), `isend` will skip empty lines (i.e. lines
  containing only whitespace) and position point on the first following non-empty line. Some
  interpreters (like Python) care about empty lines. In such cases it might be useful to set
  `isend-skip-empty-lines` to nil.

- `isend-strip-empty-lines`: if non-nil, `isend` will remove empty (or whitespace-only) lines from
  the region before sending it to the interpreter. Note that this only works when sending an entire
  region (as opposed to a single line).

- `isend-delete-indentation`: if non-nil, `isend` will delete indentation from all lines in the
  region. Note that this only works when sending a region (as opposed to a single line). Relative
  indentation w.r.t the first line is preserved. This is useful e.g. to send Python blocks outside
  of their original context.

- `isend-end-with-empty-line`: if non-nil, `isend` appends an empty line to regions sent. Note that
  this only works when sending an entire region (as opposed to a single line).

- `isend-bracketed-paste`: if non-nil, `isend` uses [bracketed
  paste](https://cirw.in/blog/bracketed-paste). In short, this means it surrounds the contents
  it sends with escape sequences indicating the underlying process that this
  content is being pasted. Some interpreters (*e.g.* the Julia REPL) use this in meaningful ways.

- `isend-send-line-function` and `isend-send-region-function`: if set, these are
  the functions called by `isend` to send a line or a region respectively. These
  functions are called in a buffer containing the text to be sent. They can
  modify it as needed before it is sent to the process. These functions also
  receive as argument the destination buffer, in case some interaction with it
  would be useful.

  Possible values include:

  - `nil` (default): do nothing (the contents will be sent as they are)
  - `isend--ipython-paste`: copy the contents to the clipoard, and send `%paste` to the interpreter
        buffer (where an `iPython` process is supposed to be running).
  - `isend--ipython-cpaste`: wrap the contents within a `%cpaste` command (an `iPython` processes
    is supposed to be running in the associated buffer).

- `isend-mark-defun`: a function that will mark the current "defun" to be sent
  by `isend-send-defun`.

  Possible values include:

  - `mark-defun` (default): works for LISP-like languages
  - `isend--python-mark-defun`: marks the current top-level block in a python buffer


### Setup helpers

A few helpers are provided to help setup `isend` when working with multiple languages:

```lisp
;; If you work with shell scripts
(add-hook 'isend-mode-hook 'isend-default-shell-setup)

;; If you work with python scripts, one of those could be used (but not both!)
;;   - default python interpreter
(add-hook 'isend-mode-hook 'isend-default-python-setup)

;;   - specific setup for iPython
(add-hook 'isend-mode-hook 'isend-default-ipython-setup)

;; If you work with julia
(add-hook 'isend-mode-hook 'isend-default-julia-setup)
```


## Contributing

If you make improvements to this code or have suggestions, please do not hesitate to fork the
repository or submit bug reports on [github](https://github.com/ffevotte/isend-mode.el). The repository's
URL is:

    https://github.com/ffevotte/isend-mode.el.git


Many thanks go to the following contributors:
- [Soumya Tripathy](https://github.com/Blade6570): vterm support
- [James Porter](https://github.com/porterjamesj): empty lines handling;
- [@albertstartup](https://github.com/albertstartup): handling newer version of
  iPython.


## License

Copyright (C) 2012-2019 François Févotte.

This program is free software: you can redistribute it and/or modify it under the terms of the GNU
General Public License as published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.  If not,
see <http://www.gnu.org/licenses/>.
