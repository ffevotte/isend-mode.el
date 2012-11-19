# isend-mode.el

`isend-mode.el` is an Emacs extension allowing interaction with code interpreters in `ansi-term` or
`term` buffers.


## Installation

Just clone the repository and add the following lines in your Emacs initialization file (`.emacs` or
`.emacs.d/init.el`):

```lisp
(add-to-list 'load-path "/path/to/isend-mode.el")
(require 'isend-mode)
```


## Basic usage

1. Open an `ansi-term` buffer where you interpeter will live. For example:
   `M-x ansi-term RET /usr/bin/python RET`

2. Open a buffer with the code you want to execute, and associate it to the interpreter buffer using
   the `isend-associate` command. For example:
   `M-x isend-associate RET *ansi-term* RET`
   
3. Hitting `C-RET` will send the current line to the interpreter.


## Use cases

- Interactive demo of a terminal-based program: you prepare all the commands you want to run in a buffer
  and interactively send them to the interpreter as if you had typed them.
  
- Running interpreted code step by step. This is for example useful if you often run the same list
  of shell commands but don't want to formally handle all possible errors in a shell script.


## Contributing

If you make improvements to this code or have suggestions, please do not hesitate to fork the
repository or submit bug reports on [github](https://github.com/ffevotte/isend-mode.el). The repository's
URL is:

    https://github.com/ffevotte/isend-mode.el.git


## License

Copyright (C) 2012 François Févotte.

This program is free software: you can redistribute it and/or modify it under the terms of the GNU
General Public License as published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.  If not,
see <http://www.gnu.org/licenses/>.
