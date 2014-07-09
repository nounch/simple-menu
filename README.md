# simple-menu.el

`simple-menu.el`  providesa simple menu consisting of mappings between
single key presses and user-defined functions.

The functions can be either interactive or non-interactive and will be
executed in the context of the invocation buffer whereas a visual list
of key-command bindings is displayed in the `*Simple Menu*` buffer which
appears in another window, if possible.

The list and sort order of commands is customizable via the variable
`simple-menu-commands` like this:
  
        M-x customize-variable RET simple-menu-commands

The list and sort order of commands is customizable via the variable
`simple-menu-trigger-keys` like this:
  
        M-x customize-variable RET simple-menu-trigger-keys

## Usage

1. Require it

        (require 'simple-menu)

2. Invoke the menu:

        M-x simple-menu

3. Hit the number associated with the command that you want to execute as
   displayed in the `*Simple Menu*` buffer

   *or*

   hit `q` to abort the selection and close the menu buffer.
