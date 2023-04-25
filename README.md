# lam: Local Abbrev Manager

## Installation

## Usuage

Just type abbrevations into the `*lam*` buffer for them to become instantly available. Abbrevs are reloaded when you switch window focus - no saving or extra commands required!
```
("eia" "Emacs is Awesome!")
("lam" "Local Abbrev Manager")
```

But, wait. Abbrevs can do a lot more! Did you know that we can call arbitrary elisp from abbrevs?

If there is a third arg, it is wrapped into a lambda and called after expansion.
```
("sayhi" "" (message "Hi"))
```
Will echo "Hi" to the minibuffer.

If you don't remember/can't be bothered to lookup exact elisp functions, you can use the following approach to bind key sequences.

For example in `org-mode`, we can insert timestamps using `C-c .`. But, to add a timestamp for current time with the time portion requires 2 universal prefixes. The key sequence is thus: `C-u C-u C-c .`. In some workflows (not all - this is important, otherwise I would just bind a global abbrev), I insert a lot of timestamps as I take notes.

The "right" way to do this would be:
+ Look up help using `C-h k` to figure out the right function being called: `org-time-stamp`
+ Understand that 2 `C-u`'s translates to an arg of '(16). That took me some time to figure out! See this: http://xahlee.info/emacs/emacs/elisp_universal_argument.html

Finally, we can bind this as:
```
("Now" "" (org-time-stamp '(16)))
```

Instead of all this effort, you can do:
```
("Now" "" (execute-kbd-macro (read-kbd-macro "C-u C-u C-c .")))
```

But, even this is too much, so we provide an easy to use macro:
```
("Now" "" (lam/kbd "C-u C-u C-c ."))
```
