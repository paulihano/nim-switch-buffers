# nim-switch-buffers
Emacs buffer switching made easy.

Wouldn't it be nice if emacs buffer switching worked like Firefox tab switching?

* Never loose the existing order.
* Control-PgUp/Dn to switch buffers in order without end.
* Shift-Control-PgUp/Dn to shift current buffer in the list.

I didn't want tabs and the mess they make on the console.
I just wanted the functionality.

# Ordered tab switching without the tabs.

* You don't need the entire file to get the buffer switching.
* I uploaded my entire keyboard declartion el that is included by my init.el.
* Blindly using the entire file may send you down a dependency rabbit hole.

If you want the minimal, copy only those sections marked

";; -- used to order buffers --"

* Simplistic code - The code for buffer switching consumes about 200 consecutive lines beginning after the inital comment.
* No dependency rabbit hole - The code for buffer switching should not be dependent upon any functions that do not come with a standard emacs install.

I decided to include the entire file for educational purposes, but in truth, it is much easier for me to upload the entire file and not have extra versions.

# Note:

Give a look-see at nim-buffer-types-wanted. This semi-customizable variable (I am a newbie) determines what buffers are included/excluded from the immediately available buffers.

This code stands alone and does NOT link itself into the inners of anything.
* Buffer switching code cares about no mode.
* Buffer switching code requires no initialization.
* Buffer switching code does not use hooks.
* Buffer switching code does not kill or create any buffer.
* Buffer switching code does not use or affect emacs (buffer-menu).
* Buffer switching code does not change any other existing way to visit buffers.

Please report errors and/or upgrades.
# Thank You
