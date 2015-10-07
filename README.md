# org-notify

Provides notifications for active org agenda deadlines via notify-send.

![Screenshot](/../screenshots/screenshot.png?raw=true "org-notify screenshot")



## Command overview
### `org-notify-check`

> Check for and display agenda deadlines that are active and due.

Org-notify parses your org agenda for the current day looking for any
headlines with a deadline that aren't marked with the `DONE` state
(customisable via the `org-notify-done-state` variable). These are then
sent to your notification daemon via notify-send.

In order to keep messages clean, org-notify strips any todo states
from the headline text. These default to `TODO` and `STARTED` so you can
add your own by modifying the `org-notify-todo-states` variable.


### `org-notify-enable`

> Enable periodic deadline checking.

Sets a timer which periodically calls `org-notify-check`. The polling
interval can be set by changing the `org-notify-interval` (defaults to
300s).


### `org-notify-disable`

> Disable periodic deadline checking.

Cancels any timers set up with the `org-notify-enable` function.



## Installation
### Via MELPA
Installing via melpa is a simple as:
`package-install` RET `org-notify` RET


### Manually

Clone the repo somewhere you will remember and then add it to your
load path.
```elisp
(add-to-list 'load-path "path/to/cloned/repo")
(require 'org-notify)
```

