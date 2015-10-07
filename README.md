# org-notify

Provides notifications for active org agenda deadlines via notify-send.

![Screenshot](/../screenshots/screenshot.png?raw=true "org-notify screenshot")



## Command overview
### `org-notify-check`

> Check for and display agenda deadlines that are active and due.

Org-notify parses your org agenda for the current day looking for any
headlines that are scheduled or contain a deadline that aren't marked
with any of your `DONE` state keywords.


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



## Configuration

Org-notify uses the excellent
[alert](https://github.com/jwiegley/alert) package from John Wiegley
to present its alerts. This defaults to using the emacs `message`
function for displaying notifications, to change it to something
prettier set the `alert-default-style` variable to one of the options
listed [here](https://github.com/jwiegley/alert#builtin-alert-styles).

To get system notifications like the screenshot use the following:
```elisp
(setq alert-default-style 'libnotify)
```

You can even define your own styles!


### Custom titles

Org-notify uses the title `*org*` by default. You can set this to
something else by changing the `org-notify-notification-title`
variable. Use this if you'd like to customise the display of org
notifications when using a daemon such as
[dunst](https://github.com/knopwob/dunst).


## TODOs

* Notification priorities based on headline type (scheduled vs deadline)


