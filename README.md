# org-alert

Provides notifications for scheduled or deadlined agenda entries.

![Screenshot](/../screenshots/screenshot.png?raw=true "org-alert screenshot")



## Command overview
### `org-alert-check`

> Check for and display agenda entries that are active and due.

org-alert parses your org agenda for the current day looking for any
headlines that are scheduled or contain a deadline that aren't marked
with any of your `DONE` state keywords.


### `org-alert-enable`

> Enable periodic deadline checking.

Sets a timer which periodically calls `org-alert-check`. The
interval can be set by changing the `org-alert-interval` (defaults to
300s).


### `org-alert-disable`

> Disable periodic deadline checking.

Cancels any timers set up with the `org-alert-enable` function.



## Installation
### Via MELPA
Installing via melpa is a simple as:
`package-install` RET `org-alert` RET


### use-package

```elisp
(use-package org-alert
  :ensure t)
```

### Manually

Clone the repo somewhere you will remember and then add it to your
load path.
```elisp
(add-to-list 'load-path "path/to/org-alert")
(require 'org-alert)
```



## Configuration

### Notification styles

org-alert uses the excellent
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

### Alert intervals

`org-alert-interval` determines how often org-alert checks your agenda
file, and `org-alert-notify-cutoff` controls how long before a
scheduled event a notification should be sent.

### Custom titles

org-alert uses the title `*org*` by default. You can set this to
something else by changing the `org-alert-notification-title`
variable. Use this if you'd like to customise the display of org
notifications when using a daemon such as
[dunst](https://github.com/knopwob/dunst).

### Custom regexp for searching agenda entries

org-alert searches for agenda entries with 'Sched' or 'Deadline' word
by default. You can set any other regexp you wish using
the `org-alert-match-string` variable.

## TODOs

* Notification priorities based on headline type (scheduled vs deadline)
* Set notify-cutoff for individual events using org properties


