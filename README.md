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
to present its alerts. This defaults to using the Emacs `message`
function for displaying notifications, to change it to something
prettier set the `alert-default-style` variable to one of the options
listed [here](https://github.com/jwiegley/alert#builtin-alert-styles).

To get system notifications like the screenshot use the following:
```elisp
(setq alert-default-style 'libnotify)
```

You can even define your own styles!

### Alert intervals

`org-alert-interval` determines how often org-alert checks your agenda file, and
`org-alert-notify-cutoff` controls how long before a scheduled event a
notification should be sent. `org-alert-notify-after-event-cutoff` controls how
long after a scheduled event to continue sending notifications.

The snippet:

```elisp
(setq org-alert-interval 300
      org-alert-notify-cutoff 10
      org-alert-notify-after-event-cutoff 10)
```

will set org-alert to check your agenda file every 5 minutes (300 seconds),
start notifying you of a scheduled event 10 minutes before the event, and stop
notifying you of the event 10 minutes after the scheduled time has passed.

### Custom titles

org-alert uses the title `*org*` by default. You can set this to
something else by changing the `org-alert-notification-title`
variable. Use this if you'd like to customize the display of org
notifications when using a daemon such as
[dunst](https://github.com/knopwob/dunst).

### Custom regexp for searching agenda entries

org-alert searches for agenda entries with 'Scheduled' or 'Deadline' properties
by default. You can set any other regexp you wish using
the `org-alert-match-string` variable, which defaults to 

```
"SCHEDULED>=\"<today>\"+SCHEDULED<\"<tomorrow>\"|DEADLINE>=\"<today>\"+DEADLINE<\"<tomorrow>\""
```

For example, if you have nested SCHEDULED items like:

```text
* task 1
SCHEDULED: <2022-12-08 Thu 07:00>
Do something
** task 1a
SCHEDULED: <2022-12-08 Thu 15:30>
```

you may want to use a [non-greedy regular expression](https://github.com/spegoraro/org-alert/issues/29#issue-1485013029) as suggested by [hai5](https://github.com/hai5) to capture the first one rather than the second:

```elisp
(setq org-alert-match-string
      "\\(?:SCHEDULED\\|DEADLINE\\):.*?<.*?\\([0-9]\\{2\\}:[0-9]\\{2\\}\\).*>")
````

## TODOs

* Notification priorities based on headline type (scheduled vs deadline)
* Set notify-cutoff for individual events using org properties


