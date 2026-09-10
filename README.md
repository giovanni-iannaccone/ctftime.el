# 🏴‍☠️ CTFtime for Emacs

A simple Emacs plugin to browse upcoming [CTFtime](https://ctftime.org/) events directly from Emacs.

## 🚀 Features

- Upcoming CTF events
- Colors for today's, tomorrow's and upcoming events
- Search and combinable filters
- Online-only filter
- Filter by CTF format
- Event details
- Open CTFtime pages directly from Emacs
- Local caching and manual refresh

## 📦 Installation

Clone the repository and add `ctftime.el` to your Emacs configuration, for example:

```elisp
(add-to-list 'load-path "~/.config/emacs/lisp")
(require 'ctftime)
```

## 📅 Usage

Run:

```text
M-x ctftime
```

Available commands:

```text
RET   Open event on CTFtime
d     Show event details
g     Refresh
/     Search
o     Toggle online-only
t     Filter by format
f     Change time range
c     Clear filters
q     Quit
```

🏴‍☠️ Happy hacking!
