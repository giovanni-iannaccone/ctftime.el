# 🏴‍☠️ CTFtime for Emacs

A small Emacs plugin for browsing [CTFtime](https://ctftime.org/) events without leaving Emacs. The goal is simple: have a quick overview of upcoming CTFs, search through them, filter the list and open an event directly in the browser when needed.

## ⚔️ Features

- Browse upcoming CTFtime events
- Events are highlighted based on when they start
- Search events by name, location, format or description
- Filter events by format and online availability
- View logo and detailed information about an event
- Open the CTFtime page directly with `RET`
- Cache API results to avoid unnecessary requests
- Export the events in Org mode

## 📜 Requirements
- Emacs 27.1+
- A working internet connection

The plugin only relies on Emacs' built-in libraries.

## 🌱 Installation

Clone the repository or copy `ctftime.el` into a directory in your Emacs `load-path`. For example:

```elisp
(add-to-list 'load-path "~/.config/emacs/lisp")
(require 'ctftime)
```

## 🛠️ Configuration

The plugin can be customized through `M-x customize-group RET ctftime`. The main options are:

```elisp
(setq ctftime-days 30)
(setq ctftime-limit 100)
(setq ctftime-cache-duration 3600)
```

`ctftime-days` controls how far into the future events are retrieved, while `ctftime-cache-duration` controls how long API results are kept in memory.

## 🎈 Usage

Run:

```text
M-x ctftime
```

The main buffer provides a simple `tabulated-list` interface. Filters can be combined, so you can for example search for `pwn` while displaying only online Jeopardy CTFs.

| Key     | Action                          |
| ------- | ------------------------------- |
| `RET`   | Open event on CTFtime           |
| `d`     | Show event details              |
| `g`     | Refresh events                  |
| `/`     | Search events                   |
| `o`     | Toggle online-only filter       |
| `t`     | Filter by format                |
| `f`     | Change time range               |
| `c`     | Clear filters                   |
| `SPC`   | Select an event                 |
| `S-SPC` | Select all visible events       |
| `u`     | Clear event selection           |
| `x`     | Export selected events to Org   |
| `q`     | Quit                            |

## ⚖️ License

This project is licensed under the GPL-3.0 License. See the LICENSE file for details.
