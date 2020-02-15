# ivy-emoji

Insert emoji in Emacs buffers using the ivy selection framework.

## Installation

To install `ivy-emoji`, clone this repo and add the path to your
`load-path` variable. Using `use-package`, this can be done in the following
way:
``` emacs-lisp
(use-package ivy-emoji
  :load-path  "/path/to/ivy-emoji"
  :bind ("C-c i e" . ivy-emoji) ;; mnemonics i e = insert emoji
)
```

### Dependencies

Emacs has to be able to properly render emojis, so a suitable font is required.
Noto Color Emoji is reccomended for optimal results. It may be useful to add the
following line to the configuration file to ensure that Noto Color Emoji is
chosen to render emojis.

``` emacs-lisp
(set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend)
```

## Usage

Just run `ivy-emoji`, the minibuffer will populate with all the available emoji
(see screenshot), the one you select will be insert at the point.

![screenshot](ss.png "ivy-emoji")
