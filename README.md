# ivy-emoji

Insert emoji in Emacs buffers using the ivy selection framework.

## Dependencies

The package relies on `ivy` and uses `company-emoji` to generate the emoji list.

Emacs has to be able to properly render emojis, so a suitable font is required.
Noto Color Emoji is reccomended.

It may be useful to add the following line to the configuration file to ensure
that Noto Color Emoji is chosen to render emojis.

``` emacs-lisp
    (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend)
```

## Usage

Just run `ivy-emoji`, the minibuffer will populate with emojis (see screenshot),
the one you select will be insert at the point.

![screenshot](ss.png "ivy-emoji")
