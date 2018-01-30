rg<sup>3</sup>
==============

RipGrep Goes Great with emacs. Search directories fast, using [`ripgrep`](https://github.com/BurntSushi/ripgrep) and [`helm`](https://github.com/emacs-helm/helm). Inspired by [`helm-ag`](https://github.com/syohex/emacs-helm-ag) and [`f3`](https://github.com/cosmicexplorer/f3).

# Usage

*See the [`ripgrep` whirlwind tour](https://github.com/BurntSushi/ripgrep#whirlwind-tour) for further information on invoking `ripgrep`.*

- Invoke the interactive function `rg3` to start a search with `ripgrep` in the current directory.
    - `helm` is used to browse the results and update the output as you type.
    - Each line has the file path, the line number, and the column number of the start of the match, and each part is highlighted differently.
    - Use <kbd>TAB</kbd> to invoke the helm persistent action, which previews the result and highlights the matched text in the preview.
    - Use <kbd>RET</kbd> to visit the file containing the result, move point to the start of the match, and recenter.
- The text entered into the minibuffer is interpreted as a [PCRE](https://pcre.org) regexp which `ripgrep` uses to search your files.
- Use <kbd>M-d</kbd> to select a new directory to search from.
- Use <kbd>M-g</kbd> to input a glob pattern to filter files by, e.g. `*.py`.
    - The glob pattern defaults to the value of `rg3-default-glob-string`, which is an empty string (matches every file) unless you customize it.
    - Pressing <kbd>M-g</kbd> again shows the same minibuffer prompt for the glob pattern, with the string that was previously input.

# License

[GPL 3.0+](./LICENSE)
