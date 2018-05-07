helm-rg
=======

[![MELPA](https://melpa.org/packages/helm-rg-badge.svg)](https://melpa.org/#/helm-rg)

![`helm-rg` example usage](./emacs-helm-rg.png)

Search massive codebases extremely fast, using [`ripgrep`](https://github.com/BurntSushi/ripgrep) and [`helm`](https://github.com/emacs-helm/helm). Inspired by [`helm-ag`](https://github.com/syohex/emacs-helm-ag) and [`f3`](https://github.com/cosmicexplorer/f3).

Also check out [rg.el](https://github.com/dajva/rg.el), which I haven't used much but seems pretty cool.

# Usage

*See the [`ripgrep` whirlwind tour](https://github.com/BurntSushi/ripgrep#whirlwind-tour) for further information on invoking `ripgrep`.*

- Invoke the interactive function `helm-rg` to start a search with `ripgrep` in the current directory.
    - `helm` is used to browse the results and update the output as you type.
    - Each line has the file path, the line number, and the column number of the start of the match, and each part is highlighted differently.
    - Use <kbd>TAB</kbd> to invoke the helm persistent action, which previews the result and highlights the matched text in the preview.
    - Use <kbd>RET</kbd> to visit the file containing the result, move point to the start of the match, and recenter.
        - The result's buffer is displayed with `helm-rg-display-buffer-normal-method` (which defaults to `switch-to-buffer`).
        - Use a prefix argument (<kbd>C-u RET</kbd>) to open the buffer with `helm-rg-display-buffer-alternate-method` (which defaults to `pop-to-buffer`).
- The text entered into the minibuffer is interpreted into a [PCRE](https://pcre.org) regexp to pass to `ripgrep`.
    - `helm-rg`'s pattern syntax is basically PCRE, but single spaces basically act as a more powerful conjunction operator.
        - For example, the pattern `a b` in the minibuffer is transformed into `a.*b|b.*a`.
            - The single space can be used to find lines with any permutation of the regexps on either side of the space.
            - Two spaces in a row will search for a literal single space.
        - `ripgrep`'s `--smart-case` option is used so that case-sensitive search is only on if any of the characters in the pattern are capitalized.
            - For example, `ab` (conceptually) searches `[Aa][bB]`, but `Ab` in the minibuffer will only search for the pattern `Ab` with `ripgrep`, because it has at least one uppercase letter.
- Use <kbd>M-d</kbd> to select a new directory to search from.
- Use <kbd>M-g</kbd> to input a glob pattern to filter files by, e.g. `*.py`.
    - The glob pattern defaults to the value of `helm-rg-default-glob-string`, which is an empty string (matches every file) unless you customize it.
    - Pressing <kbd>M-g</kbd> again shows the same minibuffer prompt for the glob pattern, with the string that was previously input.
- Use <kbd><left></kbd> and <kbd><right></kbd> to go up and down by files in the results.
    - <kbd><up></kbd> and <kbd><down></kbd> simply go up and down by match result, and there may be many matches for your pattern in a single file, even multiple on a single line (which `ripgrep` reports as multiple separate results).
    - The <kbd><left></kbd> and <kbd><right></kbd> keys will move up or down until it lands on a result from a different file than it started on.
        - When moving by file, `helm-rg` will cycle around the results list, but it will print a harmless error message instead of looping infinitely if all results are from the same file.
- Use the interactive autoloaded function `helm-rg-display-help` to see the ripgrep command's usage info.

# TODO

- [ ] make a keybinding to drop into an edit mode and edit file content inline in results like [`helm-ag`](https://github.com/syohex/emacs-helm-ag)
    - [x] needs to dedup results from the same line
        - [x] should also merge the colorations
        - [x] this might be easier without using the `--vimgrep` flag (!!!)
    - [ ] can insert markers on either side of each line to find the text added or removed!!!! (huge)
    - [ ] **CAN CHANGE THE FILENAME BY EDITING THE FILE LINE!!!! (HUGE)**

Example output without `--vimgrep`:

    src/python/pants/reporting/plaintext_reporter.py <- get the file name in a buffer-local
    221:      workunit.start_delta_string, <- highlighted -- add a property to the text saying where to jump to that we check in the (persistent?) action

    src/python/pants/reporting/html_reporter.py
    105:              <span class="timedelta">{workunit.start_delta_string}</span>

    src/python/pants/base/workunit.py
    227:  def start_delta_string(self):
    280:                'outcome', 'start_time_string', 'start_delta_string']:

- [x] color all results in the file in the async action!
    - [x] don't recolor when switching to a different result in the same file!
        - (actually just whenever file path matches a defcustom regexp)
- fix <kbd><left></kbd> -- this doesn't render correctly on github for some reason
- toggle `--smart-case` like we do with glob strings
- allow (elisp)? regex searching of search results, including file names
    - use [`helm-swoop`](https://github.com/ShingoFukuyama/helm-swoop)?
- publish `update-commentary.el` and the associated machinery as an npm package

# License

[GPL 3.0+](./LICENSE)
