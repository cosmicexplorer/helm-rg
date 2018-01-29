rg3
===

*don't worry about the name too much yet, ok?*

A quick and dirty interactive search tool using [ripgrep](https://github.com/BurntSushi/ripgrep) as a backend. There may be other differentiators too *(???)*.

# feature list

- [ ] search literal string from current directory
    - [ ] display highlighted `rg` output
        - can we use rg's highlighting?
        - could we/would we want to add any additional hl?
        - jump to file (with toggleable customize var like helm ag)
            - results are grouped by file
- [ ] search pcre string and toggle between that and literal searches
- [ ] hook into functionality e.g. `rg`'s `-g` argument
