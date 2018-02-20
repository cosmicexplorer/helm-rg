#!/usr/bin/env coffee

fs = require 'fs'
wordwrap = require 'wordwrap'

processReadme = (readme) ->
  processed = readme
    .replace(///^[^=]+=+\n///, '')
    .replace(/<kbd>(.*?)<\/kbd>/g, (all, g1) -> "\x00#{g1}'")
    .replace(///\[!\[([^\]]+)\]\([^\)]+\)\]\(([^\)]+)\)///g,
             (all, g1, g2) -> "#{g1}: #{g2}")
    .replace(///\[([^\]]+)\]\(([^\)]+)\)///g, (all, g1, g2) -> "#{g1} (#{g2})")
    .replace(///`([^`]+)`///g, (all, g1) -> "`#{g1}'")
    .replace(/^ +/mg, (all) -> all.replace(/ /g, '='))
  wordwrap(77, {mode: 'soft'})(processed)
    .replace(///^=+///mg, (all) -> all.replace(///=///g, ' '))
    .replace(///^#+(.*)$///mg, (all, g1) -> ";=\n;;#{g1}:")
    .replace(///^$///mg, ';=')
    .replace(///^([^;])///mg, (all, g1) -> ";; #{g1}")
    .replace(///^;=///mg, '')
    .replace(/\x00/g, '`')

link = "https://github.com/cosmicexplorer/helm-rg"
header = ";; The below is generated from a README at\n;; #{link}.\n"

readme = fs.readFileSync("#{__dirname}/../README.md").toString()
helmRgEl = fs.readFileSync("#{__dirname}/../helm-rg.el").toString()

output = helmRgEl.replace(/(;;; Commentary:)\n(;; End Commentary)/g, (all, g1, g2) ->
    "#{g1}\n\n#{header}#{processReadme(readme)}\n#{g2}")

fs.writeFileSync "#{__dirname}/../helm-rg.el", output
