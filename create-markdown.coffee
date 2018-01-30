#!/usr/bin/env coffee

fs = require 'fs'
wordwrap = require 'wordwrap'

processReadme = (readme) ->
  processed = readme
    .replace(///^[^=]+=+\n///, '')
    .replace(/<kbd>(.*?)<\/kbd>/g, (all, g1) -> "'#{g1}'")
    .replace(///\[([^\]]+)\]\([^\)]+\)///g, (all, g1) -> g1)
    .replace(///`([^`]+)`///g, (all, g1) -> "`#{g1}'")
    .replace(/^ +/mg, (all) -> all.replace(/ /g, '='))
  wordwrap(77, {mode: 'soft'})(processed)
    .replace(///^=+///mg, (all) -> all.replace(///=///g, ' '))
    .replace(///^#+(.*)$///mg, (all, g1) -> ";=\n;;#{g1}:")
    .replace(///^$///mg, ';=')
    .replace(///^([^;])///mg, (all, g1) -> ";; #{g1}")
    .replace(///^;=///mg, '')

link = "https://github.com/cosmicexplorer/rg3"
header = ";; The below is generated from a README at\n;; #{link}.\n"

readme = fs.readFileSync("#{__dirname}/README.md").toString()
rg3El = fs.readFileSync("#{__dirname}/rg3.el").toString()

output = rg3El.replace(/(;;; Commentary:)\n(;; End Commentary)/g, (all, g1, g2) ->
    "#{g1}\n\n#{header}#{processReadme(readme)}\n#{g2}")

fs.writeFileSync "#{__dirname}/rg3.el", output
