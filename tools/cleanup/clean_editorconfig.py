#!/usr/bin/env python
"""
Sorts languages in .editorconfig alphabetically into .editorconfig_.
Check the file manually, then rename.
You need to pass the path to the file as an argument.
From root: $ tools/cleanup/clean_editorconfig.py .editorconfig
Known examples of bug:
- Language tag with missing space: "#Lua" instead of "# Lua"
"""

import sys

path = sys.argv[1]

with open(path) as f:
    config = f.read()

lang = config.split("\n#")
lang_sorted = lang[0:2] + sorted(lang[2:], key = lambda x : x.lower())
config = "\n#".join(lang_sorted)

with open(path + "_", "w") as f:
    f.write(config)
