#!/usr/bin/env python
"""
Sorts languages in .editorconfig alphabetically into .editorconfig_.
Check the file manually, then rename.
Known examples of bug:
- Language tag with missing space: "#Lua" instead of "# Lua"
"""

with open("../../.editorconfig") as f:
    config = f.read()

lang = config.split("\n#")
lang_sorted = lang[0:2] + sorted(lang[2:])
config = "\n#".join(lang_sorted)

with open("../../.editorconfig_", "w") as f:
    f.write(config)
