#!/usr/bin/env python
"""
Sorts languages in book.json alphabetically into book.json_.
Check the file manually, then rename.
"""
import json

with open("../../book.json") as f:
    text = f.read()

book = json.loads(text)
lang = book["pluginsConfig"]["api-language-selector"]["languages"]
book["pluginsConfig"]["api-language-selector"]["languages"] \
  = sorted(lang, key=lambda x: x[u'name'])

with open("../../book.json_", "w") as f:
    f.write(json.dumps(book, indent=2))
