#!/usr/bin/env python
"""
Sorts languages in book.json alphabetically into book.json_.
Check the file manually, then rename.
You need to pass the path to the file as an argument.
From root: $ tools/cleanup/clean_book_json.py book.json
"""

import json
import sys

path = sys.argv[1]

with open(path) as f:
    text = f.read()

book = json.loads(text)
book["pluginsConfig"]["api-language-selector"]["languages"]\
                         .sort(key=lambda x: x[u'name'].lower())

with open(path + "_", "w") as f:
    f.write(json.dumps(book, indent=2))
