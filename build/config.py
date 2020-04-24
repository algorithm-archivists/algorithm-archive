""" Default configuration file for aaa-py. """

O_NAME = "website"
CONTENTS_NAME = "contents"
INDEX_NAME = "contents/README.md"
SUMMARY_NAME = "SUMMARY.md"
AAA_PATH = "contents"
CONTENTS_PATH = "contents"
AAA_README = "README.md"
AAA_SUMMARY = "SUMMARY.md"
EXT = [
    "fenced_code",
    "codehilite",
    "tables",
    "ext.mdx_links"
]
TEMPLATE_PATH = "build/templates/index.html"
PYGMENT_THEME = "friendly"
SUMMARY_INDENT_LEVEL = 4
STYLE_PATH = "build/styles"
FAVICON_PATH = "build/favicon.ico"
EXTENSIONS = [
    ("handle_languages", "HandleLanguages"),
    ("mdify", "MDfier"),
    ("mathjaxify", "MathJax"),
    ("creative", "Creativize"),
    ("bibtexivize", "Bibtex"),
    ("importize", "Importize"),
    ("self_link", "SelfLink")
]
