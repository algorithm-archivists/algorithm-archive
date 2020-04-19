""" Default configuration file for aaa-py. """


O_NAME = "website"
CONTENTS_NAME = "contents"
INDEX_NAME = "contents/README.md"
SUMMARY_NAME = "SUMMARY.md"
AAA_CLONE_PATH = "contents"
AAA_PATH = "contents"
AAA_ORIGIN = "git://github.com/algorithm-archivists/algorithm-archive.git"
CONTENTS_PATH = "contents"
AAA_README = "README.md"
AAA_SUMMARY = "SUMMARY.md"
AAA_REPO_PATH = "algorithm-archive-master"
IMPORT_FILES = {
    "SUMMARY.md": "SUMMARY.md",
    "README.md": "README.md",
    "contents": "contents",
    "literature.bib": "literature.bib",
    "book.json": "book.json",
    "redirects.json": "redirects.json"
}
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
