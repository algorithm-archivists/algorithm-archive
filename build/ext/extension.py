class Extension:
    def __init__(self, bib, theme, md):
        self.bib = bib
        self.theme = theme
        self.md = md

    def run(self, code: str, path: str) -> str:
        pass
