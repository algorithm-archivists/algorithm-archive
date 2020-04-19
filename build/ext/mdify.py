from .extension import Extension


class MDfier(Extension):
    def run(self, code, path):
        return self.md.convert(code)
