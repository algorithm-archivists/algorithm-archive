from pygments import highlight
from pygments.lexers import get_lexer_by_name
from pygments.formatters import HtmlFormatter
from pygments.util import ClassNotFound
import os
import math
from .extension import Extension


class Importize(Extension):
    def run(self, code, path):
        return importize(code, path, self.theme)


formatter = None
cache = {}


def importize(code, path, theme='default'):
    global formatter
    result = code.split("<a href=\"")[0]
    for a in code.split("<a href=\"")[1:]:
        lnk = a.split('"')[0]
        rest = '"'.join(a.split('"')[1:])
        if rest[1:].startswith("import"):
            txt = rest.split("</a>")[0][7:]
            if txt.startswith(':'):
                txt = txt[1:]
                arr = txt.split(' ')[0]
                if len(arr.split('-')) == 1:
                    continue
                start, end = arr.split('-')
                if end[-1] == ',':
                    end = end[:-1]
                start, end = int(start or '-1'), int(end or '999999')
            else:
                start, end = -1, math.inf
            rest = "</a>".join(rest.split("</a>")[1:])
            res = ''
            with open(os.path.join(path, lnk)) as file:
                for i, line in enumerate(file):
                    if i >= start - 1 and i <= end - 1:
                        res += line
            result += '<div class="codehilite">\n'
            try:
                lang = txt.split('lang="')[1].split('"')[0]
            except IndexError:
                lang = 'python'
            if lang in cache:
                lexer = cache[lang]
            else:
                try:
                    lexer = get_lexer_by_name(lang)
                except ClassNotFound:
                    if lang == "c_cpp":
                        lexer = get_lexer_by_name("c")
                    else:
                        lexer = get_lexer_by_name("python")
                cache[lang] = lexer
            if formatter is None:
                formatter = HtmlFormatter(style=theme)
            res = highlight(res, lexer, formatter)
            result += res
            result += '\n</div>'
            result += rest
        else:
            result += "<a href=\"" + a
    return result
