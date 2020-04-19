from .extension import Extension


class MathJax(Extension):
    def run(self, code, path):
        return mathjaxify(code)


def mathjaxify(code):
    splitted = code.split("$$")
    result = ""
    for i, section in enumerate(splitted):
        if i % 2:
            section = section \
                .replace('<em>', '*') \
                .replace('</em>', '*') \
                .replace('&lt;', '<') \
                .replace('&gt;', '>') \
                .replace("&amp;", "&") \
                .replace("\\left{", "\\left[") \
                .replace("\\right.", "\\right]") \
                .replace("\\\n", "\\\\\n")
            result += "<script type='math/tex'>"
            result += section
            result += "</script>"
        else:
            result += section
    return result
