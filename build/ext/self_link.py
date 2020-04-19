from .extension import Extension


class SelfLink(Extension):
    def run(self, code, path):
        return self_link(code)


def self_link(code):
    result = code.split("<a href=\"")[0]
    for a in code.split("<a href=\"")[1:]:
        lnk = a.split('"')[0]
        rest = '"'.join(a.split('"')[1:])
        if lnk.startswith('../') and lnk.endswith('.md'):
            result += "<a href=\""
            result += lnk.replace('.md', '.html') + '">'
            result += rest[1:]
        else:
            result += "<a href=\"" + a
    return result
