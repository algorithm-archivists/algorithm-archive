import markdown
from .extension import Extension


class HandleLanguages(Extension):
    def run(self, code, path):
        return handle_languages(code)


def handle_section(section: str):
    code = section.split('{% endmethod %}')
    if len(code) == 1:
        return code[0]
    code = code[0]
    result = '<div class=code-section>\n'
    for language in code.split('{% sample lang="')[1:]:
        lang = language.split('"')[0]
        if '/' in lang:
            continue
        text = language.split('%}')[1].split('{% endsample %}')[0]
        result += f'<div class="{lang}">\n'
        result += markdown.markdown(text)
        result += '\n</div>'
    result += '\n</div>'
    return section.replace(code, result).replace("{% endmethod %}", '')


def handle_languages(code: str):
    result = ''
    for section in code.split("{% method %}"):
        result += handle_section(section)
        result += '\n'
    return result
