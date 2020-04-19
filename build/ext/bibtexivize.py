import os
from .extension import Extension


class Bibtex(Extension):
    def run(self, code, path):
        return bibtex(code, self.bib, path)[0]


def bibtex(code, bib_database, path, use_path=False):
    if use_path:
        with open(path) as f:
            code = f.read()
    halves = code.split("\" | cite }}")
    if len(halves) < 2:
        return code, []
    references = []
    for index, ref in enumerate(halves[:-1]):
        to_ref = ref.split('"')[1]
        halves[index] = ref.split('{{ ')[0]
        references.append(to_ref)
        halves[index] += f'<a href="#ref-{index + 1}">[{index + 1}]</a>'
    code = "".join(halves)
    formatted = []
    count = 1
    for i in references:
        if "/" in i:
            cd, form = bibtex("", bib_database, os.path.join(path, i), True)
            for index, e in enumerate(form):
                a, b, *c = e.split('"')
                form[index] = a + '"' + "ref-" + str(count + index) + '"' + '"'.join(c)
            formatted += form
            count += len(form)
        else:
            if (i in bib_database.entries):
                entry = bib_database.entries[i]
                author = ""
                publisher = ""
                title = ""
                year = ""
                if 'author' in entry.persons:
                    author = entry.persons['author']
                if 'title' in entry.fields:
                    title = entry.fields['title']
                if 'publisher' in entry.fields:
                    publisher = entry.fields['publisher']
                if 'year' in entry.fields:
                    year = entry.fields['year']
                jumper = f"<a name=\"ref-{count}\" class=bib-link></a>"
                string = f"{author}: {title}, <i>{publisher}</i>, {year}"
                formatted.append(jumper + string)
            else:
                formatted.append("")
            count += 1
    code = ("".join(formatted)).join(code.split('{% references %} {% endreferences %}'))
    return code, formatted
