import os
import shutil
import re
import jinja2
import markdown
import pybtex.database
import json
from config import *
from ext import get_ext
from .pull import pull
import pygit2

def build():
    md = markdown.Markdown(extensions=EXT)

    os.chdir("../")

    try:
        print("Trying to create website directory...")
        os.mkdir(O_NAME)
        print("Successfully created.")
    except FileExistsError:
        print("Website already exists. Removing...")
        shutil.rmtree(O_NAME)
        print("Making website...")
        os.mkdir(O_NAME)

    print("Making contents folder...")
    os.mkdir(f"{O_NAME}/{CONTENTS_NAME}")

    print("Done making, looking for chapters...")
    chapters = filter(lambda a: re.match('^[a-zA-Z0-9_-]+$', a),
                      os.listdir(os.path.join(AAA_PATH, CONTENTS_NAME)))

    print("Looking for the template...")
    with open(TEMPLATE_PATH, 'r') as template_file:
        print("Reading...")
        template = jinja2.Template(template_file.read())
        print("Template ready!")

    print("Building Pygments...")
    os.system(f"pygmentize -S {PYGMENT_THEME} -f html -a .codehilite > {O_NAME}/pygments.css")

    print("Parsing SUMMARY.md...")
    with open(os.path.join(AAA_PATH, SUMMARY_NAME)) as s:
        summary = parse_summary(s.read())

    print("Opening bibtex...")
    bib_database = pybtex.database.parse_file(f"{AAA_PATH}/literature.bib")

    print("Opening book.json...")
    with open(os.path.join(CONTENTS_NAME, "book.json")) as bjs:
        book_json = json.load(bjs)

    print("Creating rendering pipeline...")
    renderer = get_ext(bib_database, PYGMENT_THEME, md)

    print("Rendering chapters...")
    for chapter in chapters:
        render_chapter(chapter, renderer, template, summary, book_json)

    print("Moving favicon.ico...")
    shutil.copy(FAVICON_PATH, f"{O_NAME}/favicon.ico")

    print("Moving styles...")
    shutil.copytree(STYLE_PATH, f"{O_NAME}/styles")

    print("Parsing redirects...")
    with open(f"{AAA_PATH}/redirects.json") as rjs_file:
        rjs = json.load(rjs_file)
    rjs = {i["from"]: i["to"] for i in rjs["redirects"]}
    with open(f"{O_NAME}/redirects.json", 'w') as rjs_file:
        json.dump(rjs, rjs_file)

    print("Rendering index...")
    with open(INDEX_NAME, 'r') as readme, open(f"{O_NAME}/index.html", 'w') as index:
        index.write(render_one(readme.read(),
                               f"{O_NAME}/",
                               0,
                               renderer,
                               template,
                               summary,
                               book_json))
    os.chdir("build")
    print("Done!")

def parse_summary(summary):
    summary = summary.replace(".md", ".html") \
        .replace("(contents", "(/contents") \
        .replace('* ', '') \
        .replace('README', '/index')
    summary_parsed = []
    for index, line in enumerate(summary.split('\n')[2:-1]):
        indent, rest = line.split('[')
        name, link = rest.split('](')
        link = link[:-1]
        current_indent = len(indent) // SUMMARY_INDENT_LEVEL
        summary_parsed.append((name, link, current_indent))
    return summary_parsed


def render_chapter(chapter, renderer, template, summary, book_json):
    os.mkdir(f"{O_NAME}/{CONTENTS_NAME}/{chapter}")

    try:
        # dirty hack but it works
        shutil.copyfile(f"{AAA_PATH}/{CONTENTS_PATH}/{chapter}/CC-BY-SA_icon.svg",
                        f"{O_NAME}/{CONTENTS_NAME}/{chapter}/CC-BY-SA_icon.svg")
    except FileNotFoundError:
        pass
    try:
        shutil.copytree(f"{AAA_PATH}/{CONTENTS_PATH}/{chapter}/res",
                        f"{O_NAME}/{CONTENTS_NAME}/{chapter}/res")
    except FileNotFoundError:
        pass
    try:
        shutil.copytree(f"{AAA_PATH}/{CONTENTS_PATH}/{chapter}/code",
                        f"{O_NAME}/{CONTENTS_NAME}/{chapter}/code")
    except FileNotFoundError:
        pass

    try:
        # (CONTENTS_NAME)^2 ?
        md_file: str = next(filter(lambda a: a.endswith(".md"),
                                   os.listdir(f"{CONTENTS_NAME}/{CONTENTS_NAME}/{chapter}")))
    except StopIteration:
        return

    out_file = f"{O_NAME}/{CONTENTS_NAME}/{chapter}/{md_file.replace('.md', '.html')}"

    with open(f"{CONTENTS_NAME}/{CONTENTS_NAME}/{chapter}/{md_file}", 'r') as r:
        try:
            index = [k[0] for k in filter(lambda x: out_file.split('/')[-1] in x[1],
                                          [(i, a[1]) for i, a in enumerate(summary)])][0]
        except IndexError:
            return
        contents: str = render_one(r.read(),
                                   f"{CONTENTS_NAME}/{CONTENTS_NAME}/{chapter}",
                                   index,
                                   renderer,
                                   template,
                                   summary,
                                   book_json)
    with open(out_file, 'w') as f:
        f.write(contents)

def render_one(text, code_dir, index, renderer, template, summary, book_json) -> str:
    finalized = renderer(text, code_dir)
    rendered = template.render(md_text=finalized,
                               summary=summary,
                               index=index,
                               enumerate=enumerate,
                               bjs=json.dumps(book_json))
    return rendered
