import markdown

URLIZE_RE = '(%s)' % '|'.join([
    r'<(?:f|ht)tps?://[^>]*>',
    r'\b(?:f|ht)tps?://[^)<>\s]+[^.,)<>\s]',
    r'\bwww\.[^)<>\s]+[^.,)<>\s]',
    r'[^(<\s]+\.(?:com|net|org)\b',
])


class UrlizePattern(markdown.inlinepatterns.Pattern):
    def handle_match(self, m):
        url = m.group(2)

        if url.startswith('<'):
            url = url[1:-1]

        text = url

        if not url.split('://')[0] in ('http', 'https', 'ftp'):
            if '@' in url and not '/' in url:
                url = 'mailto:' + url
            else:
                url = 'http://' + url

        el = markdown.util.etree.Element("a")
        el.set('href', url)
        el.text = markdown.util.AtomicString(text)
        return el


class UrlizeExtension(markdown.Extension):

    def extendMarkdown(self, md, md_globals):
        md.inlinePatterns['autolink'] = UrlizePattern(URLIZE_RE, md)


def makeExtension(*args, **kwargs):
    return UrlizeExtension(*args, **kwargs)
