from .extension import Extension


class Creativize(Extension):
    def run(self, code, path):
        return creativize(code)


elem = '<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/"><img alt="Creative Commons License" style="border-width:0; width:88px" src="https://i.creativecommons.org/l/by-nc/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/">Creative Commons Attribution-NonCommercial 4.0 International License</a>.'


def creativize(code: str):
    return code.replace('{% creativecommons type="by-nc" %}\n{% endcreativecommons %}', elem)
