from pathlib import Path

Import('*')

for p in Path('contents').iterdir():
    if (q := (p / 'code')).exists():
        for path in q.iterdir():
            if path.stem in languages:
                env.SConscript(path / 'SConscript', exports='env',
                               must_exist=0)
