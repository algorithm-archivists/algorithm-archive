from pathlib import Path

Import('*')

for p in Path('contents').iterdir():
    if (q := (p / 'code')).exists():
        for path in q.iterdir():
            if path.stem in languages:
                env.SConscript(path / 'SConscript', exports='env root_dir',
                           must_exist=0)

