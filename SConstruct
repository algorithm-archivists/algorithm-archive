"""
SCons top-level build description (SConstruct) for the Arcane Algorithm Achive

This provides Builder objects for each of the language implementations in the AAA; however, this work cannot be considered exhaustive until every language has been covered.

Currently, the aim is to provide a way to compile or copy the implementation files to the build directory, as well as to provide ways to run them and capture their output.

To run the compilation for all implementations in one language, e.g. C, run the command `scons build/c`, and the resulting executables will be available in the `build/c` directory, each in their respective algorithm directory, containing the executable."""

from pathlib import Path
from collections import namedtuple
import os


rust_cargo_builder = Builder(action=['cargo build --bins --manifest-path $MANIFEST',
                                     Move('$TARGET$PROGSUFFIX', '$SOURCE_DIR/target/debug/main$PROGSUFFIX')])

rust_rustc_builder = Builder(action='rustc $SOURCE -o $TARGET$PROGSUFFIX')

go_builder = Builder(action='go build -o $TARGET$PROGSUFFIX $SOURCE')

env = Environment(ENV=os.environ,
                  BUILDERS={'rustc': rust_rustc_builder,
                            'cargo': rust_cargo_builder,
                            'Go': go_builder},
                  tools=['gcc', 'gnulink', 'g++', 'gas', 'gfortran'])

Export('env')

env['CFLAGS'] = '-Wall -Wextra -Werror'
env['CXXFLAGS'] = '-std=c++17'
env['ASFLAGS'] = '--64'

# Add other languages here when you want to add language targets
# Put 'name_of_language_directory' : 'file_extension'
languages = {
    'c': 'c',
    'cpp': 'cpp',
    'asm-x64': 's',
    'rust': 'rs',
    'go': 'go',
    'fortran': 'f90',
}

# Do not add new Builders here, add them to the BUILDERS argument in the call to Environment above
env.C = env.Program
env.CPlusPlus = env.Program
env.X64 = env.Program
env.Fortran = env.Program

for language in languages:
    Alias(language, f'#/build/{language}')

sconscripts = []
files_to_compile = {language: [] for language in languages}

FileInformation = namedtuple('FileInformation', ['path', 'chapter', 'language'])

for chapter_dir in Path.cwd().joinpath('contents').iterdir():
    if (code_dir := (chapter_dir / 'code')).exists():
        for language_dir in code_dir.iterdir():
            if (language := language_dir.stem) in languages:
                # Check for overriding sconscript
                if (sconscript_path := language_dir / 'SConscript').exists():
                    sconscripts.append(sconscript_path)
                    SConscript(sconscript_path, exports='env')
                    continue
                
                new_files = [FileInformation(path=file_path,
                                             chapter=chapter_dir.name,
                                             language=language)
                                             for file_path in language_dir.glob(f'**/*.{languages[language]}')
                            ]
                files_to_compile[language].extend(new_files)

sconscript_dir_path = Path.cwd().joinpath('sconscripts')
for language, files in files_to_compile.items():
    if files:
        if (sconscript_path := sconscript_dir_path / f"{language}_SConscript").exists():
            SConscript(sconscript_path, exports = {'files_to_compile': files})
        else:
            print(f'{language} file found at {files[0]}, but no sconscript file is present ')

