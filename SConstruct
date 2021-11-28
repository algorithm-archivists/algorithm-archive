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

# For interpreted languages to copy to build directory
copy_builder = Builder(action=Copy('$TARGET', '$SOURCE'))

env = Environment(ENV=os.environ,
                  BUILDERS={'rustc': rust_rustc_builder,
                            'cargo': rust_cargo_builder,
                            'Go': go_builder,
                            'Copier': copy_builder},
                  tools=['gcc', 'gnulink', 'g++', 'gas'])

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
    'python': 'py',
    'julia': 'jl',
    'php': 'php',
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


contents_path = Path.cwd().joinpath('contents')
for chapter_dir in contents_path.iterdir():
    for code_dir in chapter_dir.glob('**/code'):
        # For nested chapters e.g. contents/convolutions/1d/
        extended_chapter_path = code_dir.relative_to(contents_path).parent
        
        for language_dir in code_dir.iterdir():
            if (language := language_dir.stem) in languages:
                new_files = [FileInformation(path=file_path,
                                             chapter=extended_chapter_path,
                                             language=language)
                                             for file_path in language_dir.glob(f'**/*.{languages[language]}')
                            ]
                # Check for overriding SConscript
                if (sconscript_path := language_dir / 'SConscript').exists():
                    SConscript(sconscript_path, exports={'files_to_compile': new_files})
                else:
                    files_to_compile[language].extend(new_files)

sconscript_dir_path = Path.cwd().joinpath('sconscripts')
for language, files in files_to_compile.items():
    if files:
        if (sconscript_path := sconscript_dir_path / f"{language}_SConscript").exists():
            SConscript(sconscript_path, exports = {'files_to_compile': files})
        else:
            print(f'{language} file found at {files[0]}, but no sconscript file is present ')

