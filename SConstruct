"""
SCons top-level build description (SConstruct) for the Arcane Algorithm Achive

This provides Builder objects for each of the language implementations in the AAA; however, this work cannot be considered exhaustive until every language has been covered.

Currently, the aim is to provide a way to compile or copy the implementation files to the build directory, as well as to provide ways to run them and capture their output.

To run the compilation for all implementations in one language, e.g. C, run the command `scons build/c`, and the resulting executables will be available in the `build/c` directory, each in their respective algorithm directory, containing the executable."""

from pathlib import Path
import os

env = Environment(ENV={'PATH': os.environ['PATH']})

env['CC'] = 'gcc'
env['AS'] = 'as'
for tool in ['gcc','gnulink']:
   env.Tool(tool)
env['CCFLAGS'] = ''
env['CXXFLAGS'] = '-std=c++17'
env['LINKFLAGS'] = '-no-pie'

# Add other languages here when you want to add language targets
# Put 'name_of_language_directory' : 'file_extension'
languages = {'c': 'c', 'cpp': 'cpp', 'asm-x64': 's'}

env.C = env.Program
env.CPlusPlus = env.Program
env.X64 = env.Program

Export('env')

sconscripts = []
files_to_compile = {language: [] for language in languages}

for chapter_dir in Path.cwd().joinpath('contents').iterdir():
    if (code_dir := (chapter_dir / 'code')).exists():
        for path in code_dir.iterdir():
            if path.stem in languages:
                # Check for overriding sconscript
                if (sconscript_path := path / 'SConscript').exists():
                    sconscripts.append(sconscript_path)
                    SConscript(sconscript_path, exports='env')
                else:
                    files_to_compile[path.stem].extend(path.glob(f'*.{languages[path.stem]}'))

sconscript_dir_path = Path('sconscripts')
for language, files in files_to_compile.items():
    if files:
        if (sconscript_path := sconscript_dir_path / f"{language}_SConscript").exists():
            SConscript(sconscript_path, exports = {'files_to_compile': files,
                                                   'language': language})
        else:
            print(f'{language} file found at {files[0]}, but no sconscript file is present ')

