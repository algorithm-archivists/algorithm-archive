"""
SCons top-level build description (SConstruct) for the Arcane Algorithm Achive

This provides Builder objects for each of the language implementations in the AAA; however, this work cannot be considered exhaustive until every language has been covered.

Currently, the aim is to provide a way to compile or copy the implementation files to the build directory, as well as to provide ways to run them and capture their output.

To run the compilation for all implmeentations in one language, e.g. Rust, run the command `scons build/c`, and the resulting executables will be available in the `cuild/c` directory, each in their respective algorithm directory, containing the executable."""

from pathlib import Path
import os

env = Environment(ENV={'PATH': os.environ['PATH']})

env['CC'] = 'gcc'
for tool in ['gcc','gnulink']:
   env.Tool(tool)
env['CCFLAGS'] = ''

# Add other languages here when you want to add language targets
languages = ['c']

env.C = env.Program

SConscript('SConscript', exports='env languages')

