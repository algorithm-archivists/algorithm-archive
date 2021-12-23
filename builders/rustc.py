from SCons.Builder import Builder
import SCons.Util

class ToolRustcWarning(SCons.Warnings.SConsWarning):
    pass

class RustcNotFound(ToolRustcWarning):
    pass

SCons.Warnings.enableWarningClass(ToolRustcWarning)

def _detect(env):
    try:
        return env['rustc']
    except KeyError:
        pass

    cargo = env.WhereIs('rustc')
    if cargo:
        return cargo

    SCons.Warnings.warn(RustcNotFound, 'Could not detect rustc')


def exists(env):
    return env.Detect('rustc')

def rustc_emitter(target, source, env):
    src_name = str(source[0])
    pdb_name = src_name.replace(source[0].suffix, '.pdb')
    env.SideEffect(pdb_name, target)
    env.Clean(target, pdb_name)
    return (target, source)

def generate(env):
    env['RUSTC'] = _detect(env)
    env['RUSTCFLAGS'] = []

    rust_cargo_builder = Builder(
            action='"$RUSTC" $RUSTCFLAGS -o $TARGET $SOURCE',
            suffix='$PROGSUFFIX',
            src_suffix='.rs',
            emitter=rustc_emitter,
            )
    env.Append(BUILDERS={'rustc': rust_cargo_builder})
