from SCons.Builder import Builder
from SCons.Script import Move
import SCons.Util

class ToolCargoWarning(SCons.Warnings.SConsWarning):
    pass

class CargoNotFound(ToolCargoWarning):
    pass

SCons.Warnings.enableWarningClass(ToolCargoWarning)

def _detect(env):
    try:
        return env['cargo']
    except KeyError:
        pass

    cargo = env.WhereIs('cargo')
    if cargo:
        return cargo

    SCons.Warnings.warn(CargoNotFound, 'Could not detect cargo')

def exists(env):
    return env.Detect('cargo')


def generate(env):
    env['CARGO'] = _detect(env)
    env['CARGOFLAGS'] = []
    env['MANIFEST'] = []

    rust_cargo_builder = Builder(
            action=['"$CARGO" build $CARGOFLAGS --bins --manifest-path $MANIFEST',
                    Move('$TARGET$PROGSUFFIX', 
                         '$SOURCE_DIR/target/debug/main$PROGSUFFIX')
                    ],
            suffix='$PROGSUFFIX',
            )
    env.Append(BUILDERS={'cargo': rust_cargo_builder})
