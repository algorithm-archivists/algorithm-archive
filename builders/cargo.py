from SCons.Builder import Builder
from SCons.Script import Move

def exists(env):
    return env.Detect('cargo')


def generate(env):
    env['CARGO'] = 'cargo'
    env['CARGOFLAGS'] = []
    env['MANIFEST'] = []

    rust_cargo_builder = Builder(
            action=['$CARGO build $CARGOFLAGS --bins --manifest-path $MANIFEST',
                    Move('$TARGET$PROGSUFFIX', 
                         '$SOURCE_DIR/target/debug/main$PROGSUFFIX')
                    ],
            suffix='$PROGSUFFIX',
            )
    env.Append(BUILDERS={'cargo': rust_cargo_builder})
