from SCons.Builder import Builder

def generate(env):
    env['COCONUT'] = 'coconut'
    env['COCONUTFLAGS'] = []

    coconut_compiler = Builder(
        action='$COCONUT $COCONUTFLAGS $SOURCE $TARGET',
        src_suffix='.coco',
        suffix='.py',
    )

    env.Append(BUILDERS={'Coconut': coconut_compiler})

def exists(env):
    return env.Detect('coconut')


