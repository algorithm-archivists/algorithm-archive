from SCons.Builder import Builder

def exists(env):
    env.Detect('go')

def generate(env):
    env['GO'] = 'go'
    env['GOFLAGS'] = []

    go_builder = Builder(
        action='go build -o $TARGET$PROGSUFFIX $SOURCE',
        src_suffix='.go',
        suffix='$PROGSUFFIX',
    )

    env.Append(BUILDERS={'Go': go_builder})
