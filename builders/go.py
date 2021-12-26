from SCons.Builder import Builder
import SCons.Util

class ToolGoWarning(SCons.Warnings.SConsWarning):
    pass

class GoNotFound(ToolGoWarning):
    pass

SCons.Warnings.enableWarningClass(ToolGoWarning)

def _detect(env):
    try:
        return env['go']
    except KeyError:
        pass

    go = env.WhereIs('go')
    if go:
        return go

    SCons.Warnings.warn(GoNotFound, 'Could not find go executable')

def exists(env):
    env.Detect('go')

def generate(env):
    env['GO'] = _detect(env)
    env['GOFLAGS'] = []

    go_builder = Builder(
        action='"$GO" build -o $TARGET $GOFLAGS $SOURCE',
        src_suffix='.go',
        suffix='$PROGSUFFIX',
    )

    env.Append(BUILDERS={'Go': go_builder})
