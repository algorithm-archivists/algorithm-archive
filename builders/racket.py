from SCons.Builder import Builder
import SCons.Util

class ToolRacketWarning(SCons.Warnings.SConsWarning):
    pass

class RacketNotFound(ToolRacketWarning):
    pass

SCons.Warnings.enableWarningClass(ToolRacketWarning)

def _detect(env):
    try:
        return env['raco']
    except KeyError:
        pass

    go = env.WhereIs('raco')
    if go:
        return go

    SCons.Warnings.warn(RacketNotFound, 'Could not find raco executable')

def exists(env):
    env.Detect('raco')

def generate(env):
    env['RACO'] = _detect(env)
    env['RACOFLAGS'] = []

    racket_builder = Builder(
        action='"$RACO" exe -o $TARGET $RACOFLAGS $SOURCE',
        src_suffix='.rkt',
        suffix='$PROGSUFFIX',
    )

    env.Append(BUILDERS={'Racket': racket_builder})
