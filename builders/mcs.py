from SCons.Builder import Builder
import SCons.Util

class ToolMCSWarning(SCons.Warnings.SConsWarning):
    pass

class MCSNotFound(ToolMCSWarning):
    pass

SCons.Warnings.enableWarningClass(ToolMCSWarning)

def _detect(env):
    try:
        return env['mcs']
    except KeyError:
        pass

    mcs = env.WhereIs('mcs')
    if mcs:
        return mcs

    SCons.Warnings.warn(MCSNotFound, 'Could not find mcs executable')

def exists(env):
    env.Detect('mcs')

def generate(env):
    env['MCS'] = _detect(env)
    env['MCSFLAGS'] = []

    mcs_builder = Builder(
        action='"$MCS" -out:$TARGET $MCSFLAGS $SOURCES',
        src_suffix='.cs',
        suffix='$PROGSUFFIX',
    )

    env.Append(BUILDERS={'MCS': mcs_builder})
