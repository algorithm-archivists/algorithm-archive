from SCons.Builder import Builder
import SCons.Util

class ToolCocoWarning(SCons.Warnings.SConsWarning):
    pass

class CoconutNotFound(ToolCocoWarning):
    pass

SCons.Warnings.enableWarningClass(ToolCocoWarning)

def _detect(env):
    try:
        return env['coconut']
    except KeyError:
        pass

    coconut = env.WhereIs('coconut')
    if coconut:
        return coconut

    SCons.Warnings.warn(CoconutNotFound, 'Could not find Coconut executable')


def generate(env):
    env['COCONUT'] = _detect(env)
    env['COCONUTFLAGS'] = []

    coconut_compiler = Builder(
        action='"$COCONUT" $COCONUTFLAGS $SOURCE $TARGET',
        src_suffix='.coco',
        suffix='.py',
    )

    env.Append(BUILDERS={'Coconut': coconut_compiler})

def exists(env):
    return env.Detect('coconut')


