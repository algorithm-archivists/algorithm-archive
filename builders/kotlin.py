# MIT License
#
# Copyright The SCons Foundation
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
# KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
# WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

"""SCons.Tool.kotlin
Tool-specific initialization for Kotlin.
"""

import SCons.Action
import SCons.Builder
import SCons.Util


class ToolKotlinWarning(SCons.Warnings.SConsWarning):
    pass


class KotlinNotFound(ToolKotlinWarning):
    pass


SCons.Warnings.enableWarningClass(ToolKotlinWarning)


def _detect(env):
    """ Try to detect the kotlinc binary """
    try:
        return env["kotlinc"]
    except KeyError:
        pass

    kotlin = env.Detect("kotlinc")
    if kotlin:
        return kotlin

    SCons.Warnings.warn(KotlinNotFound, "Could not find kotlinc executable")


#
# Builders
#
kotlinc_builder = SCons.Builder.Builder(
    action=SCons.Action.Action("$KOTLINCCOM", "$KOTLINCCOMSTR"),
    suffix="$KOTLINCLASSSUFFIX",
    src_suffix="$KOTLINSUFFIX",
    single_source=True,
)  # file by file

kotlin_jar_builder = SCons.Builder.Builder(
    action=SCons.Action.Action("$KOTLINJARCOM", "$KOTLINJARCOMSTR"),
    suffix="$KOTLINJARSUFFIX",
    src_suffix="$KOTLINSUFFIX",
    single_source=True,
)  # file by file

kotlin_rtjar_builder = SCons.Builder.Builder(
    action=SCons.Action.Action("$KOTLINRTJARCOM", "$KOTLINRTJARCOMSTR"),
    suffix="$KOTLINJARSUFFIX",
    src_suffix="$KOTLINSUFFIX",
    single_source=True,
)  # file by file


def Kotlin(env, target, source=None, *args, **kw):
    """
    A pseudo-Builder wrapper for the kotlinc executable.
        kotlinc [options] file
    """
    if not SCons.Util.is_List(target):
        target = [target]
    if not source:
        source = target[:]
    if not SCons.Util.is_List(source):
        source = [source]

    result = []
    kotlinc_suffix = env.subst("$KOTLINCLASSSUFFIX")
    kotlinc_extension = env.subst("$KOTLINEXTENSION")
    for t, s in zip(target, source):
        t_ext = t
        if not t.endswith(kotlinc_suffix):
            if not t.endswith(kotlinc_extension):
                t_ext += kotlinc_extension

            t_ext += kotlinc_suffix
        # Ensure that the case of first letter is upper-case
        t_ext = t_ext[:1].upper() + t_ext[1:]
        # Call builder
        kotlin_class = kotlinc_builder.__call__(env, t_ext, s, **kw)
        result.extend(kotlin_class)

    return result


def KotlinJar(env, target, source=None, *args, **kw):
    """
    A pseudo-Builder wrapper for creating JAR files with the kotlinc executable.
        kotlinc [options] file -d target
    """
    if not SCons.Util.is_List(target):
        target = [target]
    if not source:
        source = target[:]
    if not SCons.Util.is_List(source):
        source = [source]

    result = []
    for t, s in zip(target, source):
        # Call builder
        kotlin_jar = kotlin_jar_builder.__call__(env, t, s, **kw)
        result.extend(kotlin_jar)

    return result


def KotlinRuntimeJar(env, target, source=None, *args, **kw):
    """
    A pseudo-Builder wrapper for creating standalone JAR files with the kotlinc executable.
        kotlinc [options] file -d target -include-runtime
    """
    if not SCons.Util.is_List(target):
        target = [target]
    if not source:
        source = target[:]
    if not SCons.Util.is_List(source):
        source = [source]

    result = []
    for t, s in zip(target, source):
        # Call builder
        kotlin_jar = kotlin_rtjar_builder.__call__(env, t, s, **kw)
        result.extend(kotlin_jar)

    return result


def generate(env):
    """Add Builders and construction variables for kotlinc to an Environment."""

    env["KOTLINC"] = _detect(env)

    env.SetDefault(
        KOTLINC="kotlinc",
        KOTLINSUFFIX=".kt",
        KOTLINEXTENSION="Kt",
        KOTLINCLASSSUFFIX=".class",
        KOTLINJARSUFFIX=".jar",
        KOTLINCFLAGS=SCons.Util.CLVar(),
        KOTLINJARFLAGS=SCons.Util.CLVar(),
        KOTLINRTJARFLAGS=SCons.Util.CLVar(["-include-runtime"]),
        KOTLINCCOM="$KOTLINC $KOTLINCFLAGS $SOURCE",
        KOTLINCCOMSTR="",
        KOTLINJARCOM="$KOTLINC $KOTLINJARFLAGS -d $TARGET $SOURCE",
        KOTLINJARCOMSTR="",
        KOTLINRTJARCOM="$KOTLINC $KOTLINRTJARFLAGS -d $TARGET $SOURCE",
        KOTLINRTJARCOMSTR="",
    )

    env.AddMethod(Kotlin, "Kotlin")
    env.AddMethod(KotlinJar, "KotlinJar")
    env.AddMethod(KotlinRuntimeJar, "KotlinRuntimeJar")


def exists(env):
    return _detect(env)
