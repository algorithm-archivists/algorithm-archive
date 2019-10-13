from math import PI
from random import rand, randomize
from sequtils import map, toSeq, zip
from sugar import `=>`
import complex
# $ nimble install fftw3
import fftw3

# For some reason this isn't in the Nim fftw3 bindings.
const FFTW_FORWARD = -1

proc allClose[T: SomeNumber | Complex[SomeFloat]](reference: openArray[T], calculated: openArray[T], thresh = 1.0e-13): bool =
  if reference.len != calculated.len:
    return false
  result = true
  for (r, c) in zip(reference, calculated):
    if abs(r - c) > thresh:
      result = false
      break

proc skip[T](inp: openArray[T], stride: int): seq[T] =
  ## Like `inp[::stride]` in Python.
  if stride < 1:
    return toSeq(inp)
  result = @[]
  var i = 0
  while i < inp.len:
    result.add(inp[i])
    i += stride

proc nim_to_fftw(inp: Complex64): fftw_complex =
  [inp.re, inp.im]

proc fftw_to_nim(inp: fftw_complex): Complex64 =
  complex64(inp[0], inp[1])

proc nim_to_fftw(inp: openArray[Complex64]): seq[fftw_complex] =
  inp.map(v => nim_to_fftw(v))

proc fftw_to_nim(inp: openArray[fftw_complex]): seq[Complex64] =
  inp.map(v => fftw_to_nim(v))

proc dft_fftw3(inp: openArray[Complex64]): seq[Complex64] =
  var
    fftw_inp = nim_to_fftw(inp)
    fftw_out = newSeq[fftw_complex](inp.len)
  let p = fftw_plan_dft_1d(cint(inp.len),
                           addr(fftw_inp[low(fftw_inp)]),
                           addr(fftw_out[low(fftw_out)]),
                           FFTW_FORWARD,
                           FFTW_ESTIMATE)
  fftw_execute(p)
  fftw_destroy_plan(p)
  fftw_to_nim(fftw_out)

proc dft[T: SomeFloat](x: openArray[Complex[T]]): seq[Complex[T]] =
  let n = x.len
  result = newSeq[Complex[T]](n)
  for i in 0..n - 1:
    for k in 0..n - 1:
      result[i] += x[k] * exp(-complex(0.0, 2.0) * PI * float(i * k / n))

proc cooley_tukey[T: SomeFloat](x: openArray[Complex[T]]): seq[Complex[T]] =
  let n = x.len
  if n <= 1:
    return toSeq(x)
  result = newSeq[Complex[T]](n)
  let
    even = cooley_tukey(skip(x, 2))
    odd = cooley_tukey(skip(x[1..high(x)], 2))
    midpoint = n div 2
  for k in 0..midpoint - 1:
    let exp_term = exp(-complex(0.0, 2.0) * PI * float(k / n)) * odd[k]
    result[k] = even[k] + exp_term
    result[k + midpoint] = even[k] - exp_term

# proc bit_reverse[T: SomeNumber](x: openArray[T]): seq[T]

# proc iterative_cooley_tukey[T: SomeFloat](x: openArray[T]): seq[Complex[T]] =
#   let n = x.len
#   result = bit_reverse(x)

when isMainModule:
  randomize()
  let
    x = toSeq(1..64).map(i => complex64(rand(1.0), 0.0))
    y = cooley_tukey(x)
    # z = iterative_cooley_tukey(x)
    t = dft(x)
    reference = dft_fftw3(x)
  assert(allClose(reference, y))
  # assert(allClose(reference, z))
  assert(allClose(reference, t))
