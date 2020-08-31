
case class Complex(val real: Double, val imag: Double) {
  def +(other: Complex): Complex = new Complex(this.real + other.real, this.imag + other.imag)
  def -(other: Complex): Complex = new Complex(this.real - other.real, this.imag - other.imag)
  
  def *(other: Complex): Complex = new Complex(
    this.real * other.real - this.imag * other.imag,
    this.real * other.imag + this.imag * other.real)
  
  def *(other: Double): Complex = new Complex(this.real * other, this.imag * other)
}

object Complex {
  def fromPolar(mag: Double, phase: Double): Complex = 
    new Complex(mag * Math.cos(phase), mag * Math.sin(phase))
}

object FT {

  /** Calculates a single DFT coefficient. */
  private def coefficient(n: Int, k: Int, ftLength: Int): Complex = 
    Complex.fromPolar(1.0, -2.0 * Math.PI * k * n / ftLength)
    
  /** Calculates one value of the DFT of signal */
  private def dftValue(signal: IndexedSeq[Double], k: Int): Complex = {
    // Multiply the signal with the coefficients vector
    val terms = for (i <- signal.indices) yield coefficient(i, k, signal.length) * signal(i)

    terms reduce { _ + _ }
  }
    
  def dft(signal: IndexedSeq[Double]): IndexedSeq[Complex] = 
    signal.indices map { dftValue(signal, _) }

  /** Combines the transforms of the even and odd indices */
  private def mergeTransforms(evens: IndexedSeq[Complex], odds: IndexedSeq[Complex]): IndexedSeq[Complex] = {
    val oddTerms = for (i <- odds.indices) 
      yield coefficient(1, i, 2 * odds.length) * odds(i)
    
    val pairs = evens.zip(oddTerms)

    (pairs map { case (e, o) => e + o }) ++ (pairs map { case (e, o) => e - o })
  }

  def cooleyTukey(signal: IndexedSeq[Double]): IndexedSeq[Complex] = signal.length match {
    case 2 => mergeTransforms(
      Vector(new Complex(signal(0), 0)),
      Vector(new Complex(signal(1), 0)))
    case _ => {
      // Split signal into even and odd indices and call cooleyTukey recursively on each.
      val evens = cooleyTukey(for (i <- 0 until signal.length by 2) yield signal(i))
      val odds = cooleyTukey(for (i <- 1 until signal.length by 2) yield signal(i))

      mergeTransforms(evens, odds)
    }
  }

  /** Reverses the bits in value. */
  private def reverseBits(value: Int, length: Int): Int = length match {
    case 1 => value
    case _ => {
      // Split bits in the middle.
      val lowerHalf = length / 2

      // The upper half will be longer if the number of bits is odd.
      val upperHalf = length - lowerHalf
      val mask = (1 << lowerHalf) - 1

      // Reverse each half recursively and then swap them.
      (reverseBits(value & mask, lowerHalf) << upperHalf) + 
        reverseBits(value >> lowerHalf, upperHalf)
    }
  }


  private def log2(x: Double): Double = Math.log(x) / Math.log(2.0)

  def bitReverseIndices(signal: IndexedSeq[Double]): IndexedSeq[Double] = {
    // Find the maximum number of bits needed.
    val bitLength = log2(signal.length).ceil.toInt

    for (i <- signal.indices)
      yield signal(reverseBits(i, bitLength))
  }

  private def butterfly(x1: Complex, x2: Complex, coeff: Complex): (Complex, Complex) = 
    (x1 + coeff * x2, x1 - coeff * x2)

  @scala.annotation.tailrec
  private def iterativeCooleyTukeyLoop(signal: IndexedSeq[Complex], dist: Int): IndexedSeq[Complex] = {
    // Distance between subsequent groups of butterflies
    val stride = 2 * dist
    val result = new Array[Complex](signal.length)

    for (groupStart <- 0 until signal.length by stride; i <- 0 until dist) {
      val index = groupStart + i
      val (r1, r2) = butterfly(
        signal(index),
        signal(index + dist),
        coefficient(1, i, stride))
      
      result(index) = r1
      result(index + dist) = r2
    }
    
    if (stride >= signal.length)
      result.toVector
    else
      iterativeCooleyTukeyLoop(result.toVector, dist * 2)
  }

  def iterativeCooleyTukey(signal: IndexedSeq[Double]): IndexedSeq[Complex] =
    iterativeCooleyTukeyLoop(bitReverseIndices(signal) map { new Complex(_, 0.0) }, 1)
}

object Main {

  private def approxEqual(a: IndexedSeq[Complex], b: IndexedSeq[Complex]): Boolean = {
    val diffs = a.zip(b) map { case (x, y) => Math.abs(x.real - y.real + x.imag - y.imag) }
    diffs map { _ < 1e-12 } reduce { _ && _ }
  }

  def main(args: Array[String]): Unit = {
    val signal = for (i <- 0 until 16) yield Math.random() * 2 - 1
    val x = FT.dft(signal)
    val y = FT.cooleyTukey(signal)
    val z = FT.iterativeCooleyTukey(signal)

    println("DFT and Cooley-Tukey approx. equal: " + approxEqual(x, y))
    println("DFT and iterative Cooley-Tukey approx. equal: " + approxEqual(x, z))
    println("Cooley-Tukey and iterative Cooley-Tukey approx. equal: " + approxEqual(y, z))
  }
}
