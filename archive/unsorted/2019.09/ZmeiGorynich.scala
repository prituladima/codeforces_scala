object ZmeiGorynich {
  def main(args: Array[String]): Unit = {
    val input = new Scanner(System.in);
    val t = input.nextInt()
    for (_ <- 0 until t) {
      val n = input.nextInt()
      val x = input.nextInt()
      val d = new Array[Int](n)
      val h = new Array[Int](n)

      for (i <- 0 until n) {
        d(i) = input.nextInt()
        h(i) = input.nextInt()
      }
      var max = Int.MinValue;
      for (i <- 0 until n) {
        max = Math.max(max, d(i))
      }
      var max2 = Int.MinValue;
      for (i <- 0 until n) {
        max2 = Math.max(max2, d(i) - h(i))
      }
      var ans = 0
      if(max >= x){
        ans = 1
      }else /* max < x */{
        if(max2 <= 0){
          ans = -1;
        }else{
          val rem = x - max;
          ans = 1
          ans += (rem + max2 -1)/max2
        }
      }

      System.out.println(ans)

    }
  }
  import java.io._
  import java.nio.file.{Files, Path}
  import java.util.StringTokenizer

  import scala.io.Codec
  class Scanner(reader: LineNumberReader) extends Iterator[String] with AutoCloseable {
    def this(reader: BufferedReader) = this(new LineNumberReader(reader))

    def this(reader: Reader) = this(new BufferedReader(reader))

    def this(inputStream: InputStream)(implicit codec: Codec) = this(new InputStreamReader(inputStream, codec.charSet))

    def this(path: Path)(implicit codec: Codec) = this(Files.newBufferedReader(path, codec.charSet))

    def this(file: File)(implicit codec: Codec) = this(file.toPath)(codec)

    def this(str: String) = this(new StringReader(str))

    private[this] val tokenizers = Iterator.continually(reader.readLine()).takeWhile(_ != null).map(new StringTokenizer(_)).filter(_.hasMoreTokens)
    private[this] var current: Option[StringTokenizer] = None

    @inline private[this] def tokenizer(): Option[StringTokenizer] = current.find(_.hasMoreTokens) orElse {
      current = if (tokenizers.hasNext) Some(tokenizers.next()) else None
      current
    }

    /**
     * Unlike Java's scanner which returns till end of current line, this actually returns the next line
     *
     * @see line() if you want the Java behaviour
     */
    def nextLine(): String = {
      current = None // reset
      reader.readLine()
    }

    def lineNumber: Int = reader.getLineNumber

    def line(): String = tokenizer().get.nextToken("\n\r")

    def nextString(): String = next()

    def nextChar(): Char = next().ensuring(_.length == 1).head

    def nextBoolean(): Boolean = next().toBoolean

    def nextByte(radix: Int = 10): Byte = java.lang.Byte.parseByte(next(), radix)

    def nextShort(radix: Int = 10): Short = java.lang.Short.parseShort(next(), radix)

    def nextInt(radix: Int = 10): Int = java.lang.Integer.parseInt(next(), radix)

    def nextLong(radix: Int = 10): Long = java.lang.Long.parseLong(next(), radix)

    def nextBigInt(radix: Int = 10): BigInt = BigInt(next(), radix)

    def nextFloat(): Float = next().toFloat

    def nextDouble(): Double = next().toDouble

    def nextBigDecimal(): BigDecimal = BigDecimal(next())

    override def next() = tokenizer().get.nextToken()

    override def hasNext = tokenizer().nonEmpty

    override def close() = reader.close()
  }

}