package cc.factorie.util

import java.io.{BufferedOutputStream, FileOutputStream, ByteArrayOutputStream, File}
import java.nio.ByteBuffer
import java.nio.file.{Paths, Files}
import java.util.zip.GZIPOutputStream

import cc.factorie.app.nlp.lexicon.{StaticLexicons, LexiconsProvider}
import cc.factorie.app.nlp.ner.ConllChainNer
import cc.factorie.la.DenseTensor1
import cc.factorie.model.WeightsSet
import cc.factorie.variable.CategoricalDomain

import scala.collection.mutable

/** Typeclass for types that can be represented as an array of bytes and can be reconstructed from an array of bytes
  * given their type. For (size) efficiency they store no metadata. The unpack method should only read after the start
  * offset in data and should return the offset at which Elem ends. */
trait BytePackable[Elem] {
  def pack(e:Elem):Array[Byte]
  def unpack(data:Array[Byte], start:Int):(Elem, Int)
}


object TensorByte {
  sealed trait IndexType {
    final val mask:Byte = 0x0C
    def set:Byte
  }
  case object ByteType extends IndexType {val set = 0x00}
  case object ShortType extends IndexType {val set = 0x04}
  case object IntType extends IndexType {val set = 0x08}
  case object LongType extends IndexType {val set = 0x0C}

  sealed trait Order {
    final val mask:Byte = 0x30
    def set:Byte
  }
  case object First extends Order {val set = 0x00}
  case object Second extends Order {val set = 0x10}
  case object Third extends Order {val set = 0x20}
  case object Fourth extends Order {val set = 0x30}
}

class TensorByte(var b:Byte) {
  def this() = this(0)
  import TensorByte._


  private def binBoolSetter(bool:Boolean, coordMask:Byte): Unit = {
    if(bool) {
      b = (b ^ coordMask).toByte
    } else {
      b = (b & (0xFF - coordMask)).toByte
    }
  }

  def storedDense_=(bool:Boolean): Unit = {binBoolSetter(bool, 0x01)}

  def storedDense = (b & 0x01) != 0

  def origDense_=(bool:Boolean): Unit = {binBoolSetter(bool, 0x02)}

  def origDense = (b & 0x02) != 0

  def indexType_=(indexType:IndexType): Unit = {
    b = (b ^ (0xFF - indexType.mask) + indexType.set).toByte
  }

  def indexType:IndexType = b & 0x0C match {
    case 0x00 => ByteType
    case 0x04 => ShortType
    case 0x08 => IntType
    case 0x0C => LongType
  }

  def orderType_=(orderType:Order): Unit = {
    b = (b ^ (0xFF - orderType.mask) + orderType.set).toByte
  }

  def orderType:Order = b & 0x30 match {
    case 0x00 => First
    case 0x10 => Second
    case 0x20 => Third
    case 0x30 => Fourth
  }

  def binaryTensor_=(bool:Boolean) {binBoolSetter(bool, 0x40)}
  def binaryTensor = (b & 0x40) != 0

  // very ugly I know
  def showBits = "%08d".format(Integer.toBinaryString(b).toInt)

  override def equals(o:Any) = o match {
    case ob:AnyRef if this eq ob => true
    case that:TensorByte => this.b == that.b
    case _ => false
  }
}

object BytePackable {

  implicit object StringSer extends BytePackable[String] {
    def pack(e: String) = {
      val stringDat = e.getBytes("UTF-8")
      IntSer.pack(stringDat.length) ++ stringDat
    }
    def unpack(data: Array[Byte], start:Int) = {
      var offset = start
      val tup = IntSer.unpack(data, offset)
      val strLen = tup._1
      offset = tup._2
      new String(data, offset, strLen, "UTF-8") -> (offset + strLen)
    }
  }
  implicit object ShortSer extends BytePackable[Short] {
    def pack(e: Short) = ByteBuffer.wrap(new Array[Byte](2)).putShort(e).array()
    def unpack(data: Array[Byte], start: Int) = ByteBuffer.wrap(data, start, 2).getShort -> (start + 2)
  }
  implicit object IntSer extends BytePackable[Int] {
    def pack(e: Int) = ByteBuffer.wrap(new Array[Byte](4)).putInt(e).array()
    def unpack(data: Array[Byte], start: Int) = ByteBuffer.wrap(data, start, 4).getInt -> (start + 4)
  }
  implicit object FloatSer extends BytePackable[Float] {
    def pack(e: Float) = ByteBuffer.wrap(new Array[Byte](4)).putFloat(e).array()
    def unpack(data: Array[Byte], start: Int) = ByteBuffer.wrap(data, start, 4).getFloat-> (start + 4)
  }
  implicit object LongSer extends BytePackable[Long] {
    def pack(e: Long) = ByteBuffer.wrap(new Array[Byte](8)).putLong(e).array()
    def unpack(data: Array[Byte], start: Int) = ByteBuffer.wrap(data, start, 8).getLong -> (start + 8)
  }
  implicit object DoubleSer extends BytePackable[Double] {
    def pack(e: Double) = ByteBuffer.wrap(new Array[Byte](8)).putDouble(e).array()
    def unpack(data: Array[Byte], start: Int) = ByteBuffer.wrap(data, start, 8).getDouble -> (start + 8)
  }
  implicit object IntsSer extends BytePackable[Array[Int]] {
    def pack(ds: Array[Int]) = IntSer.pack(ds.length) ++ ds.foldLeft(ByteBuffer.wrap(new Array[Byte](4 * ds.length)))(_ putInt _).array()
    def unpack(data: Array[Byte], start: Int) = {
      val (size, o) = IntSer.unpack(data, start)
      var offset = o
      val ds = new Array[Int](size)
      val buf = ByteBuffer.wrap(data, offset, size * 4)
      (0 until size).foreach { idx =>
        ds(idx) = buf.getInt
        offset += 4
      }
      ds -> offset
    }
  }
  implicit object DoublesSer extends BytePackable[Array[Double]] {
    def pack(ds: Array[Double]) = IntSer.pack(ds.length) ++ ds.foldLeft(ByteBuffer.wrap(new Array[Byte](8 * ds.length)))(_ putDouble _).array()
    def unpack(data: Array[Byte], start: Int) = {
      val (size, o) = IntSer.unpack(data, start)
      var offset = o
      val ds = new Array[Double](size)
      val buf = ByteBuffer.wrap(data, offset, size * 8)
      (0 until size).foreach { idx =>
        ds(idx) = buf.getDouble
        offset += 8
      }
      ds -> offset
    }
  }
  

  implicit def seqSer[Elem : BytePackable]:BytePackable[Seq[Elem]] =
    new BytePackable[Seq[Elem]] {
      private val ser = implicitly[BytePackable[Elem]]
      def pack(es: Seq[Elem]) = IntSer.pack(es.size) ++ es.flatMap(ser.pack)

      def unpack(data: Array[Byte], start: Int) = {
        val (size, off) = IntSer.unpack(data, start)
        val es = new mutable.ArrayBuffer[Elem](size)
        val finalOffset = (0 until size).foldLeft(off){ case(offset, _) =>
          val (e, newOffset) = ser.unpack(data, offset)
          es += e
          newOffset
        }
        es.toSeq -> finalOffset
      }
    }

  implicit def mapSer[K : BytePackable, V : BytePackable]:BytePackable[Map[K, V]] =
    new BytePackable[Map[K, V]] {
      private val kSer = implicitly[BytePackable[K]]
      private val vSer = implicitly[BytePackable[V]]
      def pack(em: Map[K, V]) = IntSer.pack(em.size) ++ em.flatMap{case (k,v) => kSer.pack(k) ++ vSer.pack(v)}

      def unpack(data: Array[Byte], start: Int) = {
        val (size, off) = IntSer.unpack(data, start)
        val em = new mutable.HashMap[K,V]
        val finalOffset = (0 until size).foldLeft(off) { case (offset, _) =>
          val (k, kOffset) = kSer.unpack(data, offset)
          val (v, newOffset) = vSer.unpack(data, kOffset)
          em += k -> v
          newOffset
        }
        em.toMap -> finalOffset
      }
    }

  def pack[Elem](e:Elem)(implicit ser:BytePackable[Elem]):Array[Byte] = ser pack e
  def unpack[Elem](data:Array[Byte])(implicit ser:BytePackable[Elem]):Elem = ser.unpack(data, 0)._1
  def unpackFrom[Elem](data:Array[Byte], start:Int)(implicit ser:BytePackable[Elem]):(Elem, Int) = ser.unpack(data, start)

  def unpack[A, B](data:Array[Byte])(implicit aSer:BytePackable[A], bSer:BytePackable[B]):(A, B) = {
    val (a, aOff) = aSer.unpack(data, 0)
    val (b, _) = bSer.unpack(data, aOff)
    a -> b
  }
  def unpack[A, B, C](data:Array[Byte])(implicit aSer:BytePackable[A], bSer:BytePackable[B], cSer:BytePackable[C]):(A, B, C) = {
    val (a, aOff) = aSer.unpack(data, 0)
    val (b, bOff) = bSer.unpack(data, aOff)
    val (c, _) = cSer.unpack(data, bOff)
    (a, b, c)
  }
  def unpack[A, B, C, D](data:Array[Byte])(implicit aSer:BytePackable[A], bSer:BytePackable[B], cSer:BytePackable[C], dSer:BytePackable[D]):(A, B, C, D) = {
    val (a, aOff) = aSer.unpack(data, 0)
    val (b, bOff) = bSer.unpack(data, aOff)
    val (c, cOff) = cSer.unpack(data, bOff)
    val (d, _) = dSer.unpack(data, cOff)
    (a, b, c, d)
  }
}

object SerTest {
  import BytePackable._
  def main(args:Array[String]): Unit = {
    val dubs = Array(30.9, 239.23, 103.0)
    val m = Map("one" -> 1, "two" -> 2, "three" -> 3)
    val il = Seq(1, 2, 3)
    val l = Seq("one", "two", "three")
    val s = "a test"
    val i = 23124
    val d = 243.22342
    println("before: " + dubs.toSeq)
    println("before: " + m)
    println("before: " + il)
    println("before: " + l)
    println("before: " + s)
    println("before: " + i)
    println("before: " + d)


    val dubSer = pack(dubs)
    val dsm = pack(m)
    val dsis = pack(il)
    val dss = pack(l)
    val ds = pack(s)
    val dsi = pack(i)
    val dsd = pack(d)

    val ds2 = dsm ++ dss

    val (res, off) = unpackFrom[Map[String, Int]](ds2, 0)
    println(res)
    val (res2, _) = unpackFrom[Seq[String]](ds2, off)
    println(res2)

    val (u21, u22) = unpack[Map[String, Int], Seq[String]](ds2)
    println(u21, u22)

    println(unpack[Array[Double]](dubSer).toSeq)
    println(unpack[Map[String, Int]](dsm))
    println(unpack[Seq[Int]](dsis))
    println(unpack[Seq[String]](dss))
    println(unpack[String](ds))
    println(unpack[Int](dsi))
    println(unpack[Double](dsd))

    val dens = new DenseTensor1(dubs)

    println(unpack[DenseTensor1](pack(dens)))
    val modelFile = new File("/Users/johnsullivan/git/whip/cc/factorie/app/nlp/ner/ConllChainNer.factorie")
    val lexicons = new StaticLexicons()(new File("/Users/johnsullivan/git/whip/src/main/resources/cc/factorie/app/nlp/lexicon"))
    val ner = new ConllChainNer()(modelFile, lexicons)
    val nerPack = pack(ner.ChainNERFeaturesDomain.dimensionDomain) ++ pack(ner.model.parameters)

    val oldOs = new BufferedOutputStream(new GZIPOutputStream(new FileOutputStream("oldmodel.factorie")))
    ner.serialize(oldOs)
    oldOs.flush()
    oldOs.close()

    val os = new BufferedOutputStream(new GZIPOutputStream(new FileOutputStream("nerPack.factorie")))
    os write nerPack
    os.flush()
    os.close()

    //val (catDom, ws) = unpack[CategoricalDomain[String], WeightsSet](nerPack)
    //catDom.categories.take(20) foreach println

    //val (catDom, ws) = unpack[CategoricalDomain[String], WeightsSet](nerPack)
  }
}

