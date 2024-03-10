import util.Pixel
import util.Util

import scala.annotation.tailrec

// Online viewer: https://0xc0de.fr/webppm/
object Solution {
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]

  // prerequisites
  def fromStringPPM(image: List[Char]): Image = {
    val width = image.drop(3).mkString.split(' ').head.toInt
    image.drop(3).dropWhile(_ != '\n').dropWhile(_ != '\n').drop(5).mkString.split("\\s+")
      .map(_.toInt).grouped(3).map(pixel => Pixel(pixel(0), pixel(1), pixel(2))).toList.grouped(width).toList
  }

  def toStringPPM(image: Image): List[Char] = {
    val header = s"P3\n${image.head.length} ${image.length}\n255\n".toList
    val pixels = image.flatMap(row => row.flatMap(pixel => List(pixel.red, pixel.green, pixel.blue)))
      .map(_.toString).grouped(3).map(_.mkString(" ")).mkString("\n") + "\n"
    header ++ pixels.toList
  }

  // ex 1
  def verticalConcat(image1: Image, image2: Image): Image = image1 ++ image2

  // ex 2
  def horizontalConcat(image1: Image, image2: Image): Image = image1.zip(image2).map(row => row._1 ++ row._2)

  // ex 3
  def rotate(image: Image, degrees: Integer): Image = {
    val rotation = degrees % 360
    rotation match
      case 0 => image
      case 90 => image.transpose.reverse
      case 180 => image.reverse.map(_.reverse)
      case 270 => image.transpose.map(_.reverse)
  }

  // ex 4
  val gaussianBlurKernel: GrayscaleImage = List[List[Double]](
    List( 1, 4, 7, 4, 1),
    List( 4,16,26,16, 4),
    List( 7,26,41,26, 7),
    List( 4,16,26,16, 4),
    List( 1, 4, 7, 4, 1)
  ).map(_.map(_ / 273))

  val Gx : GrayscaleImage = List(
    List(-1, 0, 1),
    List(-2, 0, 2),
    List(-1, 0, 1)
  )

  val Gy : GrayscaleImage = List(
    List( 1, 2, 1),
    List( 0, 0, 0),
    List(-1,-2,-1)
  )

  def edgeDetection(image: Image, threshold : Double): Image = {
    val grayscaledImage = image.map(_.map(Util.toGrayScale))
    val denoisedImage = applyConvolution(grayscaledImage, gaussianBlurKernel)
    val Mx = applyConvolution(denoisedImage, Gx)
    val My = applyConvolution(denoisedImage, Gy)
    Mx.zip(My).map(row => row._1.zip(row._2).map(pixel => pixel._1.abs + pixel._2.abs)).map(_.map(pixel => {
      if (pixel >= threshold) Pixel(255, 255, 255)
      else Pixel(0, 0, 0)
    }))
  }

  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage): GrayscaleImage = {
    def determineSum(imagePart: GrayscaleImage, kernel: GrayscaleImage) : Double = {
      def multiplyPair(acc: Double, pair: (Double, Double)): Double = acc + (pair._1 * pair._2)
      imagePart.flatten.zip(kernel.flatten).foldLeft(0.0)(multiplyPair)
    }
    Util.getNeighbors(image, (kernel.length - 1) / 2).map(_.map(determineSum(_, kernel)))
  }

  // ex 5
  def moduloPascal(m: Integer, funct: Integer => Pixel, size: Integer): Image = {
    val topRow = Integer.valueOf(1) :: List.fill(size - 1)(Integer.valueOf(-1))
    @tailrec
    def generateMatrix(current: Integer, matrix: List[List[Integer]]): List[List[Integer]] = {
      val prevRow = matrix.last
      @tailrec
      def generateRow(col1: Integer, col2: Integer, currentRow: List[Integer]): List[Integer] = {
        if (prevRow.apply(col2) == -1)
          currentRow ++ List(Integer.valueOf(1)) ++ List.fill(size - currentRow.length - 1)(Integer.valueOf(-1))
        else {
          generateRow(col1 + 1, col2 + 1, currentRow ++ List((prevRow.apply(col1) + prevRow.apply(col2)) % m))
        }
      }
      if (current == size - 1)
        matrix
      else generateMatrix(current + 1, matrix ++ List(generateRow(0, 1, List(1))))
    }
    generateMatrix(0, List(topRow)).map(_.map(elem => {
      if (elem == -1) Pixel(0, 0, 0)
      else funct(elem)
    }))
  }
}
