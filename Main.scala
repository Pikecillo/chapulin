import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage

object Main {
  
  def imageToArray(image: BufferedImage) : Array[Array[Array[Double]]] = {
    val w = image.getWidth
    val h = image.getHeight

    var array = Array.ofDim[Double](w, h, 3)

    for (x <- 0 until w)
      for (y <- 0 until h) {
	val pixel = image.getRGB(x, y);
	array(x)(y)(0) = (pixel >> 16 & 0xff)
	array(x)(y)(1) = (pixel >> 8 & 0xff)
	array(x)(y)(2) = (pixel & 0xff)
      }

    array
  }

  def arrayToImage(array: Array[Array[Array[Double]]]) : BufferedImage = {
    val w = array.size
    val h = array(0).size

    val image = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)

    val clamp = (x: Double, lo: Double, hi: Double) => (x.max(lo)).min(hi)

    for (x <- 0 until w)
      for (y <- 0 until h) {
	val r : Int = clamp(array(x)(y)(0), 0.0, 255.0).asInstanceOf[Int]
	val g : Int = clamp(array(x)(y)(1), 0.0, 255.0).asInstanceOf[Int]
	val b : Int = clamp(array(x)(y)(2), 0.0, 255.0).asInstanceOf[Int]
	val color = (r << 16) | (g << 8) | b;

	image.setRGB(x, y, color);
      }

    image
  }

  def perChannelMean(array: Array[Array[Array[Double]]]) : Array[Double] = {
    val arrayWidth = array.size
    val arrayHeight = array(0).size

    var r : Double = 0.0
    var g : Double = 0.0
    var b : Double = 0.0

    for(x <- 0 until arrayWidth)
      for(y <- 0 until arrayHeight) {
	r += array(x)(y)(0)
	g += array(x)(y)(1)
	b += array(x)(y)(2)
      }

    val numPixels = arrayWidth * arrayHeight

    Array(r / numPixels, g / numPixels, b / numPixels)
  }

  def perChannelStdDev(array: Array[Array[Array[Double]]]) : Array[Double] = {
    val d1 = array.size
    val d2 = array(0).size

    var c1 = 0.0
    var c2 = 0.0
    var c3 = 0.0

    val mean = perChannelMean(array)

    for(x <- 0 until d1)
      for(y <- 0 until d2) {
	c1 += scala.math.pow(array(x)(y)(0) - mean(0), 2.0) 
	c2 += scala.math.pow(array(x)(y)(1) - mean(1), 2.0)
	c3 += scala.math.pow(array(x)(y)(2) - mean(2), 2.0)
      }

    val n : Double = d1 * d2

    Array(scala.math.sqrt(c1 / n),
	  scala.math.sqrt(c2 / n),
	  scala.math.sqrt(c3 / n))
  }
  
  def transfer(sourceArray: Array[Array[Array[Double]]],
	       targetArray: Array[Array[Array[Double]]])
  : Array[Array[Array[Double]]] = {
    val sourceMean = perChannelMean(sourceArray)
    val targetMean = perChannelMean(targetArray)
    val sourceStdDev = perChannelStdDev(sourceArray)
    val targetStdDev = perChannelStdDev(targetArray) 

    val sourceWidth = sourceArray.size
    val sourceHeight = sourceArray(0).size

    var transferArray = Array.ofDim[Double](sourceWidth, sourceHeight, 3)

    for(x <- 0 until sourceWidth)
      for(y <- 0 until sourceHeight) {
	transferArray(x)(y)(0) = (sourceArray(x)(y)(0) -
	  sourceMean(0)) * targetStdDev(0) / sourceStdDev(0) + targetMean(0)
	transferArray(x)(y)(1) = (sourceArray(x)(y)(1) -
	  sourceMean(1)) * targetStdDev(1) / sourceStdDev(1) + targetMean(1)
	transferArray(x)(y)(2) = (sourceArray(x)(y)(2) -
	  sourceMean(2)) * targetStdDev(2) / sourceStdDev(2) + targetMean(2)
      }

    transferArray
  }

  def main(args: Array[String]): Unit = {
    val sourceImage = ImageIO.read(new File("images/gran_sabana.jpg"))
    val targetImage = ImageIO.read(new File("images/sahara_dessert.jpg"))
    
    val sourceArray = imageToArray(sourceImage)
    val targetArray = imageToArray(targetImage)
    
    val transferArray = transfer(sourceArray, targetArray)   
    
    val transferImage = arrayToImage(transferArray) 
    
    ImageIO.write(transferImage, "jpg", new File("images/transfer.jpg"))
  }
}
