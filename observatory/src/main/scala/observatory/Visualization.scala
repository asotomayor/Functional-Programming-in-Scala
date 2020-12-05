package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import scala.math.{pow, round}

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {
  final val P = 2
  final val width = 360
  final val height = 180

  val colorsList: List[(Temperature, Color)] = List(
    (60.0, Color(255, 255, 255)),
    (32.0, Color(255, 0, 0)),
    (12.0, Color(255, 255, 0)),
    (0.0, Color(0, 255, 255)),
    (-15.0, Color(0, 0, 255)),
    (-27.0, Color(255, 0, 255)),
    (-50.0, Color(33, 0, 107)),
    (-60.0, Color(0, 0, 0)),
  )

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    var minDist = Double.MaxValue
    var minDistTemperature = 0d
    var sumWeights = 0d
    var weightedSum = 0d
    temperatures.foreach {
      case (otherLoc: Location, temp: Temperature) =>
        val dist = location.getDistance(otherLoc)
        val w = 1 / pow(dist, P)
        sumWeights += w
        weightedSum += w * temp
        if (minDist > dist) {
          minDist = dist
          minDistTemperature = temp
        }
    }
    if (minDist < 1) minDistTemperature
    else weightedSum / sumWeights
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {

    def linInterpolate(t: Double, t0: Double, c0: Int, t1: Double, c1: Int): Int = {
      round(c0 + (t-t0)*(c1-c0)/(t1-t0)).toInt
    }

    var (t0, c0) = (Double.MinValue, points.head._2)
    var (t1, c1) = (Double.MaxValue, points.head._2)
    points.foreach {
      case (t, c) =>
        if (t <= value && t0 <= t) {
          t0 = t
          c0 = c
        }
        if (t >= value && t1 >= t) {
          t1 = t
          c1 = c
        }
    }
    if (t0 == t1) c0
    else {
      Color(
        linInterpolate(value, t0, c0.red, t1, c1.red),
        linInterpolate(value, t0, c0.green, t1, c1.green),
        linInterpolate(value, t0, c0.blue, t1, c1.blue)
      )
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val data = new Array[Pixel](width * height)
    for (x <- 0 until width; y <- 0 until height) {
      val lat = height/2 - y
      val lon = x - width/2
      val loc = Location(lat, lon)
      val temperature = predictTemperature(temperatures, loc)
      val col = interpolateColor(colors, temperature)
      data(y * width + x) = Pixel(col.red, col.green, col.blue, 255)
    }
    Image(width, height, data)
  }

}