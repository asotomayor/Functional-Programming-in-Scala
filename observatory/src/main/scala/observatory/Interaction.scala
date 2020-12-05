package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import scala.math._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {
  final val width = 256
  final val height = 256

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    val n = pow(2, tile.zoom)
    val lon = tile.x / n * 360.0 - 180.0
    val latRad = atan(sinh(Pi * (1 - 2 * tile.y / n)))
    val lat = toDegrees(latRad)
    Location(lat, lon)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val img = new Array[Pixel](width * height)
    val left = tile.x * width
    val top = tile.y * height
    for (x <- 0 until width) {
      for (y <- 0 until height) {
        val loc = tileLocation(Tile(left + x, top + y, tile.zoom + 8))
        val temperature = Visualization.predictTemperature(temperatures, loc)
        val col = Visualization.interpolateColor(colors, temperature)
        img(y * width + x) = Pixel(col.red, col.green, col.blue, 127)
      }
    }
    Image(width, height, img)
  }

  def generateImage(year: Year, t: Tile,
                    data: Iterable[(Location, Temperature)]): Unit = {
    val img = tile(data, Visualization.colorsList, t)
    val path = "target/temperatures/%d/%d/".format(year, t.zoom)
    val fileName = "%d-%d.png".format(t.x, t.y)
    new java.io.File(path).mkdirs()
    img.output(new java.io.File(path + fileName))
    println(path + fileName)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
                           yearlyData: Iterable[(Year, Data)],
                           generateImage: (Year, Tile, Data) => Unit
                         ): Unit = {
    yearlyData.foreach { case (year, data) =>
      for (zoom <- 0 to 3) {
        val n = pow(2, zoom).toInt
        for (x <- (0 until n).par; y <- (0 until n).par) {
          generateImage(year, Tile(x, y, zoom), data)
        }
      }
    }
  }

}