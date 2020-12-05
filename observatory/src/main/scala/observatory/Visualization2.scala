package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface {
  final val width = 256
  final val height = 256

  val colorsList: List[(Temperature, Color)] = List(
    (7.0, Color(0, 0, 0)),
    (4.0, Color(255, 0, 0)),
    (2.0, Color(255, 255, 0)),
    (0.0, Color(255, 255, 255)),
    (-2.0, Color(0, 255, 255)),
    (-7.0, Color(0, 0, 255))
  )

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
                             point: CellPoint,
                             d00: Temperature,
                             d01: Temperature,
                             d10: Temperature,
                             d11: Temperature
                           ): Temperature = {
    val x = point.x
    val y = point.y
    d00*(1-x)*(1-y) + d10*x*(1-y) + d01*(1-x)*y + d11*x*y
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
                     grid: GridLocation => Temperature,
                     colors: Iterable[(Temperature, Color)],
                     tile: Tile
                   ): Image = {
    val img = new Array[Pixel](width * height)
    val left = tile.x * width
    val top = tile.y * height
    for (x <- 0 until width) {
      for (y <- 0 until height) {
        val loc = Interaction.tileLocation(Tile(left + x, top + y, tile.zoom + 8))
        val lat = loc.lat.toInt
        val lon = loc.lon.toInt

        val p00 = GridLocation(lat, lon)
        val p01 = GridLocation(lat + 1, lon)
        val p10 = GridLocation(lat, lon + 1)
        val p11 = GridLocation(lat + 1, lon + 1)

        val point = CellPoint(loc.lon - lon, loc.lat - lat)

        val temperature = bilinearInterpolation(point, grid(p00), grid(p01), grid(p10), grid(p11))
        val col = Visualization.interpolateColor(colors, temperature)

        img(y * width + x) = Pixel(col.red, col.green, col.blue, 127)
      }
    }
    Image(width, height, img)
  }

}