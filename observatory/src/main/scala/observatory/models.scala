package observatory

import scala.math._

/**
  * Introduced in Week 1. Represents a location on the globe.
  * @param lat Degrees of latitude, -90 ≤ lat ≤ 90
  * @param lon Degrees of longitude, -180 ≤ lon ≤ 180
  */
case class Location(lat: Double, lon: Double) {

  def isAntipode(other: Location): Boolean = {
    (-lat == other.lat) && (lon + 180 == other.lon || lon - 180 == other.lon)
  }

  def getDistance(other: Location): Double = {
    val latRadians1 = toRadians(lat)
    val lonRadians1 = toRadians(lon)
    val latRadians2 = toRadians(other.lat)
    val lonRadians2 = toRadians(other.lon)

    val dLon = abs(lonRadians1 - lonRadians2)

    var centralAngle = 0.0
    if (equals(other)) {
      centralAngle = 0.0
    } else if (isAntipode(other)) {
      centralAngle = Pi
    } else {
      centralAngle = acos(sin(latRadians1)*sin(latRadians2) +
        cos(latRadians1)*cos(latRadians2)*cos(dLon))
    }

    // Radius of earth in kilometers
    val r = 6371.0

    // calculate distance
    r * centralAngle
  }
}

/**
  * Introduced in Week 3. Represents a tiled web map tile.
  * See https://en.wikipedia.org/wiki/Tiled_web_map
  * Based on http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
  * @param x X coordinate of the tile
  * @param y Y coordinate of the tile
  * @param zoom Zoom level, 0 ≤ zoom ≤ 19
  */
case class Tile(x: Int, y: Int, zoom: Int)

/**
  * Introduced in Week 4. Represents a point on a grid composed of
  * circles of latitudes and lines of longitude.
  * @param lat Circle of latitude in degrees, -89 ≤ lat ≤ 90
  * @param lon Line of longitude in degrees, -180 ≤ lon ≤ 179
  */
case class GridLocation(lat: Int, lon: Int)

/**
  * Introduced in Week 5. Represents a point inside of a grid cell.
  * @param x X coordinate inside the cell, 0 ≤ x ≤ 1
  * @param y Y coordinate inside the cell, 0 ≤ y ≤ 1
  */
case class CellPoint(x: Double, y: Double)

/**
  * Introduced in Week 2. Represents an RGB color.
  * @param red Level of red, 0 ≤ red ≤ 255
  * @param green Level of green, 0 ≤ green ≤ 255
  * @param blue Level of blue, 0 ≤ blue ≤ 255
  */
case class Color(red: Int, green: Int, blue: Int)