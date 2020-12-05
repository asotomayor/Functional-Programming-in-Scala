package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation extends ManipulationInterface {
  final val width = 360
  final val height = 180

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    val gridTemp = new Array[Temperature](width * height)
    for (lat <- -89 to 90; lon <- -180 to 179) {
      val y = height/2 - lat
      val x = lon + width/2
      gridTemp(y * width + x) = Visualization.predictTemperature(temperatures, Location(lat, lon))
    }
    def getter(gridLoc: GridLocation): Temperature = {
      val y = height/2 - gridLoc.lat
      val x = gridLoc.lon + width/2
      gridTemp(y * width + x)
    }
    getter
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    val gridSumTemp = Array.fill[Temperature](width * height)(0.0)
    for (temperatures <- temperaturess) {
      val tempGetter = makeGrid(temperatures)
      for (lat <- -89 to 90; lon <- -180 to 179) {
        val y = height/2 - lat
        val x = lon + width/2
        gridSumTemp(y * width + x) += tempGetter(GridLocation(lat, lon))
      }
    }
    val gridAvgTemp = gridSumTemp.map(_/temperaturess.size)
    def getter(gridLoc: GridLocation): Temperature = {
      val y = height/2 - gridLoc.lat
      val x = gridLoc.lon + width/2
      gridAvgTemp(y * width + x)
    }
    getter
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    val devGrid = new Array[Temperature](width * height)
    val tempGetter = makeGrid(temperatures)
    for (lat <- -89 to 90; lon <- -180 to 179) {
      val y = height/2 - lat
      val x = lon + width/2
      val gridLoc = GridLocation(lat, lon)
      devGrid(y * width + x) = tempGetter(gridLoc) - normals(gridLoc)
    }
    def getter(gridLoc: GridLocation): Temperature = {
      val y = height/2 - gridLoc.lat
      val x = gridLoc.lon + width/2
      devGrid(y * width + x)
    }
    getter
  }

}
