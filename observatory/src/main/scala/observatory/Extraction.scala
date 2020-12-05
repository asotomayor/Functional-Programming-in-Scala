package observatory

import java.time.LocalDate

import org.apache.log4j.{Level, Logger}
import org.apache.spark.sql._
import org.apache.spark.sql.types._
import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.parallel.ParIterable
import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {

  import org.apache.spark.sql.SparkSession

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  val conf: SparkConf = new SparkConf().setAppName("My app").setMaster("local")
  val sc: SparkContext = new SparkContext(conf)

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Temperature Data Analysis")
      .master("local")
      .getOrCreate()

  import spark.implicits._

  def stationsRead(stationsFile: String): DataFrame = {
    val stationsSchema = new StructType()
      .add("STN", StringType, nullable = false)
      .add("WBAN", StringType, nullable = false)
      .add("Lat", DoubleType, nullable = false)
      .add("Lon", DoubleType, nullable = false)

    val stationsLines = Source.
      fromInputStream(getClass.getResourceAsStream(stationsFile), "utf-8")
      .getLines().toList

    val stationsRowRdd = sc.parallelize(stationsLines)
      .map(_.split(",", -1))
      .filter(arr => arr(2) != "" && arr(3) != "")
      .map(arr =>
        Row(
          arr(0),
          arr(1),
          arr(2).toDouble,
          arr(3).toDouble
        )
      )

    spark.createDataFrame(stationsRowRdd, stationsSchema)
  }

  def temperaturesRead(temperaturesFile: String): DataFrame = {
    val temperaturesSchema = new StructType()
      .add("STN", StringType, nullable = false)
      .add("WBAN", StringType, nullable = false)
      .add("Month", IntegerType, nullable = true)
      .add("Day", IntegerType, nullable = true)
      .add("Temperature", DoubleType, nullable = false)

    val temperaturesLines = Source.
      fromInputStream(getClass.getResourceAsStream(temperaturesFile), "utf-8")
      .getLines().toList

    val temperaturesRowRdd = sc.parallelize(temperaturesLines)
      .map(_.split(",", -1))
      .filter(arr => arr(4) != "9999.9")
      .map(arr =>
        Row(
          arr(0),
          arr(1),
          if (arr(2) == "") null else arr(2).toInt,
          if (arr(3) == "") null else arr(3).toInt,
          arr(4).toDouble
        )
      )

    spark.createDataFrame(temperaturesRowRdd, temperaturesSchema)
  }

  def preProcess(stationsDf: DataFrame, temperaturesDf: DataFrame): ParIterable[(Int, Int, Double, Double, Double)] = {
    temperaturesDf
      .withColumn("Temperature", ($"Temperature"-32)*5/9)
      .join(stationsDf, Seq("STN", "WBAN"), "inner")
      .map(row =>
        (
          row.getAs[Int](2),
          row.getAs[Int](3),
          row.getAs[Double](5),
          row.getAs[Double](6),
          row.getAs[Double](4)
        )
      ).collect().par
  }

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val stationsDf = stationsRead(stationsFile)
    val temperaturesDf = temperaturesRead(temperaturesFile)
    preProcess(stationsDf, temperaturesDf)
      .map { case (month, day, lat, lon, temperature) => (
        LocalDate.of(year, month, day),
        Location(lat, lon),
        temperature
      )
      }.toIndexedSeq
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    records
      .par
      .groupBy(r => r._2)
      .mapValues(
        temperatures => {
          val temp = temperatures.aggregate(0.0)(_ + _._3, _ + _)
          val avgTemperature = temp / temperatures.size
          avgTemperature
        }
      ).toIndexedSeq
  }

}