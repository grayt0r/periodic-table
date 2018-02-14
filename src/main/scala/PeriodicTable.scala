package com.grayt0r.periodicTable

object PeriodicTable extends App {
  val inputs = List("functions", "bacon", "poison", "sickness", "ticklish")

  val elementMap = parseCsv("elements.csv")

  val results = inputs.map(convertString)

  println()
  println(results.mkString("\n"))
  println()

  def parseCsv(path: String): Map[String, (String, String)] = {
    val bufferedSource = io.Source.fromFile(path)

    try {
      bufferedSource.getLines.map({ line =>
        val Array(name, symbol, _*) = line.split(",").map(_.trim)
        symbol.toLowerCase -> (symbol, name.toLowerCase)
      }).toMap
    } finally {
      bufferedSource.close
    }
  }

  def convertString(input: String): String = {
    def convertImpl(str: String): List[(String, String)] = {
      if (str.length > 0) {
        findLongestMatch(str) match {
          case tuple @ (symbol, _) => tuple :: convertImpl(str.drop(symbol.length))
        }
      } else {
        List()
      }
    }

    convertImpl(input).unzip match {
      case (str, elements) => s"${str.mkString("")} (${elements.mkString(", ")})"
    }
  }

  def findLongestMatch(str: String): (String, String) = {
    elementMap.getOrElse(str, findLongestMatch(str.dropRight(1)))
  }
}
