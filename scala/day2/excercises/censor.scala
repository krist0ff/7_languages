import scala.io.Source
import scala.collection.mutable.HashMap

trait Censor {
  val replacements = loadReplacements

  def loadReplacements = {
    val file = "day2/excercises/replacements.txt"
    val map = new HashMap[String, String]
    Source.fromFile(file).getLines.foreach{ (line) =>
      val Array(wrong, right) = line.split(" ")
      map += wrong -> right
    }
    map
  }

  def replaceCurseWords(original:String):String = {
    replacements.foldLeft(original) { (string,replacement) => 
      val (wrong,right) = replacement
      string.replaceAll("(?i)"+wrong,right)
    }
  }
}

object CensorTest extends Censor {
  def replace(text:String):String = {
    replaceCurseWords(text)
  }
}

println(CensorTest.replace("Oh Shoot, I forgot Darn meat again. Shoot, shoot, shoot"))
