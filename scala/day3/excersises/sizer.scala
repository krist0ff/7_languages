import scala.io._
import scala.actors._
import Actor._
import scala.util.matching.Regex

object PageLoader {
  val linkRegex = new Regex("""<a.*href="http://((\w|\:|\/|\?|\=|\.|\&)+)"""","url")

  def getPageSizeAndLinks(url: String, findLinks: Boolean) =  {

    val html = try {
      Source.fromURL(url).mkString
    } catch {
      case _ => {
        println("Url "+url+" could not be loaded")
        ""
      }
    }
    
    // returns no links if passed param is false
    val links = if(findLinks) {
      val matchData = linkRegex.findAllIn(html).matchData
      matchData.map{a => "http://"+a.group("url")}.toList
    } else {
      List()
    }

    (html.length, links)
  }
}

val urls = List("http://yahoo.com",
  "http://www.twitter.com",
  "http://www.google.com",
  "http://www.cnn.com"
)

def timeMethod(method: () => Unit) = {
  val start = System.nanoTime
  method()
  val end = System.nanoTime
  println("Method took "+ (end - start)/1000000000.0 +" seconds.")
}

def getPageSizeSequentially() = {
  for(url <- urls) {
    val (size, links) = PageLoader.getPageSizeAndLinks(url, true)
    val total = links.foldLeft(size) { (sum,link) =>
      val (link_size, link_links) = PageLoader.getPageSizeAndLinks(link, false)
      sum + link_size
    }
    println("Total size for "+url+" : "+total)
  }
}

def getPageSizeConcurrently() = {
  var caller = self
  for(url <- urls) {
   actor { caller ! (url, PageLoader.getPageSizeAndLinks(url, true)) }
  }

  for(url <- urls) {
    receive {
      case (url, (size:Int,links:List[String])) =>
        println("Finished: "+url)
        val total = links.par.map{ (url) =>
          val (size, links) = PageLoader.getPageSizeAndLinks(url,false) 
          size
        }.sum
        println("Total size for "+url+" : "+total)
    }
  }
}

println("Sequential run: ")
timeMethod{getPageSizeSequentially}

println("Concurrent run: ")
timeMethod{ getPageSizeConcurrently }

