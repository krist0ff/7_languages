import scala.io._
import scala.actors._
import Actor._
import scala.util.matching.Regex

object PageLoader {
  def getPageSizeAndLinks(url: String) =  {
    println("getting url: "+url)
    val html = Source.fromURL(url).mkString
    val linkRegex = new Regex("""<a.*href="http://((\w|\:|\/|\?|\=|\.|\&)+)"""","url")
    val matchData = linkRegex.findAllIn(html).matchData
    val links = matchData.map{a => "http://"+a.group("url")}.toList

    println("Size for "+url+": "+html.length)
    println("Number of links for "+url+": "+links.length)

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
    val (size, links) = PageLoader.getPageSizeAndLinks(url)
    links.foreach(println)
    links.foreach{ link =>
      val (link_size, link_links) = PageLoader.getPageSizeAndLinks(link)
    }
    //println("Total size: "+linksSize.foldLeft(size){ (sum,ls) => sum + ls})
  }
}

def getPageSizeConcurrently() = {
  var caller = self
  for(url <- urls) {
   actor { caller ! (url, PageLoader.getPageSizeAndLinks(url)) }
  }

  for(url <- urls) {
    receive {
      case (url, (size,links)) =>
        println("Received")
    }
  }
}

println("Sequential run: ")
timeMethod{getPageSizeSequentially}

println("Concurrent run: ")
timeMethod{getPageSizeConcurrently}

