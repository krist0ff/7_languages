import scala.actors._
import scala.actors.Actor._

case object Poke
case object Feed

class Kid(name:String) extends Actor {
  def act() {
    loop {
      react{
        case Poke => {
          println(name+": Ow...")
          println(name+": Quit it...")
        }
        case Feed => {
          println(name+": Gurgle...")
          println(name+": Burp...")
        }
      }
    }
  }
}

val bart = new Kid("Bart").start
val lisa = new Kid("Lisa").start

println("Ready to poke and feed...")

bart ! Poke
lisa ! Poke
bart ! Feed
lisa ! Feed



