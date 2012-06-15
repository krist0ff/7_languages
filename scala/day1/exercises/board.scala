import scala.util.Random

object Board {
  def empty:Board = new Board(Array(" "," "," "),Array(" "," "," "),Array(" "," "," "))
}

class Board(row1:Array[String],row2:Array[String],row3:Array[String]) {
  val players = List("X","O")
  val rows = List(row1,row2,row3)
  val wins = List(
    List((0,0),(0,1),(0,2)),List((1,0),(1,1),(1,2)),List((2,0),(2,1),(2,2)),
    List((0,0),(1,0),(2,0)),List((0,1),(1,1),(2,1)),List((0,2),(1,2),(2,2)),
    List((0,0),(1,1),(2,2)),List((2,0),(1,1),(0,2))
  )

  def valid():Boolean = {
    !(rows.length != 3 || rows.exists(row => {
        row.length != 3 || row.exists(field => field != "X" && field != "O" && field != " ")
    }))
  }

  def finished():Boolean = !rows.exists(row => row.exists(field => field == " "))

  def put(player:String,row:Int,column:Int) = {
    rows(row)(column) = player
    println("current board:")
    render
  }

  def validMove(player:String, row:Int, column:Int):Boolean = {
    !(row < 0 || row > 2 || column < 0 || column > 2 || rows(row)(column) != " ")
  }

  def render = {
    rows.foreach{ row => println(row(0) + "|" + row(1) + "|" + row(2)) }
  }

  def winner():String = {
    if(!valid)
      return "INVALID BOARD"

    wins.foreach( win => {
      val fieldMap = win.map(field => rows(field._1)(field._2))
      if(fieldMap == List("X","X","X"))
        return "X"
      else {
        if(fieldMap == List("O","O","O"))
          return "O"
      }
    })

    if(!finished) "NOT FINISHED" else "DRAW"
  }

  def start = {
    println("STARTING GAME")
    render

    val r = new Random
    var current = players(r.nextInt(2))

    while(winner == "NOT FINISHED") {
      println("Your move, " + current + " (ROW COLUMN)")
      val line = readLine()
      val split = line.split(" ")
      if(split.length != 2) {
        println("INVALID FORMAT")
      } else {
        val row = split(0).toInt
        val column = split(1).toInt
        if(validMove(current, row, column)) {
          put(current,row,column)
          current = if(current == "X") "O" else "X"
        } else {
          println("MOVE: " + current + ":" + row + " " + column +" is not valid")
        }
      }
      println(winner)
    }
    println("GAME FINISHED. WINNER: "+winner)
  }
}

//Sort of unit tests, need to set it up next time
var board = new Board(Array("O","X","X","O","X"),Array("O","X","O"),Array("X","O","X"))
println("invalid board")
println("winner: " + board.winner)

board = new Board(Array("X","O","O"),Array("O","X","O"),Array("X","O","X"))
println("X won across board")
println("winner: " + board.winner)

board = new Board(Array("O","X","O"),Array("O","X","O"),Array("X","O","X"))
println("draw board")
println("winner: " + board.winner)

board = new Board(Array(" ","X","O"),Array("O","X","O"),Array("X","O","X"))
println("not finished board")
println("winner: " + board.winner)

board = new Board(Array("X","X","O"),Array("X","O","X"),Array("X","O","X"))
println("x won in column board")
println("winner: " + board.winner)

board = new Board(Array("O","X","X"),Array("X","O","X"),Array("O","O","O"))
println("O won in row board")
println("winner: " + board.winner)

board = Board.empty
println("Empty board")
println("winner: " + board.winner)

board.start

