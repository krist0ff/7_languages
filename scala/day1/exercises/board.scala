import scala.util.Random

object Board {
  def empty:Board = {
    val board = new Board(Array(" "," "," "),Array(" "," "," "),Array(" "," "," "))
    board
  }
}

class Board(row1:Array[String],row2:Array[String],row3:Array[String]) {
  val players = List("X","O")
  val rows = List(row1,row2,row3)
  def valid():Boolean = {
    if(rows.length != 3) {
      return false
    }
    var v = true
    rows.foreach {row =>
      if(row.length != 3) {
        return false
      }
      row.foreach { field =>
        if((field != "X") && (field != "O") && (field != " ")) {
          return false
        }
      }
    }
    return v
  }

  def finished():Boolean = {
    rows.foreach{ row =>
      row.foreach{ field =>
        if(field == " ") {
          return false
        }
      }
    }
    true
  }

  def put(player:String,row:Int,column:Int) = {
    rows(row)(column) = player
    println("current board:")
    render
  }

  def validMove(player:String, row:Int, column:Int):Boolean = {
    if(row < 0 || row > 2 || column < 0 || column > 2) {
      return false
    }

    if(rows(row)(column) != " ") {
      return false
    }
    true
  }

  def render = {
    rows.foreach{ row => 
      println(row(0) + "|" + row(1) + "|" + row(2))
    }
  }

  def winner():String = {
    if(!valid) {
      return "INVALID BOARD"
    }
    List("X","O").foreach { player =>
      rows.foreach{ row => 
        if(row(0) == player && row(1) == player && row(2) == player) {
          return player
        }
      }
      (0 to 2).foreach { col =>
        if(rows(0)(col) == player && rows(1)(col) == player && rows(2)(col) == player) {
          return player
        }
      }
      if(rows(0)(0) == player && rows(1)(1) == player && rows(2)(2) == player) {
        return player
      }

      if(rows(0)(2) == player && rows(1)(1) == player && rows(2)(0) == player) {
        return player
      }
    }

    if(!finished) {
      return "NOT FINISHED" 
    } else {
      return "DRAW"
    }
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
          if(current == "X") {
            current = "O"
          } else {
            current = "X"
          }
        } else {
          println("MOVE: " + current + ":" + row + " " + column +" is not valid")
        }
      }
      println(winner)
    }
    println("GAME FINISHED. WINNER: "+winner)
  }
}

// Sort of unit tests, need to set it up next time
//board = new Board(Array("O","X","X","O","X"),Array("O","X","O"),Array("X","O","X"))
//println("invalid board")
//println("winner: " + board.winner)

//var board = new Board(Array("X","O","O"),Array("O","X","O"),Array("X","O","X"))
//println("X won across board")
//println("winner: " + board.winner)

//board = new Board(Array("O","X","O"),Array("O","X","O"),Array("X","O","X"))
//println("draw board")
//println("winner: " + board.winner)

//board = new Board(Array(" ","X","O"),Array("O","X","O"),Array("X","O","X"))
//println("not finished board")
//println("winner: " + board.winner)

//board = new Board(Array("X","X","O"),Array("X","O","X"),Array("X","O","X"))
//println("x won in column board")
//println("winner: " + board.winner)

//board = new Board(Array("O","X","X"),Array("X","O","X"),Array("O","O","O"))
//println("O won in row board")
//println("winner: " + board.winner)

val board = Board.empty
println("Empty board")
println("winner: " + board.winner)

board.start

