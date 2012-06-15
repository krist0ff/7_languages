val movies = <movies>
<movie>The increadibles</movie>
<movie>Wall E</movie>
<short>Jack Jack Attack</short>
<short>Ger's Game</short>
</movies>

(movies \ "_").foreach { movie => 
  movie match {
    case <movie>{movieName}</movie> => println("Movie: "+movieName)
    case <short>{shortName}</short> => println("Short: "+shortName)
  }
}
