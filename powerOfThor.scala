import math._
import scala.util._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 * ---
 * Hint: You can use the debug stream to print TX and TY, if Thor does not follow your orders.
 **/
object Player extends App {
    // lx: the X position of the light of power
    // ly: the Y position of the light of power
    // tx: Thor's starting X position
    // ty: Thor's starting Y position
    val Array(lx, ly, tx, ty) = for(i <- readLine split " ") yield i.toInt
    
    def calculateSteps(source: (Int,Int), destination: (Int,Int)): Int = math.max(math.abs(source._1 - destination._1), 
    math.abs(source._2 - destination._2))

    def go(s:(Int,Int),d:(Int,Int)): String = {
        if (d._1==s._1)
            if (d._2==s._2+1)
                "S"
            else "N"
        else if (d._1==s._1+1)
            if (d._2 ==s._2+1)
                "SE"
            else if (d._2==s._2)
                "E"
            else "NE"
        else if (d._1==s._1-1)
            if (d._2==s._2-1)
                "NW"
            else if (d._2==s._2)
                "W"
            else "SW"
        else ""
    }

    def makeDirections(s:(Int,Int)):Array[(Int,Int)] = {
        val a = new Array[(Int,Int)](8)
        
        a(0) = (s._1,s._2+1); a(1) = (s._1+1,s._2+1); a(2) = (s._1+1,s._2); a(3) = (s._1+1,s._2-1)
        a(4) = (s._1,s._2-1); a(5) = (s._1-1,s._2-1); a(6) = (s._1-1,s._2); a(7) = (s._1-1,s._2+1)
    
        return a
    }
    
    def bestDirection(startPos:(Int,Int), endPos: (Int,Int)): (String,(Int,Int),Int) = {
        val direction = makeDirections(startPos)
        val paths = direction.map(p => (calculateSteps(p,endPos),p))
        .filter(p => p._2._1 >= 0).filter(p => p._2._2 >= 0).sorted
        
        val bestpaths = paths.filter( p => p._1 <= paths(0)._1)
        var mostReasonablePath = new Array[Int](bestpaths.length)

        for (i <- 0 to bestpaths.length-1)
            mostReasonablePath(i) = (bestpaths(i)._2._1-lx).abs + (bestpaths(i)._2._2-ly).abs
        val gobest = bestpaths(mostReasonablePath.indexWhere( _ == mostReasonablePath.min))
        
        return (go(startPos,gobest._2),gobest._2,gobest._1)
    }

    val bestFirstMove = bestDirection((tx,ty),(lx,ly))
    val move = new Array[String](bestFirstMove._3+1)
    move(0) = bestFirstMove._1
    var newPos = bestFirstMove._2

    for( i <- 1 to move.length-1){
        var bestMove = bestDirection(newPos,(lx,ly))
        move(i) = bestMove._1
        newPos = bestMove._2
    }
    
    var i = 0
    // game loop
    while(true) {
        val e = readInt // The level of Thor's remaining energy, representing the number of moves he can still make.
        println(move(i))
        i+=1
    }
}