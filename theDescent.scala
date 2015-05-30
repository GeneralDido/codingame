import math._
import scala.util._

object Player extends App {

    while(true) {
        val Array(sx, sy) = for(i <- readLine split " ") yield i.toInt
        
        val mh = new Array[Int](8)
        for(i <- 0 until 8) {
            mh(i) = readInt
        }
        
        if (sx == mh.indexWhere( _ == mh.max))
            println("FIRE") 
        else
            println("HOLD")
    }
}