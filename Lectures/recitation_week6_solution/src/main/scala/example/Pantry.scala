package example
import io.circe.parser
import scala.io.StdIn._

import io.circe.generic.auto._, io.circe.syntax._
case class Item(name: String,quantity: Int)

object Pantry extends App {
  var baseStock = List(Item("burger",20), Item("stir fry", 10))
  process_loop()

  /**
  * Commands
  * j <name of file> : load json file containing stock info
  * l <flag> : list inventory
  * <list index> <value>
  */

def processCommand(s:String,baseStock: List[Item]) : String => List[Item] = {
  s match {
    case "j" => {
        println("Please enter the name of the file to load(json)") 
        (s2:String) => {
        val data = scala.io.Source.fromFile(s2).mkString
      
        parser.parse(data) match {
        case Left(_) => throw new Exception("Could not parse file")
        case Right(json) => json.as[List[Item]].getOrElse(throw new Exception("Incorrect format"))
        }
      }
    }
    case "l" => {
      println("Enter a flag for how to sort")
      println("q - by quantity")
      println("n - by name")
      println("i - no sort(id)")
      (s2:String) => {
      s2 match {
        case "q" => println(baseStock.sortBy(x => x.quantity))
        case "n" => println(baseStock.sortBy(x => x.name))
        case "i" => println(baseStock)
        case _ => throw new Exception("invalid command")
        }
        baseStock
      }
    }
    case s => {
      val i = s.toInt 
      if (i < baseStock.length){
        println("Enter a new quantity for the item")
        (s2: String) => {
          val v = s2.toInt 
          baseStock(i) match {
            case Item(n,q) => { 
              baseStock.updated(i,Item(n,v))
            }
          }
        }
      } else {
        throw new Exception("Index out of Bounds")
      }
    }    
  }
}

def process_loop(): Unit = {
  println("Welcome to Pantry")
  println("Available Commands:")
  println("j : give a json file to load stock from")
  println("l : sort and print list")
  println("index in list : update quantity of item")
  println("quit : exit")
  println("Please enter a command: ")
  var input = readLine()
  while (input != "quit") {
    try {
      val inter = processCommand(input,baseStock)
      baseStock = inter(readLine())
    } catch {
      case e: Throwable  => {
        println("Encountered Error:")
        println(e)
      }
    }
    println("Please enter a command: ")
    input = readLine()
  }
  println("Closing pantry, goodbye")
}
}
