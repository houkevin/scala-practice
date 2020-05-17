import scala.io.Source
import scala.util.control.Breaks._
import scala.collection.mutable._
class logEntry {
  var time = ""
  var category = ""
  var message = ""
  var timestamp = ""
  var ID = 0
}
object Command {
  val categories = scala.collection.mutable.Map[String, Array[Int]]()
  val keywords = scala.collection.mutable.Map[String, Array[Int]]()
  var logs = Array[logEntry]()
  var excerptList = Array[Int]()
  var preSearch = Array[Int]()
  var idxMap = Array[Int]()
  var timeVector = Array[String]()
  var command = ""

  def LogSort(left: logEntry, right: logEntry): Boolean = {
	// sort by timestamp, then category, then ID
    if (left.time != right.time) {
      left.time < right.time
    }
    else if(left.category.toLowerCase != right.category.toLowerCase) {
      left.category < right.category
    }
    else {
      left.ID < right.ID
    }
  }
  def convertTime(timestamp: String): String = {
	// remove : from timestamp
    val res = new StringBuilder()
    for (c <- timestamp) {
      if(c != ':') {
        res += c
      }
    }
    //println(res)
    res.toString()
  }
  def createHash(): Unit = {
    for(i <- logs.indices) {
	// create hash table for categories
      var copy = logs(i).category
      val temp = copy.toLowerCase
      if(!categories.contains(temp)) {
        categories += (temp -> new Array[Int](1))
      }
      else {
        categories(temp) = categories(temp) :+ i
      }
	  // create hash table for keywords
	  // categories first
      var words = split(temp)
      for(word <- words) {
        if(keywords.contains(word)) {
          if(keywords(word)(keywords(word).size - 1) != i) {
            keywords(word) = keywords(word) :+ i
          }
        }
        else {
          keywords += (word -> new Array[Int](1))
        }
      }
	  // messages second
      words = Array()
      copy = logs(i).message.toLowerCase
      words = split(copy)
      for(word <- words) {
        if(keywords.contains(word)) {
          if(keywords(word)(keywords(word).size - 1) != i) {
            keywords(word) = keywords(word) :+ i
          }
        }
        else {
          keywords += (word -> new Array[Int](1))
        }
      }
    }
  }
  def main(args: Array[String]): Unit = {

    val fname = "spec-test-log.txt"
    val fSource = Source.fromFile(fname)
    var ID = 0
	// read timestamp, category, message without vertical bar
    for(line<-fSource.getLines) {
      val store = line.split('|')
      val current = new logEntry()
      current.time = convertTime(store(0))
      current.category = store(1)
      current.message = store(2)
      current.timestamp = store(0)
      current.ID = ID
      ID += 1
      logs = logs :+ current
    }
	// initial sort for the log entires by timestamp, then category, then ID
    logs = logs.sortWith((left,right) => LogSort(left,right))
	// mapping of entryID -> actual log idx
    idxMap = new Array[Int](logs.size)
    for(i <- logs.indices) {
      idxMap(logs(i).ID) = i
    }
    for(log <- logs) {
      timeVector = timeVector :+ log.time
    }
    println(logs.size + " entries read")
    /*for(log<-logs) {
      print(log.category + "  ")
      print(log.ID + "  ")
      print(log.message + "  ")
      print(log.time + "  ")
      print(log.timestamp + "  ")
      println()
    }*/
    createHash()
    val file = "spec-test-cmds.txt"
    val source = Source.fromFile(file)
    for(line<-source.getLines) {
      val cmd = line(0)
      cmd match {
        case 't' => {
          preSearch = Array()
          tCommand(line.substring(1,30))
          command = "t"
        }
        case 'm' => {
          preSearch = Array()
          mCommand(line.substring(1,15))
          command = "m"
        }
        case 'c' => {
          preSearch = Array()
          cCommand(line.substring(2, line.size))
          command = "c"
        }
        case 'k' => {
          preSearch = Array()
          kCommand(line.substring(2, line.size))
          command = "k"
        }
        case 'a' => {
          aCommand(line.substring(2, line.size))
        }
        case 'r' => {
          rCommand()
        }
        case 'd' => {
          dCommand(line.substring(2, line.size))
        }
        case 'b' => {
          bCommand(line.substring(2, line.size))
        }
        case 'e' => {
          eCommand(line.substring(2, line.size))
        }
        case 'l' => lCommand()
        case 'g' => gCommand()
        case 'p' => pCommand()
        case 's' => sCommand()
        case 'q' =>
        case _ => println("doesn't exist")
      }
    }
  }


  def split(s : String): Array[String] = {
    val ordinary=(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toSet
    var temp = Array[String]()
    var start = 0
    var end = 0
    for(c <- s) {
	// if current not an alphanumeric
	// push the word into the vector
      if(ordinary.contains(c)) {
        end += 1
      }
      else if(start == end) {
        start += 1
        end += 1
      }
      else {
        temp = temp :+ s.substring(start, end)
        start = end + 1
        end += 1
      }
    }
    if(start != s.length) {
      temp = temp :+ s.substring(start, end)
    }
    temp
  }
  def gCommand(): Unit = {
	// if m or t need to print the range
    if(command == "m" || command == "t") {
      for(i <- preSearch(0) until preSearch(1)) {
        println(logs(i).ID + "|" + logs(i).timestamp + "|" + logs(i).category + "|" + logs(i).message)
      }
    }
    else if(command == "c" || command == "k") {
      for(i <- preSearch.indices) {
        println(logs(preSearch(i)).ID + "|" + logs(preSearch(i)).timestamp + "|" + logs(preSearch(i)).category + "|" + logs(preSearch(i)).message)
      }
    }
    else {
      println("no previous search")
    }
  }

  def rCommand(): Unit = {
	// if m or t need to print the range
    if(command == "m" || command == "t") {
      for(i <- preSearch(0) until preSearch(1)) {
        excerptList = excerptList :+ i
      }
      val res = preSearch(1) - preSearch(0)
      println(res + " log entries appended")
    }
    else if(command == "c" || command == "k") {
      for(i <- preSearch.indices) {
        excerptList = excerptList :+ preSearch(i)
      }
      println(preSearch.size + " log entries appended")
    }
    else {
      println("no previous search")
    }
  }

  def pCommand(): Unit = {
	// print everything in the excerpt list
    for(i <- excerptList.indices) {
      println(i + "|" + logs(excerptList(i)).ID + "|" + logs(excerptList(i)).timestamp + "|" + logs(excerptList(i)).category + "|" + logs(excerptList(i)).message)
    }
  }
  def aCommand(input: String): Unit = {
	// push whatever idx specified of the log list into the excerpt list
    val idx = input.toInt
    excerptList = excerptList :+ idxMap(idx)
    println("log entry " + idx + " appended")
  }

  def dCommand(input: String): Unit = {
	// remove specified idx from excerptlist
    val idx = input.toInt
    val store = collection.mutable.ArrayBuffer(excerptList: _*)
    store.remove(idx)
    excerptList = store.toArray
    println("Deleted excerpt list entry " + idx)
  }

  def bCommand(input: String): Unit = {
	// move specified idx to the beginning of excerptlist
    val idx = input.toInt
    val temp = excerptList(idx)
    val store = collection.mutable.ArrayBuffer(excerptList: _*)
    store.remove(idx)
    store.insert(0, temp)
    excerptList = store.toArray
    println("Moved excerpt list entry " + idx)
  }

  def eCommand(input: String): Unit = {
	// move specified idx to the end of the excerptlist
    val idx = input.toInt
    val temp = excerptList(idx)
    val store = collection.mutable.ArrayBuffer(excerptList: _*)
    store.remove(idx)
    store.append(temp)
    excerptList = store.toArray
    println("Moved excerpt list entry " + idx)
  }

  def lCommand(): Unit = {
	// if empty already
    if(excerptList.isEmpty) {
      println("excerpt list cleared")
      println("(previously empty")
    }
  }
  def sCommand(): Unit = {
	// if empty already
    if(excerptList.isEmpty) {
      println("excerpt list cleared")
      println("(previously empty")
    }
    else {
	// print before sorting excerptlist
      println("excerpt list sorted")
      println("previous ordering:")
      println(0 + "|" + logs(excerptList(0)).ID + "|" + logs(excerptList(0)).timestamp + "|" + logs(excerptList(0)).category + "|" + logs(excerptList(0)).message)
      println("...")
      println(excerptList.size + "|" + logs(excerptList(excerptList.size - 1)).ID + "|" + logs(excerptList(excerptList.size - 1)).timestamp + "|" + logs(excerptList(excerptList.size - 1)).category + "|" + logs(excerptList(excerptList.size - 1)).message)
      // print after sorting
	  println("new ordering:")
      excerptList.sorted
      println(0 + "|" + logs(excerptList(0)).ID + "|" + logs(excerptList(0)).timestamp + "|" + logs(excerptList(0)).category + "|" + logs(excerptList(0)).message)
      println("...")
      println(excerptList.size + "|" + logs(excerptList(excerptList.size - 1)).ID + "|" + logs(excerptList(excerptList.size - 1)).timestamp + "|" + logs(excerptList(excerptList.size - 1)).category + "|" + logs(excerptList(excerptList.size - 1)).message)
    }
  }
  def kCommand(input: String): Unit = {
    val temp = input.toLowerCase
    val words = temp.split(" ")
    val wordSet = Set[String]()
	// remove any duplicate keywords
    for(word <- words) {
      wordSet.add(word)
    }
    var isFound = true
    var found = Array[Int]()
    println(wordSet.head)
    if(keywords.contains(wordSet.head)) {
      found = keywords(wordSet.head).clone()
    }
    else {
      // one word not found, 0 entries found
      isFound = false
    }
    if(isFound) {
      breakable {
        for(word <- wordSet) {
          if(word != wordSet.head) {
            if(keywords.contains(word)) {
              preSearch = found.intersect(keywords(word))
              found = preSearch.clone()
              preSearch = Array()
            }
            else {
              isFound = false
              break()
            }
          }
        }
      }
    }
    if(!isFound) {
      preSearch = Array()
      println("Keyword search: 0 entries found")
    }
    else {
      preSearch = found.clone()
      println("Keyword search: " + preSearch.size + " entries found")
    }
  }
  def cCommand(input: String): Unit = {

    val temp = input.toLowerCase
    if(!categories.contains(temp)) {
      println("Category search: 0 entries found")
    }
    else {
      preSearch = categories(temp).clone()
      println("Category search: " + preSearch.size + " entries found")
    }
  }
  def mCommand(input: String): Unit = {
    val t1 = input.substring(1,14)
    val num1 = convertTime(t1)
    var upper = 0
    var lower = 0
    breakable {
      for(i <- timeVector.indices) {
        if(num1.toLong == timeVector(i).toLong) {
          lower = i
          break()
        }
      }
    }
    breakable {
      for(i <- timeVector.indices) {
        if(num1.toLong < timeVector(i).toLong) {
          upper = i
          break()
        }
      }
    }
    preSearch = preSearch :+ lower
    preSearch = preSearch :+ upper
    val res = upper - lower
    println("Timestamps search: " + res + " entries found")
  }
  def tCommand(input: String): Unit = {
    val t1 = input.substring(1,14)
    val t2 = input.substring(16,29)
    val num1 = convertTime(t1)
    val num2 = convertTime(t2)
    var upper = 0
    var lower = 0
	// find upper and lower range of this timestamp range
    breakable{
      for(i <- timeVector.indices) {
        if(num1.toLong >= timeVector(i).toLong) {
          lower = i
          break()
        }
      }
    }
    breakable {
      for(i <- timeVector.indices) {
        if(num2.toLong < timeVector(i).toLong) {
          upper = i
          break()
        }
      }
    }
	// push indexes into the preSearch vector
    preSearch = preSearch :+ lower
    preSearch = preSearch :+ upper
    val res = upper - lower
    println("Timestamps search: " + res + " entries found")
  }
}