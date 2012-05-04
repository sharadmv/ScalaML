import java.util.{HashMap, LinkedList, Arrays}
import scala.collection.JavaConversions._
import scala.util.Random
import scala.io.Source
object MarkovChain {
  def main(args:Array[String]){
    var mc:MarkovChain[Char] = new MarkovChain[Char]()
      var source:String = Source.fromFile(args(0)).mkString("").trim()
      mc.train(source.toCharArray(),Integer.parseInt(args(1)))
      println(mc.walk(Integer.parseInt(args(1)),Integer.parseInt(args(2))).deep.mkString(""))
  }
}
class MarkovChain[T]() {
  var map:HashMap[T,Node] = new HashMap[T, Node]() 
    def walk[T:ClassManifest](depth:Int, length:Int):Array[Any] = {
      var arr = new Array[Any](length)
      var list = new LinkedList[Any]()
      walkFrom(depth,getRandom(),list , arr, 0, length)
      return arr 
    }
    private def getRandom():T = {
        var total = 0
        for((k,v) <- map) {
          total = total + v.weight
        }
      var threshold = new Random().nextDouble()
        for((k,v) <- map) {
          var prob = (v.weight+0.0)/total; 
          if (prob >= threshold) { 
            return k
          }
          threshold = threshold - prob
        }
      var temp = map.get(map.keySet().toArray()(0))
        var rand = new Random().nextInt(map.keySet().size())
      for (i <- 0 until map.keySet.size()){
        if (i==rand){
          temp = map.get(map.keySet().toArray()(i))
        }
      }
      return temp.value
    }
    private def walkFrom(depth:Int, value:T, t:LinkedList[Any],fin:Array[Any], i:Int,length:Int):Array[Any] = {
      if (i == length){
        return fin
      } else if (i==1){
      }
      t.offer(value)
      if (t.size() > depth){
        t.pop()
      }
      var newVal = map.get(value).random(t.asInstanceOf[LinkedList[T]],map)
      var v:T = value
      if (newVal != null) {
        v = newVal.value
      } else {
        v = getRandom()
      }
      fin(i) = v
      walkFrom(depth, v, t, fin, i+1, length)
    }
    def train(data:Array[T],max:Int) {
      for (t <- 0 until data.length){
        var temp:LinkedList[T] = new LinkedList[T]()
          for (d <- 0 until max) {
            if (t+d+1< data.length) {
              temp.add(data(t+d+1))
            }
          }
        var value = data(t)
          var node:Node = null
          if (map.containsKey(value)) {
            node = map.get(value)
          } else {
            node = new Node(value)
              map.put(value, node)
          }
        node.add(temp)
      }
    }
  class Node(v:T) {
    var value:T = v
      var weight:Int = 1
      var children:HashMap[T,Node] = new HashMap[T,Node]()
      private def checkChain(chain:LinkedList[T], node:Node):Node = {
        if (chain.size() == 0){
          return node
        }
        var dup = new LinkedList[T]()
        for (t <- chain){
          dup.add(t)
        }
        var item = dup.pop()
          if (node.children.containsKey(item)) {
            if (dup.size() == 0) {
              return node.children.get(item)
            } else {
              return checkChain(dup, node.children.get(item))
            }
          } else {
            return null
          }

      }
    def random(chain:LinkedList[T],map:HashMap[T,Node]):Node =  {
      var dup = new LinkedList[T]()
        for (t <- chain){
          dup.add(t)
        }
        if (dup.size() == 0){
          return null
        }
      var item = dup.pop()
      var t = checkChain(dup,map.get(item))
      while (t==null){
        item = dup.pop()
        t = checkChain(dup,map.get(item))
      }

      var temp = t.children
        var total = 0
        for((k,v) <- temp) {
          total = total + v.weight
        }
      var threshold = Math.random
        for((k,v) <- temp) {
          var prob = (v.weight+0.0)/total; 
          if (prob >= threshold) { 
            return v
          }
          threshold = threshold - prob
        }
      return null
    }
    def add(chain:LinkedList[T]){
      if (chain.size() == 0){
      } else if (chain.size() == 1) {
        var item:T = chain.get(0)
          if (children.containsKey(item)) {
            children.get(item).increment()
          } else {
            children.put(item, new Node(item))
          }
      } else {
        var item:T = chain.pop()
          var child:Node = null
          if (children.containsKey(item)) {
            child = children.get(item)
              child.increment()
          } else {
            child = new Node(item)
              children.put(item, child)
          }
        child.add(chain)
          chain.push(item)
      }
    }
    def increment() {
      weight  = weight + 1
    }
    override def toString() = "["+value+", "+weight+", "+children+"]"
  }
}
