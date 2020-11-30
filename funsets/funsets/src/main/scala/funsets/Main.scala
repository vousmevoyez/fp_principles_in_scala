package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))


  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    //s2
    import scala.collection.mutable.Map
    var cumsum = 0
    val dic = Map[Int, Int]()
    val res = Array(1, 2)
    var found = false
    var i = 0
    while (i < nums.length && !found) {
      cumsum += nums(i)
      if (dic.contains(cumsum - target)) {
        res(0) = dic(cumsum - target)
        res(1) = i
        found = true
      }
      dic(cumsum) = i
      i += 1
    }
    res
  }
}
