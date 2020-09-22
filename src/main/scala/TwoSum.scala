//my solution
object Solution {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    require {
      nums.length >= 2
    }

    val numsWithIndex = nums.zipWithIndex

    for((num, i) <- numsWithIndex; (nextNum, ii) <- numsWithIndex.slice(i+1, nums.length)) {

      val targetResult:Int = num + nextNum

      if (targetResult == target) {
        return Array(i, ii)
      }
    }
    throw new IllegalArgumentException(s"No sum of two numbers that gives you $target")
  }
}

// better solution I found online
// https://www.programmersought.com/article/92354413850/
object Solution {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    import scala.collection.mutable
    val map = mutable.Map[Int,Int]()
    for (i <- 0 until  nums.length){
      val temp = target-nums(i)
      if(map.contains(temp)){
        return Array(map.get(temp).get,i)
      }
      map(nums(i)) = i
    }
    throw new IllegalArgumentException("No two sum result")
  }
}
