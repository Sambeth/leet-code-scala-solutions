class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x

  override def toString: String = s"$x -> $next"
}

object Solution {

  def len(l: ListNode): Int = {
    var counter: Int = 0
    var node = l

    while(node != null) {
      counter += 1
      node = node.next
    }
    counter
  }

  def differenceInLength(l1: ListNode, l2: ListNode): Int = {
    val diff = len(l1) - len(l2)
    diff.abs
  }

  def reverseList(head: ListNode): ListNode = {
    var prev: ListNode = null
    var curr: ListNode = head
    var next: ListNode = null

    while (curr != null) {
      next = curr.next
      curr.next = prev
      prev = curr
      curr = next
    }
    prev
  }

  def addZeroNodeAtStart(l: ListNode, numOfZeroes: Int = 1): ListNode = {
    var zeroNode: ListNode = null
    var head: ListNode = l

    for(_ <- 0 until numOfZeroes) {
      zeroNode = new ListNode(0)
      zeroNode.next = head
      head = zeroNode
    }
    head
  }
}
