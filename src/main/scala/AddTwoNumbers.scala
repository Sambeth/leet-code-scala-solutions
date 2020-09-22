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

  def getSmallerLinkedList(l1: ListNode, l2: ListNode): ListNode = {
    if (len(l1) > len(l2)) l2
    else l1
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

  def addZeroNodeAtStart(l: ListNode, numOfZeroes: Int): ListNode = {
    var zeroNode: ListNode = null
    var head: ListNode = l

    for(_ <- 0 until numOfZeroes) {
      zeroNode = new ListNode(0)
      zeroNode.next = head
      head = zeroNode
    }
    head
  }

  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    var first: ListNode = l1
    var second: ListNode = l2

    // 1. check the length of the linked lists and find the difference
    val diff: Int = differenceInLength(l1: ListNode, l2: ListNode)

    // 2. if diff is greater than 0 then prepend 0s to the head of the shorter linked list
    // if diff is 0 then both lists are the same length and do nothing
    if (diff > 0) {
      // find the shortest linked list and prepend with zeroes
      if (len(first) > len(second)) {
        second = addZeroNodeAtStart(second, diff)
      }
      else {
        first = addZeroNodeAtStart(first, diff)
      }
    }

    // 3. reverse the linked lists
    first = reverseList(first)
    second = reverseList(second)

    // 4. start main compute with
    // variables for adding two linked lists
    var p, q, tempNode, prevNode, finalLinkedList: ListNode = null
    var addition, divideBy10, remainder, carry, value: Int = 0

    p = first
    q = second

    while (p != null && q != null) {

      addition = p.x + q.x
      addition = addition + carry

      // after carry value is used revert it to zero
      carry = 0

      divideBy10 = addition / 10

      if (divideBy10 > 0) {
        remainder = addition % 10
        carry = divideBy10
        value = remainder
      } else {
        value = addition
      }

      // create new node
      tempNode = new ListNode(value)

      // obviously if this is the first run then set the head of the final linked list
      if (finalLinkedList == null) {
        finalLinkedList = tempNode

        // if this is not the first run, which means the head has already
        // been created then continue the link
      } else {
        prevNode.next = tempNode
      }

      // ready previous node for next iteration
      prevNode = tempNode

      // move to next node
      p = p.next
      q = q.next
    }

    // At the end of the while loop in carry is greater than zero
    // then obviously that value needs to be final node
    // otherwise no node will be create making the linked list
    // deficient of one node
    if (carry > 0) {
      tempNode.next = new ListNode(carry)
    }

    // return reverse linked list
    reverseList(finalLinkedList)
  }
}
