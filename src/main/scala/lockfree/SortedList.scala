package lockfree

import scala.annotation.tailrec

class SortedList extends AbstractSortedList {

  // The sentinel node at the head.
  private val _head: Node = createNode(0, None, isHead = true)

  // The first logical node is referenced by the head.
  def firstNode: Option[Node] = _head.next

  // Finds the first node whose value satisfies the predicate.
  // Returns the predecessor of the node and the node.
  def findNodeWithPrev(pred: Int => Boolean): (Node, Option[Node]) = ???

  // Insert an element in the list.
  def insert(e: Int): Unit = ???

  // Checks if the list contains an element.
  def contains(e: Int): Boolean = ???

  // Delete an element from the list.
  // Should only delete one element when multiple occurences are present.
  def delete(e: Int): Boolean = ???
}
