package lockfree

import scala.annotation.tailrec

class SortedList extends AbstractSortedList {

  // The sentinel node at the head.
  private val _head: Node = createNode(0, None, isHead = true)

  // The first logical node is referenced by the head.
  def firstNode: Option[Node] = _head.next

  // Finds the first node whose value satisfies the predicate.
  // Returns the predecessor of the node and the node.
  def findNodeWithPrev(pred: Int => Boolean): (Node, Option[Node]) = 
    def rec(predecessor: Node, current: Option[Node]): (Node, Option[Node]) =
      current match
        case None => (predecessor, None)
        case Some(n) => 
          n.atomicState.get match
            case (next, false) => if (pred(n.value)) (predecessor, current) else rec(n, n.next)
            case (next, true) => predecessor.atomicState.compareAndSet((current, false), (next, false))
              findNodeWithPrev(pred)

    rec(_head, firstNode)

  // Insert an element in the list.
  def insert(e: Int): Unit = 
    val (prev, node) = findNodeWithPrev(n => n >= e)
    val newNode = (prev, node) match
      case (x, None) => createNode(e, None)
      case (a, Some(b)) => createNode(e, Some(b))
    
    if(!prev.atomicState.compareAndSet((node, false), (Some(newNode), false)))
      insert(e)

  // Checks if the list contains an element.
  def contains(e: Int): Boolean = findNodeWithPrev(n => n == e) match
    case (_, None) => false
    case (_, Some(_)) => true

  // Delete an element from the list.
  // Should only delete one element when multiple occurences are present.
  def delete(e: Int): Boolean = 
    val toDelete = findNodeWithPrev(n => n == e)
    val deleted = toDelete match
      case (n, None) => return false
      case (p, Some(c)) => c.mark
   
    if(!deleted) delete(e)
    else deleted
}
