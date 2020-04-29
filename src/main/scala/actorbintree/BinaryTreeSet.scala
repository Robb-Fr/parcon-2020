/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection */
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  override def toString = "BinaryTreeSet of root "+root

  // optional (used to stash incoming operations during garbage collection)
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case op: Operation => root!(op)
    case GC => 
      val newRoot = createRoot
      root!CopyTo(newRoot)
      context.become(garbageCollecting(newRoot))
    case others => println("Unknown message received : "+others+" by "+this) }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = 
    case op: Operation => pendingQueue = pendingQueue.appended(op)
    case CopyFinished =>
      root = newRoot
      context.become(normal)
      pendingQueue.foreach(op => self!op)
      pendingQueue = Queue.empty[Operation]
    case GC => 
    case others => println("Unknown message received : "+others+" by "+this)

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  /**
   * Acknowledges that a copy has been completed. This message should be sent
   * from a node to its parent, when this node and all its children nodes have
   * finished being copied.
   */
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  override def toString = "BinaryTreeNode of elem "+elem

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case op: Operation => 
      val rightSub = if(elem < this.elem) Left else Right
      op match
        case Insert(requester, id, elem) => 
          if(elem == this.elem) 
            removed = false 
            requester!(OperationFinished(id))
          else 
            subtrees.get(rightSub) match
              case None => 
                subtrees = subtrees+(rightSub -> context.actorOf(props(elem, false)))
                requester!(OperationFinished(id))
              case Some(t) => t!(op)

        case Contains(requester, id, elem) => 
          if(elem == this.elem && removed == false)
            requester!(ContainsResult(id, true))
          else
            subtrees.get(rightSub) match
              case None => requester!(ContainsResult(id, false))
              case Some(t) => t!(op)

        case Remove(requester, id, elem) => 
          if(elem == this.elem) 
            removed = true 
            requester!(OperationFinished(id))
          else 
            subtrees.get(rightSub) match
              case None => requester!(OperationFinished(id))
              case Some(t) => t!(op)

    case CopyTo(treeNode) => 
      var expected: Set[ActorRef] = Set.empty

      if(!removed)
        treeNode!Insert(self, this.elem, this.elem)

      subtrees.get(Left) match
        case Some(a) => expected+=a
          a!CopyTo(treeNode)
        case _ =>

      subtrees.get(Right) match
        case Some(a) => expected+=a
          a!CopyTo(treeNode)
        case _ =>

      if(expected.isEmpty && removed)
        context.parent!CopyFinished
        self!PoisonPill
      else
        context.become(copying(expected, removed))

    case others => println("Unknown message received : "+others+" by "+this) }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = 
    case OperationFinished(_) => context.become(copying(expected, true))
      if((expected).isEmpty)
        context.parent!CopyFinished
        self!PoisonPill
    case CopyFinished => context.become(copying(expected-sender, insertConfirmed))
      if((expected-sender).isEmpty && insertConfirmed)
        context.parent!CopyFinished
        self!PoisonPill
    case others => println("Unknown message received : "+others+" by "+this)
}