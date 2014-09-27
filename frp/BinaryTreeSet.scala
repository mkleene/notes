/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue
import actorbintree.BinaryTreeSet.OperationReply

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

  /** Request to perform garbage collection*/
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

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    
    case op: Operation => 
      assert(pendingQueue.isEmpty)
      
      pendingQueue = pendingQueue :+ op
      sendMessageFromQueue
      
    case GC => doGC  
  }
  
  def sendMessageFromQueue() {
    
    if (pendingQueue.isEmpty) {
      context.become(normal)
      return
    }
    
    val msg = pendingQueue.head
    pendingQueue = pendingQueue.tail
    
    msg match {
      case i: Insert => root ! i.copy(requester = self)
      case c: Contains => root ! c.copy(requester = self)
      case r: Remove => root ! r.copy(requester = self)
    }
    
    context.become(waitingForOperation(msg))
  }
    
  def waitingForOperation(operation: Operation): Receive = {
    
    case o: Operation => pendingQueue = pendingQueue :+ o;
    
    case reply: OperationReply =>
        assert(reply.id == operation.id)
        operation.requester ! reply
        sendMessageFromQueue
        
    case GC => context.become(waitingToStartGC(operation))
  }
  
  def doGC() {
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context.become(garbageCollecting(newRoot)) 
  }
  
  def waitingToStartGC(operation: Operation): Receive = {
    
    case op: Operation => pendingQueue = pendingQueue :+ op
    
    case reply: OperationReply =>
      assert(reply.id == operation.id)
      operation.requester ! reply
      doGC
  }
  
  def waitingForCopy: Receive = {
    
    case o: Operation => pendingQueue = pendingQueue :+ o
    
    case CopyFinished => sendMessageFromQueue
    
    case GC => 
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case CopyFinished => 
      root = newRoot
      sendMessageFromQueue
      
    case op: Operation => pendingQueue = pendingQueue :+ op
    case GC =>
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  
  def getSide(i: Int): BinaryTreeNode.Position = {
      if (i < this.elem) Left else Right
  }
  
  def receive = rec(initiallyRemoved)

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  def rec(removed: Boolean): Receive = {
    
    case c@Contains(requester, id, elem) =>
      
      if (elem == this.elem) {
        requester ! ContainsResult(id, !removed)
      } else {
        this.subtrees.get(getSide(elem)) match {
          case Some(actor) => actor ! c
          case None => requester ! ContainsResult(id, false)
        }
      }
      
    case i@Insert(requester, id, elem) =>
      if (elem == this.elem) {
        if (removed) {
          context.become(rec(false))
        }
        requester ! OperationFinished(id)
      } else {
        val side = getSide(elem)
        this.subtrees.get(side) match {
            case Some(actor) => actor ! i
            case None => 
              val tree = context.actorOf(BinaryTreeNode.props(elem, initiallyRemoved = false))
              this.subtrees = this.subtrees + (side -> tree)
              requester ! OperationFinished(id)
        }
      }
      
    case r@Remove(requester, id, elem) =>
      if (elem == this.elem) {
        if (!removed) {
          context.become(rec(true))
        }
        requester ! OperationFinished(id)
      } else {
        subtrees.get(getSide(elem)) match {
          case Some(actor) => actor ! r
          case None => requester ! OperationFinished(id)
        }
      }
      
    case ct: CopyTo =>
  
      for (st <- subtrees.values) {
        st ! ct
      }
      
      if (!removed) {
          ct.treeNode ! Insert(self, -1, elem)
      }
      
      val noWorkForInsertion = subtrees.size == 0 && removed
      
      if (noWorkForInsertion) {
          context.parent ! CopyFinished
      } else {
          context.become(copying(subtrees.values.toSet, removed))
      }
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    
    case CopyFinished =>
      assert(expected.contains(sender))
      val newExpected = expected - sender
      if (newExpected.isEmpty && insertConfirmed) {
        context.parent ! CopyFinished
        context.stop(self)
      } else {
        context.become(copying(newExpected, insertConfirmed))
      }
      
    case OperationFinished(_) =>
      if (expected.isEmpty) {
        context.parent ! CopyFinished
        context.stop(self)
      } else {
        context.become(copying(expected, true))
      }
  }
}

