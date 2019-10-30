// T: a list of things
// + defines the inheritance (and allows inheritance for the lists)
abstract class SimpleList[+T]
{ // abstraction
  def head: T
  def tail: SimpleList[T]
  def isEmpty: Boolean
  def length: Int

// :: appends an item at the beginning of the list
// S >: T means T is a subtype or an equal level to S

// In this case we are redefining :: as a function that adds a new element using ListNode function
  def ::[S >: T] (newHead: S):
    SimpleList[S] = new ListNode(newHead, this)
}

// ListNode <: SimpleList
class ListNode[T](hd: T, tl: SimpleList[T]) extends SimpleList[T] {
  // to change the value of the variables
  override def head = hd
  override def tail = tl
  override def isEmpty = false
  override def length = 1 + tail.length
}

// EmptyList <: SimpleList
object EmptyList extends SimpleList[Nothing] {
  // to change the value of the variables
  override def head = throw new NoSuchElementException
  override def tail = throw new NoSuchElementException
  override def isEmpty = true
  override def length: Int = 0
}

// example of a list of 4 number and an empty list
val oddNumbers = 1 :: 3 :: 5 :: 7 :: EmptyList

println(oddNumbers.length)
println(oddNumbers.tail)
println(oddNumbers.tail)

