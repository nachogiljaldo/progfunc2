package quickcheck.test

// Figure 3, page 7
trait BinomialHeap extends quickcheck.Heap {

  type Rank = Int
  case class Node(value: A, rank: Rank, tail: List[Node])
  override type H = List[Node]

  protected def root(t: Node) = t.value
  protected def rank(t: Node) = t.rank
  protected def link(t1: Node, t2: Node): Node = // t1.rank==t2.rank
    if (ord.lteq(t1.value,t2.value)) Node(t1.value, t1.rank+1, t2::t1.tail) else Node(t2.value, t2.rank+1, t1::t2.tail)
  protected def ins(t: Node, ts: H): H = ts match {
    case Nil => List(t)
    case tp::ts => // t.rank<=tp.rank
      if (t.rank<tp.rank) t::tp::ts else ins(link(t, tp), ts)
  }

  override def empty = Nil
  override def isEmpty(ts: H) = ts.isEmpty

  override def insert(x: A, ts: H) = ins(Node(x,0,Nil), ts)
  override def meld(ts1: H, ts2: H) = (ts1, ts2) match {
    case (Nil, ts) => ts
    case (ts, Nil) => ts
    case (t1::ts1, t2::ts2) =>
      if (t1.rank<t2.rank) t1::meld(ts1,t2::ts2)
      else if (t2.rank<t1.rank) t2::meld(t1::ts1,ts2)
      else ins(link(t1,t2),meld(ts1,ts2))
  }

  override def findMin(ts: H) = ts match {
    case Nil => throw new NoSuchElementException("min of empty heap")
    case t::Nil => root(t)
    case t::ts =>
      val x = findMin(ts)
      if (ord.lteq(root(t),x)) root(t) else x
  }
  override def deleteMin(ts: H) = ts match {
    case Nil => throw new NoSuchElementException("delete min of empty heap")
    case t::ts =>
      def getMin(t: Node, ts: H): (Node, H) = ts match {
        case Nil => (t, Nil)
        case tp::tsp =>
          val (tq,tsq) = getMin(tp, tsp)
          if (ord.lteq(root(t),root(tq))) (t,ts) else (tq,t::tsq)
      }
      val (Node(_,_,c),tsq) = getMin(t, ts)
      meld(c.reverse, tsq)
  }
}

trait Bogus1BinomialHeap extends BinomialHeap {
  override def findMin(ts: H) = ts match {
    case Nil => throw new NoSuchElementException("min of empty heap")
    case t::ts => root(t)
  }
}

trait Bogus2BinomialHeap extends BinomialHeap {
  override protected def link(t1: Node, t2: Node): Node = // t1.rank==t2.rank
    if (!ord.lteq(t1.value,t2.value)) Node(t1.value, t1.rank+1, t2::t1.tail) else Node(t2.value, t2.rank+1, t1::t2.tail)
}

trait Bogus3BinomialHeap extends BinomialHeap {
  override protected def link(t1: Node, t2: Node): Node = // t1.rank==t2.rank
    if (ord.lteq(t1.value,t2.value)) Node(t1.value, t1.rank+1, t1::t1.tail) else Node(t2.value, t2.rank+1, t2::t2.tail)
}

trait Bogus4BinomialHeap extends BinomialHeap {
  override def deleteMin(ts: H) = ts match {
    case Nil => throw new NoSuchElementException("delete min of empty heap")
    case t::ts => meld(t.tail.reverse, ts)
  }
}

trait Bogus5BinomialHeap extends BinomialHeap {
  override def meld(ts1: H, ts2: H) = ts1 match {
    case Nil => ts2
    case t1::ts1 => List(Node(t1.value, t1.rank, ts1++ts2))
  }
}
