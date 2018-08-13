object HighLists extends App {

  def squareList(xs: List[Int]): List[Int] =
    xs match {
      case Nil => Nil
      case y::ys => y*y :: squareList(ys)
    }

  def nSquareList(xs: List[Int]): List[Int] =
    xs.map(x => x*x)

  def posElems(xs: List[Int]): List[Int] =
    xs match {
      case Nil => Nil
      case y :: ys => if(y>0) y :: posElems(ys) else posElems(ys)
    }

  val nums = List(2, -4, 5, 7, 1)

  val temp = List("a", "a", "a", "b", "c", "c", "a")
  println(temp.dropWhile(y => y.equals("a")))

  println(nums.filter(x => x>0))
  println(nums.filterNot(x => x>0))
  println(nums.partition(x =>x>0))
  println(nums.takeWhile(x => x>0))
  println(nums.dropWhile(x=>x>0))
  println(nums.span(x=>x>0))

  def pack[T](xs: List[T]): List[List[T]] =
    xs match {
      case Nil => Nil
      case x :: xs1 => xs.takeWhile(y => y.equals(x)) :: pack(xs1.dropWhile(y => y.equals(x)))
    }


  println(pack(temp))

  def encode[T](xs: List[T]): List[(T, Int)] =
    xs match {
      case Nil => Nil
      case x :: xs1 =>  xs.takeWhile(y => y.equals(x)) match {
        case z :: zs => (z, (z::zs).size) :: encode(xs1.dropWhile(y => y.equals(x)))
      }

        //List(xs.takeWhile(y => y.equals(x)),xs.takeWhile(y => y.equals(x)).size) :: encode(xs1.dropWhile(y => y.equals(x)))
    }

  println(encode(temp))


  def concat[T](xs: List[T], ys:List[T]): List[T] =
    (xs foldRight ys) (_ :: _)

  def mapFun[T, U](xs: List[T], f: T => U): List[U] = {
    (xs foldRight List[U]()) (f(_) :: _)
  }

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)((x,y) => y+1)


  /*

  To prove (xs ++ ys) map f = (xs map f) ++ (ys map f)

  Recall Concat Defn

  Nil ++ ys = ys                                -> Concat 1
  (x :: xs1) ++ ys = x :: (xs1 ++ ys)           -> Concat 2

  Recall Map Defn

  Nil map f = Nil                               -> Map 1
  (x :: xs) map f = f(x) :: (xs map f)          -> Map 2

  Base Hypothesis to Prove:

  (Nil ++ ys) map f = (Nil map f) ++ (ys map f)

  LHS:
  (Nil ++ ys) map f = ys map f                  -> By Concat1
  RHS:
  (Nil map f) ++ (ys map f)
  = Nil ++ (ys map f)                           -> Map 1
  = ys map f                                    -> Concat1


  Therefore Induction Hypothesis:
  (xs ++ ys) map f = (xs map f) ++ (ys map f)   -> Induction 1

  To prove:
  (x :: xs ++ ys) map f = (x :: xs map f) ++ (ys map f)

  LHS:

   (x :: xs ++ ys) map f
   =  ( x :: (xs ++ ys)) map f                   -> by Concat 2
   =  f(x) :: ( xs ++ ys) map f                  -> Map 2 (unfold operation)
   =  f(x) :: (xs map f) ++ (ys map f)           -> By Induction 1
   = (x :: xs map f) ++ (ys map f)               -> By Map 2 (fold operation)
   = RHS , hence proved, EQD

   */
}
