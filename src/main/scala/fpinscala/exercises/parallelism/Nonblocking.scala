package fpinscala.exercises.parallelism

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CompletableFuture
import scala.util.Using
// import java.util.concurrent.StructuredTaskScope

object Nonblocking:

  type Future[+A] = (A => Unit) => Unit

  type Par[+A] = ExecutorService => Future[A]

  object Par:

    def unit[A](a: A): Par[A] =
      es => cb => cb(a)

    /** A non-strict version of `unit` */
    def delay[A](a: => A): Par[A] =
      es => cb => cb(a)

    def fork[A](a: => Par[A]): Par[A] =
      es => cb => eval(es)(a(es)(cb))

    /**
     * Helper function for constructing `Par` values out of calls to non-blocking continuation-passing-style APIs.
     * This will come in handy in Chapter 13.
     */
    def async[A](f: (A => Unit) => Unit): Par[A] = 
      es => cb => f(cb)

    /**
     * Helper function, for evaluating an action
     * asynchronously, using the given `ExecutorService`.
     */
    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r })

    extension [A](p: Par[A])
      // def run(es: ExecutorService): A =
      //   val ref = new AtomicReference[A] // A mutable, threadsafe reference, to use for storing the result
      //   val latch = new CountDownLatch(1) // A latch which, when decremented, implies that `ref` has the result
      //   p(es) { a => ref.set(a); latch.countDown } // Asynchronously set the result, and decrement the latch
      //   latch.await // Block until the `latch.countDown` is invoked asynchronously
      //   ref.get // Once we've passed the latch, we know `ref` has been set, and return its value

      def run(es: ExecutorService): A =
        val future = CompletableFuture[A]()
        p(es) { a => future.complete(a) } // Set the result in the CompletableFuture
        future.join() // Block until the result is available and return it

      // Requires Java with StructuredTaskScope support
      // def map2StructuredTaskScope[B, C](p2: Par[B])(f: (A, B) => C): Par[C] =
      //   es => cb =>
      //         val refA = new AtomicReference[A]()
      //         val refB = new AtomicReference[B]()
      //         val latch = new CountDownLatch(2) // Need two results

      //         val f1 = p(es)
      //         val f2 = p2(es)

      //         Using(new StructuredTaskScope()) { scope =>
      //           // Fork task for p
      //           scope.fork(new Callable[Object] {
      //             def call: Object = {
      //               f1 { a =>
      //                 refA.set(a)
      //                 latch.countDown()
      //               }
      //               null // StructuredTaskScope doesn't use the Callable's return value
      //             }
      //           })

      //           // Fork task for p2
      //           scope.fork(new Callable[Object] {
      //             def call: Object = {
      //               f2 { b =>
      //                 refB.set(b)
      //                 latch.countDown()
      //               }
      //               null // StructuredTaskScope doesn't use the Callable's return value
      //             }
      //           })

      //           scope.join() // Wait for both tasks to complete
      //         }

      //         // After join, wait for both results and compute the final value
      //         latch.await()
      //         val result = f(refA.get(), refB.get())
      //         cb(result)

      def map2[B, C](p2: Par[B])(f: (A, B) => C): Par[C] =
        es => cb =>
          val futureA = CompletableFuture[A]()
          val futureB = CompletableFuture[B]()

          p(es) { a => futureA.complete(a) }
          p2(es) { b => futureB.complete(b) }

          // Combine the results of both futures
          futureA.thenCombine(futureB, (a: A, b: B) => f(a, b))
                 .whenComplete((result, ex) => 
                   if ex == null then cb(result)
                   else throw ex // Propagate any exceptions
                 )

      // This is from the book and uses the Actor implementation to coordinate the two futures
      def map2Actor[B, C](p2: Par[B])(f: (A, B) => C): Par[C] =
        es => cb =>
          var ar: Option[A] = None
          var br: Option[B] = None
          // Author note:
          // this implementation is a little too liberal in forking of threads -
          // it forks a new logical thread for the actor and for stack-safety,
          // forks evaluation of the callback `cb`
          // Justin's note:
          // combiner is an actor. the handler function that follows simply coordinates
          // the two Par executions of p and p2.
          // The first result to arrive is stored as an Option 
          // then when the second result arrives it is able to 
          // complete the callback.
          val combiner = Actor[Either[A,B]](es):
            case Left(a) =>
              if br.isDefined then eval(es)(cb(f(a, br.get)))
              else ar = Some(a)
            case Right(b) =>
              if ar.isDefined then eval(es)(cb(f(ar.get, b)))
              else br = Some(b)
          // Each par is evaluated and the callback sends the result
          // to the combiner actor.
          p(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))

      def map[B](f: A => B): Par[B] =
        es => cb => p(es)(a => eval(es)(cb(f(a))))

      def flatMap[B](f: A => Par[B]): Par[B] =
        es => cb => p(es)(a => f(a)(es)(cb))

      def zip[B](b: Par[B]): Par[(A,B)] = map2(b)((_,_))

    def lazyUnit[A](a: => A): Par[A] =
      fork(unit(a))

    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
      as match
        case Nil => unit(Nil)
        case h :: t => h.map2(fork(sequence(t)))(_ :: _)

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork:
      if as.isEmpty then unit(Vector())
      else if as.length == 1 then map(as.head)(a => Vector(a))
      else
        val (l, r) = as.splitAt(as.length / 2)
        sequenceBalanced(l).map2(sequenceBalanced(r))(_ ++ _)

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
      sequence(as.map(asyncF(f)))

    def parMap[A, B](as: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] =
      sequenceBalanced(as.map(asyncF(f)))

    // exercise answers

    /*
     * We can implement `choice` as a new primitive.
     *
     * `p(es)(result => ...)` for some `ExecutorService`, `es`, and
     * some `Par`, `p`, is the idiom for running `p`, and registering
     * a callback to be invoked when its result is available. The
     * result will be bound to `result` in the function passed to
     * `p(es)`.
     *
     * If you find this code difficult to follow, you may want to
     * write down the type of each subexpression and follow the types
     * through the implementation. What is the type of `p(es)`? What
     * about `t(es)`? What about `t(es)(cb)`?
     */
    def choice[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es => cb => p(es): b =>
        if b then eval(es)(t(es)(cb))
        else eval(es)(f(es)(cb))

    /* The code here is very similar. */
    def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] =
      es => cb => p(es)(n =>
        eval(es)(ps(n % ps.length)(es)(cb)))

    def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
        choiceN(a.map(b => if b then 0 else 1))(List(ifTrue, ifFalse))
      // es => cb => a(es): b => 
      //   val index = if b then 0 else 1
      //   val cl = List(ifTrue, ifFalse)
      //   choiceN(Par.unit(index))(cl)

    def choiceMap[K, V](p: Par[K])(ps: Map[K, Par[V]]): Par[V] =
      es => cd => p(es): k =>
        ps.get(k) match
          case Some(v) => eval(es)(v(es)(cd))
          case None => throw new IllegalArgumentException("key not found")

    /* `chooser` is usually called `flatMap` or `bind`. */
    def chooser[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
      es => cb => p(es): a =>
        eval(es)(f(a)(es)(cb))

    def choiceViaFlatMap[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
      p.flatMap(b =>
        if b then t
        else f
      )

    def choiceNViaFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
      p.flatMap(p =>
          choices(p % choices.length))

    def join[A](ppa: Par[Par[A]]): Par[A] =
      es => cb => ppa(es)(pa => eval(es)(pa(es)(cb)))

    def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
      a.flatMap(identity)

    def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
      join(p.map(f))
