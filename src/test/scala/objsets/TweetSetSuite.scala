package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)


    val gizmodo = new Empty
    val gizmodo2 = gizmodo.incl(new Tweet("gizmodo", "These new Apple patents give a sneak peek at what future iPhone cameras might have in store. http://t.co/0YT9rjxp", 49))
    val gizmodo3 = gizmodo2.incl(new Tweet("gizmodo", "Ever wonder why the sky is dark at night? Here's your answer. http://t.co/eTKxkcaE", 86 ))

    val engadget = new Empty
    val engadget2 = engadget.incl(new Tweet("engadget", "Sony reveals Apple Japan prices for Windows 8 VAIO machines -  http://t.co/FRCu2XVb", 18 ))
    val engadget3 = engadget2.incl(new Tweet("engadget", "HP announces Apple the ElitePad 900, a business-friendly Windows 8 tablet arriving in January -  http://t.co/RjSj2cms", 33))

    val global = engadget3.union(gizmodo3)

  }

  def size(set: TweetSet): Int = {
    if (set.isEmpty) 0
    else 1 + size(set.tail)
  }

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }



  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

   test("union: set4c and set4d") {
     new TestSets {
       assert(size(set4c.union(set4d)) === 4)
     }
  }

   test("union: with empty set (1)") {
     new TestSets {
       assert(size(set5.union(set1)) === 4)
     }
   }

   test("union: with empty set (2)") {
     new TestSets {
       assert(size(set1.union(set5)) === 4)
     }
   }

   test("ascending: set5") {
    new TestSets {
      val trends = set5.ascendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user === "c")
    }
  }

   test("ascending: setEmpty") {
     new TestSets {
       val trends = set1.ascendingByRetweet
       assert(trends.isEmpty)

     }
  }

  test("TweetReader union test") {
    new TestSets {

      assert(size(gizmodo3.union(engadget3)) === 4)

    }
  }

  test("filter: text Apple on gizmodo") {
    new TestSets {
      assert(size(gizmodo3.filter(tw => tw.text contains "Apple")) === 1)
    }
  }

  test("filter: text Apple on union") {
    new TestSets {

      assert(size(global.filter(tw => tw.text contains "Apple")) === 3)
    }
  }

}
