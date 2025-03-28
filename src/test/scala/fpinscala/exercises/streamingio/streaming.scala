package fpinscala.exercises.streamingio

//> using test.dep "org.scalameta::munit::1.0.2"

import fpinscala.exercises.streamingio.SimplePulls
import munit.FunSuite

class SimplePullsTest extends FunSuite {
  test("Pull.fromListViaUnfold should convert list to Pull and back to original list") {
    // Test data
    val inputList = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    
    // Create Pull from list
    val pull: SimplePulls.Pull[Int, Unit] = SimplePulls.Pull.fromListViaUnfold(inputList)
    
    // Convert Pull back to list
    val resultList = pull.toList
    
    // Assert that the result matches the input
    assertEquals(resultList, inputList, "The list reconstructed from Pull should match the original list")
  }

  test("Pull.fromListViaUnfold with empty list should produce empty list") {
    // Test with an empty list
    val emptyList = List.empty[Int]
    val pull = SimplePulls.Pull.fromListViaUnfold(emptyList)
    val result = pull.toList
    
    // Assert the result is empty
    assertEquals(result, emptyList, "An empty list should produce an empty Pull and back")
  }

  test("Pull.fromListViaUnfold with single element should work correctly") {
    // Test with a single-element list
    val singleElementList = List(42)
    val pull = SimplePulls.Pull.fromListViaUnfold(singleElementList)
    val result = pull.toList
    
    // Assert the result matches
    assertEquals(result, singleElementList, "A single-element list should round-trip correctly")
  }

  test("Pull.takeWhile should work correctly") {
    val elements = List(1,3,5,7,9,10)
    val pull = SimplePulls.Pull.fromList(elements).takeWhile(_ % 2 == 1)
    val result = pull.toList
    val expected = List(1,3,5,7,9)

    // Assert the result matches
    assertEquals(result, expected, "A list with initial matching elements")
  }

  test("Pull.dropWhile should work correctly") {
    val elements = List(1,3,5,7,9,10,12,13)
    val pull = SimplePulls.Pull.fromList(elements).dropWhile(_ % 2 == 1)
    val result = pull.fold(List.newBuilder[Int])((bldr, o) => bldr += o)
    val expected = List(10,12,13)

    // Fold returns a tuple with the result (a new pull with the reamining elements)
    // and the output of the dropWhile which is empty
    assertEquals(result(0).toList, expected, "A list of the remaining elements")
  }

  test("Pull.filter should work correctly") {
    val elements = List(1,2,3,5,4,7,9,8,10)
    val pull = SimplePulls.Pull.fromList(elements).filter(_ % 2 == 0)
    val result = pull.toList
    val expected = List(2,4,8,10)

    // Assert the result matches
    assertEquals(result, expected, "Matching elements are kept")
  }
}
