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
}
