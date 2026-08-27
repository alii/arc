import arc/repl/examples
import gleam/option.{None, Some}

// examples.get is 1-based
pub fn get_zero_is_out_of_range_test() {
  assert examples.get(0) == None
}

pub fn get_negative_is_out_of_range_test() {
  assert examples.get(-1) == None
}

pub fn get_one_is_the_first_example_test() {
  assert case examples.get(1) {
    Some(ex) -> ex.title == "Closures"
    None -> False
  }
}

pub fn get_past_the_end_is_out_of_range_test() {
  assert examples.get(9999) == None
}
