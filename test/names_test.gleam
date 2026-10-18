import arc/bytecode/key.{Named, Private}
import arc/bytecode/names
import gleam/list

pub fn every_ready_made_key_has_the_number_its_name_gives_test() {
  list.each(names.all, fn(k) {
    assert key.named(key.to_text(k)) == k
  })
}

pub fn the_number_depends_on_the_text_alone_test() {
  assert key.named("alpha") == key.named("al" <> "pha")
  let assert Named(id: a, ..) = key.named("alpha")
  let assert Named(id: b, ..) = key.named("alphb")
  let assert Named(id: c, ..) = key.named("")
  let assert Named(id: d, ..) = key.named("\u{0}")
  assert a != b && c != d
}

// so that one map can hold both kinds
pub fn a_private_name_never_shares_a_number_with_a_name_test() {
  let assert Named(id: plain, ..) = key.named("x")
  let assert Private(id: hidden, ..) = key.private("x")
  assert plain % 2 == 0
  assert hidden == plain + 1
}

// a small integer on a 64-bit vm, which is what makes it quick to look up
pub fn numbers_fit_in_59_bits_test() {
  list.each(
    ["", "a", "length", "aVeryLongPropertyNameIndeed_0123456789"],
    fn(s) {
      let assert Private(id:, ..) = key.private(s)
      assert id >= 0 && id < 576_460_752_303_423_488
    },
  )
}
