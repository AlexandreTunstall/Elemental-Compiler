; ModuleID = '<string>'
source_filename = "test/Golden/CallProduct.elem"

declare void @dothing(i2, i1) local_unnamed_addr

define void @main() local_unnamed_addr {
  tail call void @dothing(i2 0, i1 false)
  ret void
}
