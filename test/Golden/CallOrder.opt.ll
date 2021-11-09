; ModuleID = '<string>'
source_filename = "test/Golden/CallOrder.elem"

declare void @dothing(i2, i1) local_unnamed_addr

define void @main(i2, i1) local_unnamed_addr {
  tail call void @dothing(i2 %0, i1 %1)
  ret void
}
