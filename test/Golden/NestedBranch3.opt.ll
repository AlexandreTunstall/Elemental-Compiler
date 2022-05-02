; ModuleID = '<string>'
source_filename = "test/Golden/NestedBranch3.elem"

declare i2 @dothing() local_unnamed_addr

define private fastcc void @"411"() unnamed_addr {
  %1 = tail call i2 @dothing()
  ret void
}

define i1 @main() local_unnamed_addr {
  %1 = tail call i2 @dothing()
  %2 = icmp eq i2 %1, 0
  br i1 %2, label %3, label %.sink.split

.sink.split:                                      ; preds = %0
  tail call fastcc void @"411"()
  br label %3

3:                                                ; preds = %0, %.sink.split
  ret i1 false
}
