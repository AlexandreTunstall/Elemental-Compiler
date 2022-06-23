; ModuleID = '<string>'
source_filename = "test/Golden/NestedBranch3.elem"

declare i2 @dothing() local_unnamed_addr

define i1 @main() local_unnamed_addr {
  %1 = tail call i2 @dothing()
  %2 = icmp eq i2 %1, 0
  br i1 %2, label %__elem_0.exit, label %__elem_0.exit.sink.split

__elem_0.exit.sink.split:                         ; preds = %0
  %3 = tail call i2 @dothing()
  %4 = icmp ne i2 %3, 0
  br label %__elem_0.exit

__elem_0.exit:                                    ; preds = %0, %__elem_0.exit.sink.split
  %5 = phi i1 [ %4, %__elem_0.exit.sink.split ], [ false, %0 ]
  ret i1 %5
}
