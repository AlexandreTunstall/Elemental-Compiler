; ModuleID = '<string>'
source_filename = "test/Golden/NestedBranch.elem"

declare void @dothing() local_unnamed_addr

define void @main(i8) local_unnamed_addr {
  %2 = icmp eq i8 %0, 0
  br i1 %2, label %3, label %__elem_14.exit

3:                                                ; preds = %1
  tail call void @dothing()
  br label %__elem_14.exit

__elem_14.exit:                                   ; preds = %1, %3
  ret void
}
