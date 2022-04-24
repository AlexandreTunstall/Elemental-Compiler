; ModuleID = '<string>'
source_filename = "test/Golden/NestedBranch.elem"

declare void @dothing() local_unnamed_addr

define void @main(i8) local_unnamed_addr {
  %2 = icmp eq i8 %0, 0
  br i1 %2, label %3, label %4

3:                                                ; preds = %1
  tail call void @dothing()
  br label %4

4:                                                ; preds = %3, %1
  ret void
}
