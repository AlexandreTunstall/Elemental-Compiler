; ModuleID = '<string>'
source_filename = "test/Golden/NestedBranch2.elem"

declare void @dothing(i1) local_unnamed_addr

define void @main(i1) local_unnamed_addr {
  br i1 %0, label %2, label %3

2:                                                ; preds = %1
  tail call void @dothing(i1 true)
  ret void

3:                                                ; preds = %1
  ret void
}