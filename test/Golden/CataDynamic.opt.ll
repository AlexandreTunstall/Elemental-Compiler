; ModuleID = '<string>'
source_filename = "test/Golden/CataDynamic.elem"

declare void @dothing() local_unnamed_addr

define void @main(i1) local_unnamed_addr {
  tail call void @dothing()
  tail call void @dothing()
  br i1 %0, label %3, label %2

2:                                                ; preds = %1
  tail call void @dothing()
  tail call void @dothing()
  br label %3

3:                                                ; preds = %1, %2
  ret void
}
