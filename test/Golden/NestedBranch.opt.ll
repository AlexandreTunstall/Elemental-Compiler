; ModuleID = '<string>'
source_filename = "test/Golden/NestedBranch.elem"

declare void @dothing() local_unnamed_addr

define void @main(i8) local_unnamed_addr {
  %2 = icmp eq i8 %0, 0
  br i1 %2, label %codeRepl.i, label %3

3:                                                ; preds = %codeRepl.i, %1
  ret void

codeRepl.i:                                       ; preds = %1
  tail call void @dothing()
  br label %3
}
