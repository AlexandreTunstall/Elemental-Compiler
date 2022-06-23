; ModuleID = '<string>'
source_filename = "test/Golden/BranchIO.elem"

declare void @t() local_unnamed_addr

declare void @f() local_unnamed_addr

define void @main(i1) local_unnamed_addr {
  br i1 %0, label %2, label %3

2:                                                ; preds = %1
  tail call void @t()
  ret void

3:                                                ; preds = %1
  tail call void @f()
  ret void
}
