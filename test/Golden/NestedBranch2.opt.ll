; ModuleID = '<string>'
source_filename = "test/Golden/NestedBranch2.elem"

declare void @dothing(i4) local_unnamed_addr

define private fastcc void @"6391"(i4) unnamed_addr {
  tail call void @dothing(i4 %0)
  tail call void @dothing(i4 %0)
  ret void
}

define void @main(i4) local_unnamed_addr {
  %2 = icmp eq i4 %0, 0
  br i1 %2, label %3, label %.sink.split

.sink.split:                                      ; preds = %1
  tail call fastcc void @"6391"(i4 %0)
  br label %3

3:                                                ; preds = %1, %.sink.split
  ret void
}
