; ModuleID = '<string>'
source_filename = "test/Golden/ShareIOPoly.elem"

declare void @dothing() local_unnamed_addr

define i1 @main1(i1 returned) local_unnamed_addr {
  tail call void @dothing()
  ret i1 %0
}

define i2 @main2(i2 returned) local_unnamed_addr {
  tail call void @dothing()
  ret i2 %0
}
