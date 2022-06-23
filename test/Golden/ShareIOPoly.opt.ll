; ModuleID = '<string>'
source_filename = "test/Golden/ShareIOPoly.elem"

declare void @dothing() local_unnamed_addr

define i1 @main1(i1 returned) local_unnamed_addr {
  tail call void @dothing()
  ret i1 %0
}

define i2 @main2(i2) local_unnamed_addr {
__elem_1.exit:
  %1 = icmp sgt i2 %0, -1
  %2 = and i2 %0, 1
  tail call void @dothing()
  %3 = or i2 %0, -2
  %4 = select i1 %1, i2 %2, i2 %3
  ret i2 %4
}
