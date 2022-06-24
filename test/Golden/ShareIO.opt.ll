; ModuleID = '<string>'
source_filename = "test/Golden/ShareIO.elem"

declare void @putbit(i1) local_unnamed_addr

declare i1 @getbit() local_unnamed_addr

define void @main1() local_unnamed_addr {
__elem_2.exit:
  %0 = tail call i1 @getbit()
  %1 = tail call i1 @getbit()
  %.sink = xor i1 %0, %1
  tail call void @putbit(i1 %.sink)
  ret void
}

define void @main2() local_unnamed_addr {
__elem_2.exit:
  %0 = tail call i1 @getbit()
  %1 = tail call i1 @getbit()
  %.sink = xor i1 %0, %1
  tail call void @putbit(i1 %.sink)
  ret void
}
