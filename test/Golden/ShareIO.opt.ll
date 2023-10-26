; ModuleID = '<string>'
source_filename = "test/Golden/ShareIO.elem"

declare void @putbit(i1) local_unnamed_addr

declare i1 @getbit() local_unnamed_addr

define void @main1() local_unnamed_addr {
  %1 = tail call i1 @getbit()
  %2 = tail call i1 @getbit()
  %3 = xor i1 %1, %2
  tail call void @putbit(i1 %3)
  ret void
}

define void @main2() local_unnamed_addr {
  %1 = tail call i1 @getbit()
  %2 = tail call i1 @getbit()
  %3 = xor i1 %1, %2
  tail call void @putbit(i1 %3)
  ret void
}
