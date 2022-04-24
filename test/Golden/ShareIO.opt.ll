; ModuleID = '<string>'
source_filename = "test/Golden/ShareIO.elem"

declare void @putbit(i1) local_unnamed_addr

declare i1 @getbit() local_unnamed_addr

define private fastcc { i1, i1 } @"2485"() unnamed_addr {
  %1 = tail call i1 @getbit()
  %2 = tail call i1 @getbit()
  %3 = insertvalue { i1, i1 } zeroinitializer, i1 %2, 1
  %4 = insertvalue { i1, i1 } %3, i1 %1, 0
  ret { i1, i1 } %4
}

define void @main1() local_unnamed_addr {
  %1 = tail call fastcc { i1, i1 } @"2485"()
  %2 = extractvalue { i1, i1 } %1, 1
  %3 = extractvalue { i1, i1 } %1, 0
  %4 = xor i1 %3, %2
  tail call void @putbit(i1 %4)
  ret void
}

define void @main2() local_unnamed_addr {
  %1 = tail call fastcc { i1, i1 } @"2485"()
  %2 = extractvalue { i1, i1 } %1, 1
  %3 = extractvalue { i1, i1 } %1, 0
  %4 = xor i1 %3, %2
  tail call void @putbit(i1 %4)
  ret void
}
