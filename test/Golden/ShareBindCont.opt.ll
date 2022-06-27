; ModuleID = '<string>'
source_filename = "test/Golden/ShareBindCont.elem"

declare i1 @getbit() local_unnamed_addr

define i1 @main1() local_unnamed_addr {
  %1 = tail call i1 @getbit()
  ret i1 %1
}

define i1 @main2() local_unnamed_addr {
  %1 = tail call i1 @getbit()
  %not..i.i = xor i1 %1, true
  ret i1 %not..i.i
}
