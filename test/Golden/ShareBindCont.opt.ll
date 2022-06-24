; ModuleID = '<string>'
source_filename = "test/Golden/ShareBindCont.elem"

declare i1 @getbit() local_unnamed_addr

define i1 @main1() local_unnamed_addr {
__elem_0.exit:
  %0 = tail call i1 @getbit()
  ret i1 %0
}

define i1 @main2() local_unnamed_addr {
__elem_0.exit:
  %0 = tail call i1 @getbit()
  %not. = xor i1 %0, true
  ret i1 %not.
}
