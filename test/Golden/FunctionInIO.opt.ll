; ModuleID = '<string>'
source_filename = "test/Golden/FunctionInIO.elem"

declare i1 @getbit() local_unnamed_addr

; Function Attrs: norecurse nounwind readnone
define i1 @main(i1) local_unnamed_addr #0 {
  %not. = xor i1 %0, true
  ret i1 %not.
}

define i1 @main2() local_unnamed_addr {
__elem_0.exit:
  %0 = tail call i1 @getbit()
  %1 = tail call i1 @getbit()
  %2 = xor i1 %0, %1
  ret i1 %2
}

attributes #0 = { norecurse nounwind readnone }
