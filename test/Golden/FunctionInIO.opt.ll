; ModuleID = '<string>'
source_filename = "test/Golden/FunctionInIO.elem"

declare i1 @getbit() local_unnamed_addr

; Function Attrs: norecurse nounwind readnone
define i1 @main(i1) local_unnamed_addr #0 {
  %not. = xor i1 %0, true
  ret i1 %not.
}

define i1 @main2() local_unnamed_addr {
  %1 = tail call i1 @getbit()
  %2 = tail call i1 @getbit()
  %3 = xor i1 %1, %2
  ret i1 %3
}

attributes #0 = { norecurse nounwind readnone }
