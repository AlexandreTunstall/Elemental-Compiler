; ModuleID = '<string>'
source_filename = "test/Golden/SimpleArgs.elem"

; Function Attrs: norecurse nounwind readnone
define i1 @snd(i1, i1) local_unnamed_addr #0 {
  ret i1 %1
}

; Function Attrs: norecurse nounwind readnone
define i1 @main(i1) local_unnamed_addr #0 {
  ret i1 %0
}

attributes #0 = { norecurse nounwind readnone }
