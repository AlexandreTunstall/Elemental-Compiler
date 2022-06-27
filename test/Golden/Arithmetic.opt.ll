; ModuleID = '<string>'
source_filename = "test/Golden/Arithmetic.elem"

; Function Attrs: norecurse nounwind readnone
define i1 @expsign(i2, i2) local_unnamed_addr #0 {
  %3 = and i2 %0, 1
  %4 = icmp ne i2 %3, 0
  %5 = icmp eq i2 %1, 0
  %6 = or i1 %5, %4
  ret i1 %6
}

attributes #0 = { norecurse nounwind readnone }
