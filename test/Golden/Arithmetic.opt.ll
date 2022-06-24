; ModuleID = '<string>'
source_filename = "test/Golden/Arithmetic.elem"

; Function Attrs: norecurse nounwind readnone
define i1 @expsign(i2, i2) local_unnamed_addr #0 {
__elem_0.exit:
  %2 = and i2 %0, 1
  %3 = icmp ne i2 %2, 0
  %4 = icmp eq i2 %1, 0
  %spec.select = or i1 %4, %3
  ret i1 %spec.select
}

attributes #0 = { norecurse nounwind readnone }
