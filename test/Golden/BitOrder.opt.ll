; ModuleID = '<string>'
source_filename = "test/Golden/BitOrder.elem"

; Function Attrs: norecurse nounwind readnone
define i2 @main(i2) local_unnamed_addr #0 {
__elem_0.exit:
  %1 = xor i2 %0, 1
  ret i2 %1
}

attributes #0 = { norecurse nounwind readnone }
