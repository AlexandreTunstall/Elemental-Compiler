; ModuleID = '<string>'
source_filename = "test/Golden/BitOrder.elem"

; Function Attrs: norecurse nounwind readnone
define i2 @main(i2) local_unnamed_addr #0 {
  %2 = icmp sgt i2 %0, -1
  %3 = and i2 %0, 1
  br i1 %2, label %6, label %4

4:                                                ; preds = %1
  %5 = xor i2 %3, -1
  ret i2 %5

6:                                                ; preds = %1
  %7 = xor i2 %3, 1
  ret i2 %7
}

attributes #0 = { norecurse nounwind readnone }
