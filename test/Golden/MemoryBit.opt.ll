; ModuleID = '<string>'
source_filename = "test/Golden/MemoryBit.elem"

; Function Attrs: nofree norecurse nounwind
define void @main() local_unnamed_addr #0 {
  %1 = load volatile i1, i1* inttoptr (i14 -8192 to i1*), align 8192
  br i1 %1, label %2, label %3

2:                                                ; preds = %0
  store volatile i1 false, i1* inttoptr (i14 -8192 to i1*), align 8192
  ret void

3:                                                ; preds = %0
  store volatile i1 true, i1* inttoptr (i14 -8192 to i1*), align 8192
  ret void
}

attributes #0 = { nofree norecurse nounwind }
