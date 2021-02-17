; ModuleID = '<string>'
source_filename = "test/Golden/MemoryBit.elem"

; Function Attrs: nofree norecurse nounwind
define void @main() local_unnamed_addr #0 {
  %1 = load volatile i1, i1* inttoptr (i128 8192 to i1*), align 8192
  %not. = xor i1 %1, true
  store volatile i1 %not., i1* inttoptr (i128 8192 to i1*), align 8192
  ret void
}

attributes #0 = { nofree norecurse nounwind }
