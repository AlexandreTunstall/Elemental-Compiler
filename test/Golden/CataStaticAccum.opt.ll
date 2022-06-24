; ModuleID = '<string>'
source_filename = "test/Golden/CataStaticAccum.elem"

declare i1 @dothing() local_unnamed_addr

define i1 @main() local_unnamed_addr {
  %1 = tail call i1 @dothing()
  %2 = tail call i1 @dothing()
  %3 = tail call i1 @dothing()
  %not.1 = xor i1 %3, true
  br i1 %1, label %4, label %__elem_2.exit

4:                                                ; preds = %0
  %spec.select = select i1 %2, i1 %3, i1 %not.1
  ret i1 %spec.select

__elem_2.exit:                                    ; preds = %0
  %spec.select2 = select i1 %2, i1 %not.1, i1 %3
  ret i1 %spec.select2
}
