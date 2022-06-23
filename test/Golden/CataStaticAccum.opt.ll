; ModuleID = '<string>'
source_filename = "test/Golden/CataStaticAccum.elem"

declare i1 @dothing() local_unnamed_addr

define i1 @main() local_unnamed_addr {
__elem_2.exit:
  %0 = tail call i1 @dothing()
  %1 = tail call i1 @dothing()
  %2 = tail call i1 @dothing()
  %spec.select = xor i1 %0, %1
  %3 = xor i1 %2, %spec.select
  ret i1 %3
}
