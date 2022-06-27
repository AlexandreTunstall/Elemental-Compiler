; ModuleID = '<string>'
source_filename = "test/Golden/CataStaticAccum.elem"

declare i1 @dothing() local_unnamed_addr

define i1 @main() local_unnamed_addr {
  %1 = tail call i1 @dothing()
  %2 = tail call i1 @dothing()
  %3 = tail call i1 @dothing()
  %.v.i.i.i.i.i.i.i = xor i1 %1, %2
  %4 = xor i1 %.v.i.i.i.i.i.i.i, %3
  ret i1 %4
}
