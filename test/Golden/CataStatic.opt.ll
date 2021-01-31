; ModuleID = '<string>'
source_filename = "test/Golden/CataStatic.elem"

declare void @dothing() local_unnamed_addr

define void @main() local_unnamed_addr {
  tail call void @dothing()
  tail call void @dothing()
  tail call void @dothing()
  ret void
}
