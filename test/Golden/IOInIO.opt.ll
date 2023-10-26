; ModuleID = '<string>'
source_filename = "test/Golden/IOInIO.elem"

declare i1 @getbit() local_unnamed_addr

define i1 @main() local_unnamed_addr {
  %1 = tail call i1 @getbit()
  ret i1 %1
}
