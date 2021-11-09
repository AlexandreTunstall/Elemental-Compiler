; ModuleID = '<string>'
source_filename = "test/Golden/EchoChar.elem"

declare void @putchar(i8) local_unnamed_addr

; Function Attrs: nofree nounwind
declare i8 @getchar() local_unnamed_addr #0

define void @main() local_unnamed_addr {
  %1 = tail call i8 @getchar()
  tail call void @putchar(i8 %1)
  ret void
}

attributes #0 = { nofree nounwind }
