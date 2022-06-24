; ModuleID = '<string>'
source_filename = "test/Golden/CallOrder.elem"

declare void @dothing(i2, i1) local_unnamed_addr

define void @main(i2, i1) local_unnamed_addr {
  %3 = icmp sgt i2 %0, -1
  %4 = and i2 %0, 1
  %5 = icmp eq i2 %4, 0
  br i1 %3, label %11, label %6

6:                                                ; preds = %2
  br i1 %5, label %9, label %7

7:                                                ; preds = %6
  br i1 %1, label %8, label %codeRepl.i.i

8:                                                ; preds = %7
  tail call void @dothing(i2 -1, i1 true)
  br label %__elem_0.exit

codeRepl.i.i:                                     ; preds = %7
  tail call void @dothing(i2 -1, i1 false)
  br label %__elem_0.exit

9:                                                ; preds = %6
  br i1 %1, label %10, label %codeRepl.i1.i

10:                                               ; preds = %9
  tail call void @dothing(i2 -2, i1 true)
  br label %__elem_0.exit

codeRepl.i1.i:                                    ; preds = %9
  tail call void @dothing(i2 -2, i1 false)
  br label %__elem_0.exit

__elem_0.exit:                                    ; preds = %15, %codeRepl.i1.i3, %13, %codeRepl.i.i1, %10, %codeRepl.i1.i, %8, %codeRepl.i.i
  ret void

11:                                               ; preds = %2
  br i1 %5, label %14, label %12

12:                                               ; preds = %11
  br i1 %1, label %13, label %codeRepl.i.i1

13:                                               ; preds = %12
  tail call void @dothing(i2 1, i1 true)
  br label %__elem_0.exit

codeRepl.i.i1:                                    ; preds = %12
  tail call void @dothing(i2 1, i1 false)
  br label %__elem_0.exit

14:                                               ; preds = %11
  br i1 %1, label %15, label %codeRepl.i1.i3

15:                                               ; preds = %14
  tail call void @dothing(i2 0, i1 true)
  br label %__elem_0.exit

codeRepl.i1.i3:                                   ; preds = %14
  tail call void @dothing(i2 0, i1 false)
  br label %__elem_0.exit
}
