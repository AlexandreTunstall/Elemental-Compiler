; ModuleID = '<string>'
source_filename = "test/Golden/CallOrder.elem"

declare void @dothing(i2, i1) local_unnamed_addr

define void @main(i2, i1) local_unnamed_addr {
  %3 = icmp sgt i2 %0, -1
  %4 = and i2 %0, 1
  %5 = icmp eq i2 %4, 0
  br i1 %3, label %12, label %6

6:                                                ; preds = %2
  br i1 %5, label %9, label %codeRepl.i

codeRepl.i:                                       ; preds = %6
  br i1 %1, label %7, label %8

7:                                                ; preds = %codeRepl.i
  tail call void @dothing(i2 -1, i1 true)
  br label %__elem_0.1.exit

8:                                                ; preds = %codeRepl.i
  tail call void @dothing(i2 -1, i1 false)
  br label %__elem_0.1.exit

9:                                                ; preds = %6
  br i1 %1, label %10, label %11

10:                                               ; preds = %9
  tail call void @dothing(i2 -2, i1 true)
  br label %__elem_0.1.exit

11:                                               ; preds = %9
  tail call void @dothing(i2 -2, i1 false)
  br label %__elem_0.1.exit

__elem_0.1.exit:                                  ; preds = %16, %17, %14, %13, %10, %11, %8, %7
  ret void

12:                                               ; preds = %2
  br i1 %5, label %15, label %codeRepl.i1

codeRepl.i1:                                      ; preds = %12
  br i1 %1, label %13, label %14

13:                                               ; preds = %codeRepl.i1
  tail call void @dothing(i2 1, i1 true)
  br label %__elem_0.1.exit

14:                                               ; preds = %codeRepl.i1
  tail call void @dothing(i2 1, i1 false)
  br label %__elem_0.1.exit

15:                                               ; preds = %12
  br i1 %1, label %16, label %17

16:                                               ; preds = %15
  tail call void @dothing(i2 0, i1 true)
  br label %__elem_0.1.exit

17:                                               ; preds = %15
  tail call void @dothing(i2 0, i1 false)
  br label %__elem_0.1.exit
}
