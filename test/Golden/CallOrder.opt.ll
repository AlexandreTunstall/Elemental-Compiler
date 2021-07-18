; ModuleID = '<string>'
source_filename = "test/Golden/CallOrder.elem"

declare void @dothing(i2, i1) local_unnamed_addr

define void @main(i2, i1) local_unnamed_addr {
  %3 = and i2 %0, 1
  %4 = icmp eq i2 %3, 0
  %5 = icmp sgt i2 %0, -1
  br i1 %1, label %6, label %13

6:                                                ; preds = %2
  br i1 %4, label %10, label %7

7:                                                ; preds = %6
  br i1 %5, label %9, label %8

8:                                                ; preds = %7
  tail call void @dothing(i2 -1, i1 true)
  br label %20

9:                                                ; preds = %7
  tail call void @dothing(i2 1, i1 true)
  br label %20

10:                                               ; preds = %6
  br i1 %5, label %12, label %11

11:                                               ; preds = %10
  tail call void @dothing(i2 -2, i1 true)
  br label %20

12:                                               ; preds = %10
  tail call void @dothing(i2 0, i1 true)
  br label %20

13:                                               ; preds = %2
  br i1 %4, label %17, label %14

14:                                               ; preds = %13
  br i1 %5, label %16, label %15

15:                                               ; preds = %14
  tail call void @dothing(i2 -1, i1 false)
  br label %20

16:                                               ; preds = %14
  tail call void @dothing(i2 1, i1 false)
  br label %20

17:                                               ; preds = %13
  br i1 %5, label %19, label %18

18:                                               ; preds = %17
  tail call void @dothing(i2 -2, i1 false)
  br label %20

19:                                               ; preds = %17
  tail call void @dothing(i2 0, i1 false)
  br label %20

20:                                               ; preds = %16, %15, %19, %18, %9, %8, %12, %11
  ret void
}
