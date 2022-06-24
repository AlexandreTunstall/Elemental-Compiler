; ModuleID = '<string>'
source_filename = "test/Golden/EchoChar.elem"

declare void @putchar(i8) local_unnamed_addr

; Function Attrs: nofree nounwind
declare i8 @getchar() local_unnamed_addr #0

define void @main() local_unnamed_addr {
  %1 = tail call i8 @getchar()
  %2 = icmp sgt i8 %1, -1
  %3 = and i8 %1, 64
  %4 = icmp eq i8 %3, 0
  %5 = and i8 %1, 32
  %6 = icmp eq i8 %5, 0
  %7 = and i8 %1, 16
  %8 = icmp eq i8 %7, 0
  br i1 %2, label %231, label %9

9:                                                ; preds = %0
  br i1 %4, label %122, label %codeRepl.i

codeRepl.i:                                       ; preds = %9
  br i1 %6, label %67, label %10

10:                                               ; preds = %codeRepl.i
  br i1 %8, label %39, label %codeRepl.i.i

codeRepl.i.i:                                     ; preds = %10
  %11 = and i8 %1, 8
  %12 = icmp eq i8 %11, 0
  %13 = and i8 %1, 4
  %14 = icmp eq i8 %13, 0
  %15 = and i8 %1, 2
  %16 = icmp eq i8 %15, 0
  %17 = and i8 %1, 1
  %18 = trunc i8 %17 to i3
  %19 = icmp eq i8 %17, 0
  %..i7.i = select i1 %19, i3 2, i3 3
  %.sink.i8.i = select i1 %16, i3 %18, i3 %..i7.i
  br i1 %12, label %27, label %20

20:                                               ; preds = %codeRepl.i.i
  %21 = zext i3 %.sink.i8.i to i4
  br i1 %14, label %24, label %codeRepl.i.i12

codeRepl.i.i12:                                   ; preds = %20
  %22 = or i4 %21, -4
  %23 = zext i4 %22 to i5
  br label %__elem_3.exit

24:                                               ; preds = %20
  %25 = or i4 %21, -8
  %26 = zext i4 %25 to i5
  br label %__elem_3.exit

27:                                               ; preds = %codeRepl.i.i
  br i1 %14, label %30, label %codeRepl.i1.i13

codeRepl.i1.i13:                                  ; preds = %27
  %28 = or i3 %.sink.i8.i, -4
  %29 = zext i3 %28 to i5
  br label %__elem_3.exit

30:                                               ; preds = %27
  %31 = zext i3 %.sink.i8.i to i5
  br label %__elem_3.exit

__elem_3.exit:                                    ; preds = %codeRepl.i.i12, %24, %codeRepl.i1.i13, %30
  %.sink17.i = phi i5 [ %29, %codeRepl.i1.i13 ], [ %31, %30 ], [ %26, %24 ], [ %23, %codeRepl.i.i12 ]
  %32 = or i5 %.sink17.i, -16
  %33 = zext i5 %32 to i6
  %34 = or i6 -32, %33
  %35 = zext i6 %34 to i7
  %36 = or i7 -64, %35
  %37 = zext i7 %36 to i8
  %38 = or i8 -128, %37
  tail call void @putchar(i8 %38)
  br label %__elem_0.4.exit

39:                                               ; preds = %10
  %40 = and i8 %1, 8
  %41 = icmp eq i8 %40, 0
  %42 = and i8 %1, 4
  %43 = icmp eq i8 %42, 0
  %44 = and i8 %1, 2
  %45 = icmp eq i8 %44, 0
  %46 = and i8 %1, 1
  %47 = trunc i8 %46 to i3
  %48 = icmp eq i8 %46, 0
  %..i7.i14 = select i1 %48, i3 2, i3 3
  %.sink.i8.i15 = select i1 %45, i3 %47, i3 %..i7.i14
  br i1 %41, label %56, label %49

49:                                               ; preds = %39
  %50 = zext i3 %.sink.i8.i15 to i4
  br i1 %43, label %53, label %codeRepl.i.i16

codeRepl.i.i16:                                   ; preds = %49
  %51 = or i4 %50, -4
  %52 = zext i4 %51 to i5
  br label %__elem_3.exit19

53:                                               ; preds = %49
  %54 = or i4 %50, -8
  %55 = zext i4 %54 to i5
  br label %__elem_3.exit19

56:                                               ; preds = %39
  br i1 %43, label %59, label %codeRepl.i1.i18

codeRepl.i1.i18:                                  ; preds = %56
  %57 = or i3 %.sink.i8.i15, -4
  %58 = zext i3 %57 to i5
  br label %__elem_3.exit19

59:                                               ; preds = %56
  %60 = zext i3 %.sink.i8.i15 to i5
  br label %__elem_3.exit19

__elem_3.exit19:                                  ; preds = %codeRepl.i.i16, %53, %codeRepl.i1.i18, %59
  %.sink17.i17 = phi i5 [ %58, %codeRepl.i1.i18 ], [ %60, %59 ], [ %55, %53 ], [ %52, %codeRepl.i.i16 ]
  %61 = zext i5 %.sink17.i17 to i6
  %62 = or i6 -32, %61
  %63 = zext i6 %62 to i7
  %64 = or i7 -64, %63
  %65 = zext i7 %64 to i8
  %66 = or i8 -128, %65
  tail call void @putchar(i8 %66)
  br label %__elem_0.4.exit

67:                                               ; preds = %codeRepl.i
  br i1 %8, label %95, label %codeRepl.i1.i

codeRepl.i1.i:                                    ; preds = %67
  %68 = and i8 %1, 8
  %69 = icmp eq i8 %68, 0
  %70 = and i8 %1, 4
  %71 = icmp eq i8 %70, 0
  %72 = and i8 %1, 2
  %73 = icmp eq i8 %72, 0
  %74 = and i8 %1, 1
  %75 = trunc i8 %74 to i3
  %76 = icmp eq i8 %74, 0
  %..i7.i20 = select i1 %76, i3 2, i3 3
  %.sink.i8.i21 = select i1 %73, i3 %75, i3 %..i7.i20
  br i1 %69, label %84, label %77

77:                                               ; preds = %codeRepl.i1.i
  %78 = zext i3 %.sink.i8.i21 to i4
  br i1 %71, label %81, label %codeRepl.i.i22

codeRepl.i.i22:                                   ; preds = %77
  %79 = or i4 %78, -4
  %80 = zext i4 %79 to i5
  br label %__elem_3.exit25

81:                                               ; preds = %77
  %82 = or i4 %78, -8
  %83 = zext i4 %82 to i5
  br label %__elem_3.exit25

84:                                               ; preds = %codeRepl.i1.i
  br i1 %71, label %87, label %codeRepl.i1.i24

codeRepl.i1.i24:                                  ; preds = %84
  %85 = or i3 %.sink.i8.i21, -4
  %86 = zext i3 %85 to i5
  br label %__elem_3.exit25

87:                                               ; preds = %84
  %88 = zext i3 %.sink.i8.i21 to i5
  br label %__elem_3.exit25

__elem_3.exit25:                                  ; preds = %codeRepl.i.i22, %81, %codeRepl.i1.i24, %87
  %.sink17.i23 = phi i5 [ %86, %codeRepl.i1.i24 ], [ %88, %87 ], [ %83, %81 ], [ %80, %codeRepl.i.i22 ]
  %89 = or i5 %.sink17.i23, -16
  %90 = zext i5 %89 to i6
  %91 = zext i6 %90 to i7
  %92 = or i7 -64, %91
  %93 = zext i7 %92 to i8
  %94 = or i8 -128, %93
  tail call void @putchar(i8 %94)
  br label %__elem_0.4.exit

95:                                               ; preds = %67
  %96 = and i8 %1, 8
  %97 = icmp eq i8 %96, 0
  %98 = and i8 %1, 4
  %99 = icmp eq i8 %98, 0
  %100 = and i8 %1, 2
  %101 = icmp eq i8 %100, 0
  %102 = and i8 %1, 1
  %103 = trunc i8 %102 to i3
  %104 = icmp eq i8 %102, 0
  %..i7.i26 = select i1 %104, i3 2, i3 3
  %.sink.i8.i27 = select i1 %101, i3 %103, i3 %..i7.i26
  br i1 %97, label %112, label %105

105:                                              ; preds = %95
  %106 = zext i3 %.sink.i8.i27 to i4
  br i1 %99, label %109, label %codeRepl.i.i28

codeRepl.i.i28:                                   ; preds = %105
  %107 = or i4 %106, -4
  %108 = zext i4 %107 to i5
  br label %__elem_3.exit31

109:                                              ; preds = %105
  %110 = or i4 %106, -8
  %111 = zext i4 %110 to i5
  br label %__elem_3.exit31

112:                                              ; preds = %95
  br i1 %99, label %115, label %codeRepl.i1.i30

codeRepl.i1.i30:                                  ; preds = %112
  %113 = or i3 %.sink.i8.i27, -4
  %114 = zext i3 %113 to i5
  br label %__elem_3.exit31

115:                                              ; preds = %112
  %116 = zext i3 %.sink.i8.i27 to i5
  br label %__elem_3.exit31

__elem_3.exit31:                                  ; preds = %codeRepl.i.i28, %109, %codeRepl.i1.i30, %115
  %.sink17.i29 = phi i5 [ %114, %codeRepl.i1.i30 ], [ %116, %115 ], [ %111, %109 ], [ %108, %codeRepl.i.i28 ]
  %117 = zext i5 %.sink17.i29 to i6
  %118 = zext i6 %117 to i7
  %119 = or i7 -64, %118
  %120 = zext i7 %119 to i8
  %121 = or i8 -128, %120
  tail call void @putchar(i8 %121)
  br label %__elem_0.4.exit

122:                                              ; preds = %9
  br i1 %6, label %178, label %123

123:                                              ; preds = %122
  br i1 %8, label %151, label %codeRepl.i.i3

codeRepl.i.i3:                                    ; preds = %123
  %124 = and i8 %1, 8
  %125 = icmp eq i8 %124, 0
  %126 = and i8 %1, 4
  %127 = icmp eq i8 %126, 0
  %128 = and i8 %1, 2
  %129 = icmp eq i8 %128, 0
  %130 = and i8 %1, 1
  %131 = trunc i8 %130 to i3
  %132 = icmp eq i8 %130, 0
  %..i7.i32 = select i1 %132, i3 2, i3 3
  %.sink.i8.i33 = select i1 %129, i3 %131, i3 %..i7.i32
  br i1 %125, label %140, label %133

133:                                              ; preds = %codeRepl.i.i3
  %134 = zext i3 %.sink.i8.i33 to i4
  br i1 %127, label %137, label %codeRepl.i.i34

codeRepl.i.i34:                                   ; preds = %133
  %135 = or i4 %134, -4
  %136 = zext i4 %135 to i5
  br label %__elem_3.exit37

137:                                              ; preds = %133
  %138 = or i4 %134, -8
  %139 = zext i4 %138 to i5
  br label %__elem_3.exit37

140:                                              ; preds = %codeRepl.i.i3
  br i1 %127, label %143, label %codeRepl.i1.i36

codeRepl.i1.i36:                                  ; preds = %140
  %141 = or i3 %.sink.i8.i33, -4
  %142 = zext i3 %141 to i5
  br label %__elem_3.exit37

143:                                              ; preds = %140
  %144 = zext i3 %.sink.i8.i33 to i5
  br label %__elem_3.exit37

__elem_3.exit37:                                  ; preds = %codeRepl.i.i34, %137, %codeRepl.i1.i36, %143
  %.sink17.i35 = phi i5 [ %142, %codeRepl.i1.i36 ], [ %144, %143 ], [ %139, %137 ], [ %136, %codeRepl.i.i34 ]
  %145 = or i5 %.sink17.i35, -16
  %146 = zext i5 %145 to i6
  %147 = or i6 -32, %146
  %148 = zext i6 %147 to i7
  %149 = zext i7 %148 to i8
  %150 = or i8 -128, %149
  tail call void @putchar(i8 %150)
  br label %__elem_0.4.exit

151:                                              ; preds = %123
  %152 = and i8 %1, 8
  %153 = icmp eq i8 %152, 0
  %154 = and i8 %1, 4
  %155 = icmp eq i8 %154, 0
  %156 = and i8 %1, 2
  %157 = icmp eq i8 %156, 0
  %158 = and i8 %1, 1
  %159 = trunc i8 %158 to i3
  %160 = icmp eq i8 %158, 0
  %..i7.i38 = select i1 %160, i3 2, i3 3
  %.sink.i8.i39 = select i1 %157, i3 %159, i3 %..i7.i38
  br i1 %153, label %168, label %161

161:                                              ; preds = %151
  %162 = zext i3 %.sink.i8.i39 to i4
  br i1 %155, label %165, label %codeRepl.i.i40

codeRepl.i.i40:                                   ; preds = %161
  %163 = or i4 %162, -4
  %164 = zext i4 %163 to i5
  br label %__elem_3.exit43

165:                                              ; preds = %161
  %166 = or i4 %162, -8
  %167 = zext i4 %166 to i5
  br label %__elem_3.exit43

168:                                              ; preds = %151
  br i1 %155, label %171, label %codeRepl.i1.i42

codeRepl.i1.i42:                                  ; preds = %168
  %169 = or i3 %.sink.i8.i39, -4
  %170 = zext i3 %169 to i5
  br label %__elem_3.exit43

171:                                              ; preds = %168
  %172 = zext i3 %.sink.i8.i39 to i5
  br label %__elem_3.exit43

__elem_3.exit43:                                  ; preds = %codeRepl.i.i40, %165, %codeRepl.i1.i42, %171
  %.sink17.i41 = phi i5 [ %170, %codeRepl.i1.i42 ], [ %172, %171 ], [ %167, %165 ], [ %164, %codeRepl.i.i40 ]
  %173 = zext i5 %.sink17.i41 to i6
  %174 = or i6 -32, %173
  %175 = zext i6 %174 to i7
  %176 = zext i7 %175 to i8
  %177 = or i8 -128, %176
  tail call void @putchar(i8 %177)
  br label %__elem_0.4.exit

178:                                              ; preds = %122
  br i1 %8, label %205, label %codeRepl.i1.i4

codeRepl.i1.i4:                                   ; preds = %178
  %179 = and i8 %1, 8
  %180 = icmp eq i8 %179, 0
  %181 = and i8 %1, 4
  %182 = icmp eq i8 %181, 0
  %183 = and i8 %1, 2
  %184 = icmp eq i8 %183, 0
  %185 = and i8 %1, 1
  %186 = trunc i8 %185 to i3
  %187 = icmp eq i8 %185, 0
  %..i7.i44 = select i1 %187, i3 2, i3 3
  %.sink.i8.i45 = select i1 %184, i3 %186, i3 %..i7.i44
  br i1 %180, label %195, label %188

188:                                              ; preds = %codeRepl.i1.i4
  %189 = zext i3 %.sink.i8.i45 to i4
  br i1 %182, label %192, label %codeRepl.i.i46

codeRepl.i.i46:                                   ; preds = %188
  %190 = or i4 %189, -4
  %191 = zext i4 %190 to i5
  br label %__elem_3.exit49

192:                                              ; preds = %188
  %193 = or i4 %189, -8
  %194 = zext i4 %193 to i5
  br label %__elem_3.exit49

195:                                              ; preds = %codeRepl.i1.i4
  br i1 %182, label %198, label %codeRepl.i1.i48

codeRepl.i1.i48:                                  ; preds = %195
  %196 = or i3 %.sink.i8.i45, -4
  %197 = zext i3 %196 to i5
  br label %__elem_3.exit49

198:                                              ; preds = %195
  %199 = zext i3 %.sink.i8.i45 to i5
  br label %__elem_3.exit49

__elem_3.exit49:                                  ; preds = %codeRepl.i.i46, %192, %codeRepl.i1.i48, %198
  %.sink17.i47 = phi i5 [ %197, %codeRepl.i1.i48 ], [ %199, %198 ], [ %194, %192 ], [ %191, %codeRepl.i.i46 ]
  %200 = or i5 %.sink17.i47, -16
  %201 = zext i5 %200 to i6
  %202 = zext i6 %201 to i7
  %203 = zext i7 %202 to i8
  %204 = or i8 -128, %203
  tail call void @putchar(i8 %204)
  br label %__elem_0.4.exit

205:                                              ; preds = %178
  %206 = and i8 %1, 8
  %207 = icmp eq i8 %206, 0
  %208 = and i8 %1, 4
  %209 = icmp eq i8 %208, 0
  %210 = and i8 %1, 2
  %211 = icmp eq i8 %210, 0
  %212 = and i8 %1, 1
  %213 = trunc i8 %212 to i3
  %214 = icmp eq i8 %212, 0
  %..i7.i50 = select i1 %214, i3 2, i3 3
  %.sink.i8.i51 = select i1 %211, i3 %213, i3 %..i7.i50
  br i1 %207, label %222, label %215

215:                                              ; preds = %205
  %216 = zext i3 %.sink.i8.i51 to i4
  br i1 %209, label %219, label %codeRepl.i.i52

codeRepl.i.i52:                                   ; preds = %215
  %217 = or i4 %216, -4
  %218 = zext i4 %217 to i5
  br label %__elem_3.exit55

219:                                              ; preds = %215
  %220 = or i4 %216, -8
  %221 = zext i4 %220 to i5
  br label %__elem_3.exit55

222:                                              ; preds = %205
  br i1 %209, label %225, label %codeRepl.i1.i54

codeRepl.i1.i54:                                  ; preds = %222
  %223 = or i3 %.sink.i8.i51, -4
  %224 = zext i3 %223 to i5
  br label %__elem_3.exit55

225:                                              ; preds = %222
  %226 = zext i3 %.sink.i8.i51 to i5
  br label %__elem_3.exit55

__elem_3.exit55:                                  ; preds = %codeRepl.i.i52, %219, %codeRepl.i1.i54, %225
  %.sink17.i53 = phi i5 [ %224, %codeRepl.i1.i54 ], [ %226, %225 ], [ %221, %219 ], [ %218, %codeRepl.i.i52 ]
  %227 = zext i5 %.sink17.i53 to i6
  %228 = zext i6 %227 to i7
  %229 = zext i7 %228 to i8
  %230 = or i8 -128, %229
  tail call void @putchar(i8 %230)
  br label %__elem_0.4.exit

__elem_0.4.exit:                                  ; preds = %__elem_3.exit103, %__elem_3.exit97, %__elem_3.exit91, %__elem_3.exit85, %__elem_3.exit79, %__elem_3.exit73, %__elem_3.exit67, %__elem_3.exit61, %__elem_3.exit55, %__elem_3.exit49, %__elem_3.exit43, %__elem_3.exit37, %__elem_3.exit31, %__elem_3.exit25, %__elem_3.exit19, %__elem_3.exit
  ret void

231:                                              ; preds = %0
  br i1 %4, label %340, label %codeRepl.i1

codeRepl.i1:                                      ; preds = %231
  br i1 %6, label %287, label %232

232:                                              ; preds = %codeRepl.i1
  br i1 %8, label %260, label %codeRepl.i.i6

codeRepl.i.i6:                                    ; preds = %232
  %233 = and i8 %1, 8
  %234 = icmp eq i8 %233, 0
  %235 = and i8 %1, 4
  %236 = icmp eq i8 %235, 0
  %237 = and i8 %1, 2
  %238 = icmp eq i8 %237, 0
  %239 = and i8 %1, 1
  %240 = trunc i8 %239 to i3
  %241 = icmp eq i8 %239, 0
  %..i7.i56 = select i1 %241, i3 2, i3 3
  %.sink.i8.i57 = select i1 %238, i3 %240, i3 %..i7.i56
  br i1 %234, label %249, label %242

242:                                              ; preds = %codeRepl.i.i6
  %243 = zext i3 %.sink.i8.i57 to i4
  br i1 %236, label %246, label %codeRepl.i.i58

codeRepl.i.i58:                                   ; preds = %242
  %244 = or i4 %243, -4
  %245 = zext i4 %244 to i5
  br label %__elem_3.exit61

246:                                              ; preds = %242
  %247 = or i4 %243, -8
  %248 = zext i4 %247 to i5
  br label %__elem_3.exit61

249:                                              ; preds = %codeRepl.i.i6
  br i1 %236, label %252, label %codeRepl.i1.i60

codeRepl.i1.i60:                                  ; preds = %249
  %250 = or i3 %.sink.i8.i57, -4
  %251 = zext i3 %250 to i5
  br label %__elem_3.exit61

252:                                              ; preds = %249
  %253 = zext i3 %.sink.i8.i57 to i5
  br label %__elem_3.exit61

__elem_3.exit61:                                  ; preds = %codeRepl.i.i58, %246, %codeRepl.i1.i60, %252
  %.sink17.i59 = phi i5 [ %251, %codeRepl.i1.i60 ], [ %253, %252 ], [ %248, %246 ], [ %245, %codeRepl.i.i58 ]
  %254 = or i5 %.sink17.i59, -16
  %255 = zext i5 %254 to i6
  %256 = or i6 -32, %255
  %257 = zext i6 %256 to i7
  %258 = or i7 -64, %257
  %259 = zext i7 %258 to i8
  tail call void @putchar(i8 %259)
  br label %__elem_0.4.exit

260:                                              ; preds = %232
  %261 = and i8 %1, 8
  %262 = icmp eq i8 %261, 0
  %263 = and i8 %1, 4
  %264 = icmp eq i8 %263, 0
  %265 = and i8 %1, 2
  %266 = icmp eq i8 %265, 0
  %267 = and i8 %1, 1
  %268 = trunc i8 %267 to i3
  %269 = icmp eq i8 %267, 0
  %..i7.i62 = select i1 %269, i3 2, i3 3
  %.sink.i8.i63 = select i1 %266, i3 %268, i3 %..i7.i62
  br i1 %262, label %277, label %270

270:                                              ; preds = %260
  %271 = zext i3 %.sink.i8.i63 to i4
  br i1 %264, label %274, label %codeRepl.i.i64

codeRepl.i.i64:                                   ; preds = %270
  %272 = or i4 %271, -4
  %273 = zext i4 %272 to i5
  br label %__elem_3.exit67

274:                                              ; preds = %270
  %275 = or i4 %271, -8
  %276 = zext i4 %275 to i5
  br label %__elem_3.exit67

277:                                              ; preds = %260
  br i1 %264, label %280, label %codeRepl.i1.i66

codeRepl.i1.i66:                                  ; preds = %277
  %278 = or i3 %.sink.i8.i63, -4
  %279 = zext i3 %278 to i5
  br label %__elem_3.exit67

280:                                              ; preds = %277
  %281 = zext i3 %.sink.i8.i63 to i5
  br label %__elem_3.exit67

__elem_3.exit67:                                  ; preds = %codeRepl.i.i64, %274, %codeRepl.i1.i66, %280
  %.sink17.i65 = phi i5 [ %279, %codeRepl.i1.i66 ], [ %281, %280 ], [ %276, %274 ], [ %273, %codeRepl.i.i64 ]
  %282 = zext i5 %.sink17.i65 to i6
  %283 = or i6 -32, %282
  %284 = zext i6 %283 to i7
  %285 = or i7 -64, %284
  %286 = zext i7 %285 to i8
  tail call void @putchar(i8 %286)
  br label %__elem_0.4.exit

287:                                              ; preds = %codeRepl.i1
  br i1 %8, label %314, label %codeRepl.i1.i7

codeRepl.i1.i7:                                   ; preds = %287
  %288 = and i8 %1, 8
  %289 = icmp eq i8 %288, 0
  %290 = and i8 %1, 4
  %291 = icmp eq i8 %290, 0
  %292 = and i8 %1, 2
  %293 = icmp eq i8 %292, 0
  %294 = and i8 %1, 1
  %295 = trunc i8 %294 to i3
  %296 = icmp eq i8 %294, 0
  %..i7.i68 = select i1 %296, i3 2, i3 3
  %.sink.i8.i69 = select i1 %293, i3 %295, i3 %..i7.i68
  br i1 %289, label %304, label %297

297:                                              ; preds = %codeRepl.i1.i7
  %298 = zext i3 %.sink.i8.i69 to i4
  br i1 %291, label %301, label %codeRepl.i.i70

codeRepl.i.i70:                                   ; preds = %297
  %299 = or i4 %298, -4
  %300 = zext i4 %299 to i5
  br label %__elem_3.exit73

301:                                              ; preds = %297
  %302 = or i4 %298, -8
  %303 = zext i4 %302 to i5
  br label %__elem_3.exit73

304:                                              ; preds = %codeRepl.i1.i7
  br i1 %291, label %307, label %codeRepl.i1.i72

codeRepl.i1.i72:                                  ; preds = %304
  %305 = or i3 %.sink.i8.i69, -4
  %306 = zext i3 %305 to i5
  br label %__elem_3.exit73

307:                                              ; preds = %304
  %308 = zext i3 %.sink.i8.i69 to i5
  br label %__elem_3.exit73

__elem_3.exit73:                                  ; preds = %codeRepl.i.i70, %301, %codeRepl.i1.i72, %307
  %.sink17.i71 = phi i5 [ %306, %codeRepl.i1.i72 ], [ %308, %307 ], [ %303, %301 ], [ %300, %codeRepl.i.i70 ]
  %309 = or i5 %.sink17.i71, -16
  %310 = zext i5 %309 to i6
  %311 = zext i6 %310 to i7
  %312 = or i7 -64, %311
  %313 = zext i7 %312 to i8
  tail call void @putchar(i8 %313)
  br label %__elem_0.4.exit

314:                                              ; preds = %287
  %315 = and i8 %1, 8
  %316 = icmp eq i8 %315, 0
  %317 = and i8 %1, 4
  %318 = icmp eq i8 %317, 0
  %319 = and i8 %1, 2
  %320 = icmp eq i8 %319, 0
  %321 = and i8 %1, 1
  %322 = trunc i8 %321 to i3
  %323 = icmp eq i8 %321, 0
  %..i7.i74 = select i1 %323, i3 2, i3 3
  %.sink.i8.i75 = select i1 %320, i3 %322, i3 %..i7.i74
  br i1 %316, label %331, label %324

324:                                              ; preds = %314
  %325 = zext i3 %.sink.i8.i75 to i4
  br i1 %318, label %328, label %codeRepl.i.i76

codeRepl.i.i76:                                   ; preds = %324
  %326 = or i4 %325, -4
  %327 = zext i4 %326 to i5
  br label %__elem_3.exit79

328:                                              ; preds = %324
  %329 = or i4 %325, -8
  %330 = zext i4 %329 to i5
  br label %__elem_3.exit79

331:                                              ; preds = %314
  br i1 %318, label %334, label %codeRepl.i1.i78

codeRepl.i1.i78:                                  ; preds = %331
  %332 = or i3 %.sink.i8.i75, -4
  %333 = zext i3 %332 to i5
  br label %__elem_3.exit79

334:                                              ; preds = %331
  %335 = zext i3 %.sink.i8.i75 to i5
  br label %__elem_3.exit79

__elem_3.exit79:                                  ; preds = %codeRepl.i.i76, %328, %codeRepl.i1.i78, %334
  %.sink17.i77 = phi i5 [ %333, %codeRepl.i1.i78 ], [ %335, %334 ], [ %330, %328 ], [ %327, %codeRepl.i.i76 ]
  %336 = zext i5 %.sink17.i77 to i6
  %337 = zext i6 %336 to i7
  %338 = or i7 -64, %337
  %339 = zext i7 %338 to i8
  tail call void @putchar(i8 %339)
  br label %__elem_0.4.exit

340:                                              ; preds = %231
  br i1 %6, label %394, label %341

341:                                              ; preds = %340
  br i1 %8, label %368, label %codeRepl.i.i9

codeRepl.i.i9:                                    ; preds = %341
  %342 = and i8 %1, 8
  %343 = icmp eq i8 %342, 0
  %344 = and i8 %1, 4
  %345 = icmp eq i8 %344, 0
  %346 = and i8 %1, 2
  %347 = icmp eq i8 %346, 0
  %348 = and i8 %1, 1
  %349 = trunc i8 %348 to i3
  %350 = icmp eq i8 %348, 0
  %..i7.i80 = select i1 %350, i3 2, i3 3
  %.sink.i8.i81 = select i1 %347, i3 %349, i3 %..i7.i80
  br i1 %343, label %358, label %351

351:                                              ; preds = %codeRepl.i.i9
  %352 = zext i3 %.sink.i8.i81 to i4
  br i1 %345, label %355, label %codeRepl.i.i82

codeRepl.i.i82:                                   ; preds = %351
  %353 = or i4 %352, -4
  %354 = zext i4 %353 to i5
  br label %__elem_3.exit85

355:                                              ; preds = %351
  %356 = or i4 %352, -8
  %357 = zext i4 %356 to i5
  br label %__elem_3.exit85

358:                                              ; preds = %codeRepl.i.i9
  br i1 %345, label %361, label %codeRepl.i1.i84

codeRepl.i1.i84:                                  ; preds = %358
  %359 = or i3 %.sink.i8.i81, -4
  %360 = zext i3 %359 to i5
  br label %__elem_3.exit85

361:                                              ; preds = %358
  %362 = zext i3 %.sink.i8.i81 to i5
  br label %__elem_3.exit85

__elem_3.exit85:                                  ; preds = %codeRepl.i.i82, %355, %codeRepl.i1.i84, %361
  %.sink17.i83 = phi i5 [ %360, %codeRepl.i1.i84 ], [ %362, %361 ], [ %357, %355 ], [ %354, %codeRepl.i.i82 ]
  %363 = or i5 %.sink17.i83, -16
  %364 = zext i5 %363 to i6
  %365 = or i6 -32, %364
  %366 = zext i6 %365 to i7
  %367 = zext i7 %366 to i8
  tail call void @putchar(i8 %367)
  br label %__elem_0.4.exit

368:                                              ; preds = %341
  %369 = and i8 %1, 8
  %370 = icmp eq i8 %369, 0
  %371 = and i8 %1, 4
  %372 = icmp eq i8 %371, 0
  %373 = and i8 %1, 2
  %374 = icmp eq i8 %373, 0
  %375 = and i8 %1, 1
  %376 = trunc i8 %375 to i3
  %377 = icmp eq i8 %375, 0
  %..i7.i86 = select i1 %377, i3 2, i3 3
  %.sink.i8.i87 = select i1 %374, i3 %376, i3 %..i7.i86
  br i1 %370, label %385, label %378

378:                                              ; preds = %368
  %379 = zext i3 %.sink.i8.i87 to i4
  br i1 %372, label %382, label %codeRepl.i.i88

codeRepl.i.i88:                                   ; preds = %378
  %380 = or i4 %379, -4
  %381 = zext i4 %380 to i5
  br label %__elem_3.exit91

382:                                              ; preds = %378
  %383 = or i4 %379, -8
  %384 = zext i4 %383 to i5
  br label %__elem_3.exit91

385:                                              ; preds = %368
  br i1 %372, label %388, label %codeRepl.i1.i90

codeRepl.i1.i90:                                  ; preds = %385
  %386 = or i3 %.sink.i8.i87, -4
  %387 = zext i3 %386 to i5
  br label %__elem_3.exit91

388:                                              ; preds = %385
  %389 = zext i3 %.sink.i8.i87 to i5
  br label %__elem_3.exit91

__elem_3.exit91:                                  ; preds = %codeRepl.i.i88, %382, %codeRepl.i1.i90, %388
  %.sink17.i89 = phi i5 [ %387, %codeRepl.i1.i90 ], [ %389, %388 ], [ %384, %382 ], [ %381, %codeRepl.i.i88 ]
  %390 = zext i5 %.sink17.i89 to i6
  %391 = or i6 -32, %390
  %392 = zext i6 %391 to i7
  %393 = zext i7 %392 to i8
  tail call void @putchar(i8 %393)
  br label %__elem_0.4.exit

394:                                              ; preds = %340
  br i1 %8, label %420, label %codeRepl.i1.i10

codeRepl.i1.i10:                                  ; preds = %394
  %395 = and i8 %1, 8
  %396 = icmp eq i8 %395, 0
  %397 = and i8 %1, 4
  %398 = icmp eq i8 %397, 0
  %399 = and i8 %1, 2
  %400 = icmp eq i8 %399, 0
  %401 = and i8 %1, 1
  %402 = trunc i8 %401 to i3
  %403 = icmp eq i8 %401, 0
  %..i7.i92 = select i1 %403, i3 2, i3 3
  %.sink.i8.i93 = select i1 %400, i3 %402, i3 %..i7.i92
  br i1 %396, label %411, label %404

404:                                              ; preds = %codeRepl.i1.i10
  %405 = zext i3 %.sink.i8.i93 to i4
  br i1 %398, label %408, label %codeRepl.i.i94

codeRepl.i.i94:                                   ; preds = %404
  %406 = or i4 %405, -4
  %407 = zext i4 %406 to i5
  br label %__elem_3.exit97

408:                                              ; preds = %404
  %409 = or i4 %405, -8
  %410 = zext i4 %409 to i5
  br label %__elem_3.exit97

411:                                              ; preds = %codeRepl.i1.i10
  br i1 %398, label %414, label %codeRepl.i1.i96

codeRepl.i1.i96:                                  ; preds = %411
  %412 = or i3 %.sink.i8.i93, -4
  %413 = zext i3 %412 to i5
  br label %__elem_3.exit97

414:                                              ; preds = %411
  %415 = zext i3 %.sink.i8.i93 to i5
  br label %__elem_3.exit97

__elem_3.exit97:                                  ; preds = %codeRepl.i.i94, %408, %codeRepl.i1.i96, %414
  %.sink17.i95 = phi i5 [ %413, %codeRepl.i1.i96 ], [ %415, %414 ], [ %410, %408 ], [ %407, %codeRepl.i.i94 ]
  %416 = or i5 %.sink17.i95, -16
  %417 = zext i5 %416 to i6
  %418 = zext i6 %417 to i7
  %419 = zext i7 %418 to i8
  tail call void @putchar(i8 %419)
  br label %__elem_0.4.exit

420:                                              ; preds = %394
  %421 = and i8 %1, 8
  %422 = icmp eq i8 %421, 0
  %423 = and i8 %1, 4
  %424 = icmp eq i8 %423, 0
  %425 = and i8 %1, 2
  %426 = icmp eq i8 %425, 0
  %427 = and i8 %1, 1
  %428 = trunc i8 %427 to i3
  %429 = icmp eq i8 %427, 0
  %..i7.i98 = select i1 %429, i3 2, i3 3
  %.sink.i8.i99 = select i1 %426, i3 %428, i3 %..i7.i98
  br i1 %422, label %437, label %430

430:                                              ; preds = %420
  %431 = zext i3 %.sink.i8.i99 to i4
  br i1 %424, label %434, label %codeRepl.i.i100

codeRepl.i.i100:                                  ; preds = %430
  %432 = or i4 %431, -4
  %433 = zext i4 %432 to i5
  br label %__elem_3.exit103

434:                                              ; preds = %430
  %435 = or i4 %431, -8
  %436 = zext i4 %435 to i5
  br label %__elem_3.exit103

437:                                              ; preds = %420
  br i1 %424, label %440, label %codeRepl.i1.i102

codeRepl.i1.i102:                                 ; preds = %437
  %438 = or i3 %.sink.i8.i99, -4
  %439 = zext i3 %438 to i5
  br label %__elem_3.exit103

440:                                              ; preds = %437
  %441 = zext i3 %.sink.i8.i99 to i5
  br label %__elem_3.exit103

__elem_3.exit103:                                 ; preds = %codeRepl.i.i100, %434, %codeRepl.i1.i102, %440
  %.sink17.i101 = phi i5 [ %439, %codeRepl.i1.i102 ], [ %441, %440 ], [ %436, %434 ], [ %433, %codeRepl.i.i100 ]
  %442 = zext i5 %.sink17.i101 to i6
  %443 = zext i6 %442 to i7
  %444 = zext i7 %443 to i8
  tail call void @putchar(i8 %444)
  br label %__elem_0.4.exit
}

attributes #0 = { nofree nounwind }
