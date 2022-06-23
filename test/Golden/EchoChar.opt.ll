; ModuleID = '<string>'
source_filename = "test/Golden/EchoChar.elem"

declare void @putchar(i8) local_unnamed_addr

; Function Attrs: nofree nounwind
declare i8 @getchar() local_unnamed_addr #0

define private fastcc void @__elem_2(i1, i8, i1, i1) unnamed_addr {
  %5 = and i8 %1, 16
  %6 = icmp eq i8 %5, 0
  %7 = and i8 %1, 8
  %8 = icmp eq i8 %7, 0
  br i1 %6, label %11, label %9

9:                                                ; preds = %4
  br i1 %8, label %10, label %codeRepl.i

codeRepl.i:                                       ; preds = %9
  tail call fastcc void @__elem_4(i1 true, i8 %1, i1 %2, i1 %3, i1 %0, i1 true)
  br label %__elem_3.2.exit

10:                                               ; preds = %9
  tail call fastcc void @__elem_4(i1 false, i8 %1, i1 %2, i1 %3, i1 %0, i1 true)
  br label %__elem_3.2.exit

__elem_3.2.exit:                                  ; preds = %codeRepl.i1, %12, %10, %codeRepl.i
  ret void

11:                                               ; preds = %4
  br i1 %8, label %12, label %codeRepl.i1

codeRepl.i1:                                      ; preds = %11
  tail call fastcc void @__elem_4(i1 true, i8 %1, i1 %2, i1 %3, i1 %0, i1 false)
  br label %__elem_3.2.exit

12:                                               ; preds = %11
  tail call fastcc void @__elem_4(i1 false, i8 %1, i1 %2, i1 %3, i1 %0, i1 false)
  br label %__elem_3.2.exit
}

define private fastcc void @__elem_4(i1, i8, i1, i1, i1, i1) unnamed_addr {
  %7 = and i8 %1, 4
  %8 = icmp eq i8 %7, 0
  %9 = and i8 %1, 2
  %10 = icmp eq i8 %9, 0
  br i1 %8, label %13, label %11

11:                                               ; preds = %6
  br i1 %10, label %12, label %codeRepl.i

codeRepl.i:                                       ; preds = %11
  tail call fastcc void @__elem_6(i1 true, i8 %1, i1 %2, i1 %3, i1 %4, i1 %5, i1 %0, i1 true)
  br label %__elem_5.1.exit

12:                                               ; preds = %11
  tail call fastcc void @__elem_6(i1 false, i8 %1, i1 %2, i1 %3, i1 %4, i1 %5, i1 %0, i1 true)
  br label %__elem_5.1.exit

__elem_5.1.exit:                                  ; preds = %codeRepl.i1, %14, %12, %codeRepl.i
  ret void

13:                                               ; preds = %6
  br i1 %10, label %14, label %codeRepl.i1

codeRepl.i1:                                      ; preds = %13
  tail call fastcc void @__elem_6(i1 true, i8 %1, i1 %2, i1 %3, i1 %4, i1 %5, i1 %0, i1 false)
  br label %__elem_5.1.exit

14:                                               ; preds = %13
  tail call fastcc void @__elem_6(i1 false, i8 %1, i1 %2, i1 %3, i1 %4, i1 %5, i1 %0, i1 false)
  br label %__elem_5.1.exit
}

define private fastcc void @__elem_6(i1, i8, i1, i1, i1, i1, i1, i1) unnamed_addr {
  %9 = and i8 %1, 1
  %10 = icmp eq i8 %9, 0
  br i1 %10, label %12, label %11

11:                                               ; preds = %8
  br i1 %2, label %13, label %14

12:                                               ; preds = %8
  br i1 %2, label %15, label %16

13:                                               ; preds = %11
  br i1 %3, label %17, label %18

14:                                               ; preds = %11
  br i1 %3, label %47, label %48

15:                                               ; preds = %12
  br i1 %3, label %77, label %78

16:                                               ; preds = %12
  br i1 %3, label %107, label %108

17:                                               ; preds = %13
  br i1 %4, label %19, label %20

18:                                               ; preds = %13
  br i1 %4, label %33, label %34

19:                                               ; preds = %17
  br i1 %5, label %21, label %22

20:                                               ; preds = %17
  br i1 %5, label %27, label %28

21:                                               ; preds = %19
  br i1 %6, label %23, label %24

22:                                               ; preds = %19
  br i1 %6, label %25, label %26

23:                                               ; preds = %21
  br i1 %7, label %137, label %138

24:                                               ; preds = %21
  br i1 %7, label %139, label %140

25:                                               ; preds = %22
  br i1 %7, label %141, label %142

26:                                               ; preds = %22
  br i1 %7, label %143, label %144

27:                                               ; preds = %20
  br i1 %6, label %29, label %30

28:                                               ; preds = %20
  br i1 %6, label %31, label %32

29:                                               ; preds = %27
  br i1 %7, label %145, label %146

30:                                               ; preds = %27
  br i1 %7, label %147, label %148

31:                                               ; preds = %28
  br i1 %7, label %149, label %150

32:                                               ; preds = %28
  br i1 %7, label %151, label %152

33:                                               ; preds = %18
  br i1 %5, label %35, label %36

34:                                               ; preds = %18
  br i1 %5, label %41, label %42

35:                                               ; preds = %33
  br i1 %6, label %37, label %38

36:                                               ; preds = %33
  br i1 %6, label %39, label %40

37:                                               ; preds = %35
  br i1 %7, label %153, label %154

38:                                               ; preds = %35
  br i1 %7, label %155, label %156

39:                                               ; preds = %36
  br i1 %7, label %157, label %158

40:                                               ; preds = %36
  br i1 %7, label %159, label %160

41:                                               ; preds = %34
  br i1 %6, label %43, label %44

42:                                               ; preds = %34
  br i1 %6, label %45, label %46

43:                                               ; preds = %41
  br i1 %7, label %161, label %162

44:                                               ; preds = %41
  br i1 %7, label %163, label %164

45:                                               ; preds = %42
  br i1 %7, label %165, label %166

46:                                               ; preds = %42
  br i1 %7, label %167, label %168

47:                                               ; preds = %14
  br i1 %4, label %49, label %50

48:                                               ; preds = %14
  br i1 %4, label %63, label %64

49:                                               ; preds = %47
  br i1 %5, label %51, label %52

50:                                               ; preds = %47
  br i1 %5, label %57, label %58

51:                                               ; preds = %49
  br i1 %6, label %53, label %54

52:                                               ; preds = %49
  br i1 %6, label %55, label %56

53:                                               ; preds = %51
  br i1 %7, label %169, label %170

54:                                               ; preds = %51
  br i1 %7, label %171, label %172

55:                                               ; preds = %52
  br i1 %7, label %173, label %174

56:                                               ; preds = %52
  br i1 %7, label %175, label %176

57:                                               ; preds = %50
  br i1 %6, label %59, label %60

58:                                               ; preds = %50
  br i1 %6, label %61, label %62

59:                                               ; preds = %57
  br i1 %7, label %177, label %178

60:                                               ; preds = %57
  br i1 %7, label %179, label %180

61:                                               ; preds = %58
  br i1 %7, label %181, label %182

62:                                               ; preds = %58
  br i1 %7, label %183, label %184

63:                                               ; preds = %48
  br i1 %5, label %65, label %66

64:                                               ; preds = %48
  br i1 %5, label %71, label %72

65:                                               ; preds = %63
  br i1 %6, label %67, label %68

66:                                               ; preds = %63
  br i1 %6, label %69, label %70

67:                                               ; preds = %65
  br i1 %7, label %185, label %186

68:                                               ; preds = %65
  br i1 %7, label %187, label %188

69:                                               ; preds = %66
  br i1 %7, label %189, label %190

70:                                               ; preds = %66
  br i1 %7, label %191, label %192

71:                                               ; preds = %64
  br i1 %6, label %73, label %74

72:                                               ; preds = %64
  br i1 %6, label %75, label %76

73:                                               ; preds = %71
  br i1 %7, label %193, label %194

74:                                               ; preds = %71
  br i1 %7, label %195, label %196

75:                                               ; preds = %72
  br i1 %7, label %197, label %198

76:                                               ; preds = %72
  br i1 %7, label %199, label %200

77:                                               ; preds = %15
  br i1 %4, label %79, label %80

78:                                               ; preds = %15
  br i1 %4, label %93, label %94

79:                                               ; preds = %77
  br i1 %5, label %81, label %82

80:                                               ; preds = %77
  br i1 %5, label %87, label %88

81:                                               ; preds = %79
  br i1 %6, label %83, label %84

82:                                               ; preds = %79
  br i1 %6, label %85, label %86

83:                                               ; preds = %81
  br i1 %7, label %201, label %202

84:                                               ; preds = %81
  br i1 %7, label %203, label %204

85:                                               ; preds = %82
  br i1 %7, label %205, label %206

86:                                               ; preds = %82
  br i1 %7, label %207, label %208

87:                                               ; preds = %80
  br i1 %6, label %89, label %90

88:                                               ; preds = %80
  br i1 %6, label %91, label %92

89:                                               ; preds = %87
  br i1 %7, label %209, label %210

90:                                               ; preds = %87
  br i1 %7, label %211, label %212

91:                                               ; preds = %88
  br i1 %7, label %213, label %214

92:                                               ; preds = %88
  br i1 %7, label %215, label %216

93:                                               ; preds = %78
  br i1 %5, label %95, label %96

94:                                               ; preds = %78
  br i1 %5, label %101, label %102

95:                                               ; preds = %93
  br i1 %6, label %97, label %98

96:                                               ; preds = %93
  br i1 %6, label %99, label %100

97:                                               ; preds = %95
  br i1 %7, label %217, label %218

98:                                               ; preds = %95
  br i1 %7, label %219, label %220

99:                                               ; preds = %96
  br i1 %7, label %221, label %222

100:                                              ; preds = %96
  br i1 %7, label %223, label %224

101:                                              ; preds = %94
  br i1 %6, label %103, label %104

102:                                              ; preds = %94
  br i1 %6, label %105, label %106

103:                                              ; preds = %101
  br i1 %7, label %225, label %226

104:                                              ; preds = %101
  br i1 %7, label %227, label %228

105:                                              ; preds = %102
  br i1 %7, label %229, label %230

106:                                              ; preds = %102
  br i1 %7, label %231, label %232

107:                                              ; preds = %16
  br i1 %4, label %109, label %110

108:                                              ; preds = %16
  br i1 %4, label %123, label %124

109:                                              ; preds = %107
  br i1 %5, label %111, label %112

110:                                              ; preds = %107
  br i1 %5, label %117, label %118

111:                                              ; preds = %109
  br i1 %6, label %113, label %114

112:                                              ; preds = %109
  br i1 %6, label %115, label %116

113:                                              ; preds = %111
  br i1 %7, label %233, label %234

114:                                              ; preds = %111
  br i1 %7, label %235, label %236

115:                                              ; preds = %112
  br i1 %7, label %237, label %238

116:                                              ; preds = %112
  br i1 %7, label %239, label %240

117:                                              ; preds = %110
  br i1 %6, label %119, label %120

118:                                              ; preds = %110
  br i1 %6, label %121, label %122

119:                                              ; preds = %117
  br i1 %7, label %241, label %242

120:                                              ; preds = %117
  br i1 %7, label %243, label %244

121:                                              ; preds = %118
  br i1 %7, label %245, label %246

122:                                              ; preds = %118
  br i1 %7, label %247, label %248

123:                                              ; preds = %108
  br i1 %5, label %125, label %126

124:                                              ; preds = %108
  br i1 %5, label %131, label %132

125:                                              ; preds = %123
  br i1 %6, label %127, label %128

126:                                              ; preds = %123
  br i1 %6, label %129, label %130

127:                                              ; preds = %125
  br i1 %7, label %249, label %250

128:                                              ; preds = %125
  br i1 %7, label %251, label %252

129:                                              ; preds = %126
  br i1 %7, label %253, label %254

130:                                              ; preds = %126
  br i1 %7, label %255, label %256

131:                                              ; preds = %124
  br i1 %6, label %133, label %134

132:                                              ; preds = %124
  br i1 %6, label %135, label %136

133:                                              ; preds = %131
  br i1 %7, label %257, label %258

134:                                              ; preds = %131
  br i1 %7, label %259, label %260

135:                                              ; preds = %132
  br i1 %7, label %261, label %262

136:                                              ; preds = %132
  br i1 %7, label %263, label %264

137:                                              ; preds = %23
  br i1 %0, label %265, label %266

138:                                              ; preds = %23
  br i1 %0, label %267, label %268

139:                                              ; preds = %24
  br i1 %0, label %269, label %270

140:                                              ; preds = %24
  br i1 %0, label %271, label %272

141:                                              ; preds = %25
  br i1 %0, label %273, label %274

142:                                              ; preds = %25
  br i1 %0, label %275, label %276

143:                                              ; preds = %26
  br i1 %0, label %277, label %278

144:                                              ; preds = %26
  br i1 %0, label %279, label %280

145:                                              ; preds = %29
  br i1 %0, label %281, label %282

146:                                              ; preds = %29
  br i1 %0, label %283, label %284

147:                                              ; preds = %30
  br i1 %0, label %285, label %286

148:                                              ; preds = %30
  br i1 %0, label %287, label %288

149:                                              ; preds = %31
  br i1 %0, label %289, label %290

150:                                              ; preds = %31
  br i1 %0, label %291, label %292

151:                                              ; preds = %32
  br i1 %0, label %293, label %294

152:                                              ; preds = %32
  br i1 %0, label %295, label %296

153:                                              ; preds = %37
  br i1 %0, label %297, label %298

154:                                              ; preds = %37
  br i1 %0, label %299, label %300

155:                                              ; preds = %38
  br i1 %0, label %301, label %302

156:                                              ; preds = %38
  br i1 %0, label %303, label %304

157:                                              ; preds = %39
  br i1 %0, label %305, label %306

158:                                              ; preds = %39
  br i1 %0, label %307, label %308

159:                                              ; preds = %40
  br i1 %0, label %309, label %310

160:                                              ; preds = %40
  br i1 %0, label %311, label %312

161:                                              ; preds = %43
  br i1 %0, label %313, label %314

162:                                              ; preds = %43
  br i1 %0, label %315, label %316

163:                                              ; preds = %44
  br i1 %0, label %317, label %318

164:                                              ; preds = %44
  br i1 %0, label %319, label %320

165:                                              ; preds = %45
  br i1 %0, label %321, label %322

166:                                              ; preds = %45
  br i1 %0, label %323, label %324

167:                                              ; preds = %46
  br i1 %0, label %325, label %326

168:                                              ; preds = %46
  br i1 %0, label %327, label %328

169:                                              ; preds = %53
  br i1 %0, label %329, label %330

170:                                              ; preds = %53
  br i1 %0, label %331, label %332

171:                                              ; preds = %54
  br i1 %0, label %333, label %334

172:                                              ; preds = %54
  br i1 %0, label %335, label %336

173:                                              ; preds = %55
  br i1 %0, label %337, label %338

174:                                              ; preds = %55
  br i1 %0, label %339, label %340

175:                                              ; preds = %56
  br i1 %0, label %341, label %342

176:                                              ; preds = %56
  br i1 %0, label %343, label %344

177:                                              ; preds = %59
  br i1 %0, label %345, label %346

178:                                              ; preds = %59
  br i1 %0, label %347, label %348

179:                                              ; preds = %60
  br i1 %0, label %349, label %350

180:                                              ; preds = %60
  br i1 %0, label %351, label %352

181:                                              ; preds = %61
  br i1 %0, label %353, label %354

182:                                              ; preds = %61
  br i1 %0, label %355, label %356

183:                                              ; preds = %62
  br i1 %0, label %357, label %358

184:                                              ; preds = %62
  br i1 %0, label %359, label %360

185:                                              ; preds = %67
  br i1 %0, label %361, label %362

186:                                              ; preds = %67
  br i1 %0, label %363, label %364

187:                                              ; preds = %68
  br i1 %0, label %365, label %366

188:                                              ; preds = %68
  br i1 %0, label %367, label %368

189:                                              ; preds = %69
  br i1 %0, label %369, label %370

190:                                              ; preds = %69
  br i1 %0, label %371, label %372

191:                                              ; preds = %70
  br i1 %0, label %373, label %374

192:                                              ; preds = %70
  br i1 %0, label %375, label %376

193:                                              ; preds = %73
  br i1 %0, label %377, label %378

194:                                              ; preds = %73
  br i1 %0, label %379, label %380

195:                                              ; preds = %74
  br i1 %0, label %381, label %382

196:                                              ; preds = %74
  br i1 %0, label %383, label %384

197:                                              ; preds = %75
  br i1 %0, label %385, label %386

198:                                              ; preds = %75
  br i1 %0, label %387, label %388

199:                                              ; preds = %76
  br i1 %0, label %389, label %390

200:                                              ; preds = %76
  br i1 %0, label %391, label %392

201:                                              ; preds = %83
  br i1 %0, label %393, label %394

202:                                              ; preds = %83
  br i1 %0, label %395, label %396

203:                                              ; preds = %84
  br i1 %0, label %397, label %398

204:                                              ; preds = %84
  br i1 %0, label %399, label %400

205:                                              ; preds = %85
  br i1 %0, label %401, label %402

206:                                              ; preds = %85
  br i1 %0, label %403, label %404

207:                                              ; preds = %86
  br i1 %0, label %405, label %406

208:                                              ; preds = %86
  br i1 %0, label %407, label %408

209:                                              ; preds = %89
  br i1 %0, label %409, label %410

210:                                              ; preds = %89
  br i1 %0, label %411, label %412

211:                                              ; preds = %90
  br i1 %0, label %413, label %414

212:                                              ; preds = %90
  br i1 %0, label %415, label %416

213:                                              ; preds = %91
  br i1 %0, label %417, label %418

214:                                              ; preds = %91
  br i1 %0, label %419, label %420

215:                                              ; preds = %92
  br i1 %0, label %421, label %422

216:                                              ; preds = %92
  br i1 %0, label %423, label %424

217:                                              ; preds = %97
  br i1 %0, label %425, label %426

218:                                              ; preds = %97
  br i1 %0, label %427, label %428

219:                                              ; preds = %98
  br i1 %0, label %429, label %430

220:                                              ; preds = %98
  br i1 %0, label %431, label %432

221:                                              ; preds = %99
  br i1 %0, label %433, label %434

222:                                              ; preds = %99
  br i1 %0, label %435, label %436

223:                                              ; preds = %100
  br i1 %0, label %437, label %438

224:                                              ; preds = %100
  br i1 %0, label %439, label %440

225:                                              ; preds = %103
  br i1 %0, label %441, label %442

226:                                              ; preds = %103
  br i1 %0, label %443, label %444

227:                                              ; preds = %104
  br i1 %0, label %445, label %446

228:                                              ; preds = %104
  br i1 %0, label %447, label %448

229:                                              ; preds = %105
  br i1 %0, label %449, label %450

230:                                              ; preds = %105
  br i1 %0, label %451, label %452

231:                                              ; preds = %106
  br i1 %0, label %453, label %454

232:                                              ; preds = %106
  br i1 %0, label %455, label %456

233:                                              ; preds = %113
  br i1 %0, label %457, label %458

234:                                              ; preds = %113
  br i1 %0, label %459, label %460

235:                                              ; preds = %114
  br i1 %0, label %461, label %462

236:                                              ; preds = %114
  br i1 %0, label %463, label %464

237:                                              ; preds = %115
  br i1 %0, label %465, label %466

238:                                              ; preds = %115
  br i1 %0, label %467, label %468

239:                                              ; preds = %116
  br i1 %0, label %469, label %470

240:                                              ; preds = %116
  br i1 %0, label %471, label %472

241:                                              ; preds = %119
  br i1 %0, label %473, label %474

242:                                              ; preds = %119
  br i1 %0, label %475, label %476

243:                                              ; preds = %120
  br i1 %0, label %477, label %478

244:                                              ; preds = %120
  br i1 %0, label %479, label %480

245:                                              ; preds = %121
  br i1 %0, label %481, label %482

246:                                              ; preds = %121
  br i1 %0, label %483, label %484

247:                                              ; preds = %122
  br i1 %0, label %485, label %486

248:                                              ; preds = %122
  br i1 %0, label %487, label %488

249:                                              ; preds = %127
  br i1 %0, label %489, label %490

250:                                              ; preds = %127
  br i1 %0, label %491, label %492

251:                                              ; preds = %128
  br i1 %0, label %493, label %494

252:                                              ; preds = %128
  br i1 %0, label %495, label %496

253:                                              ; preds = %129
  br i1 %0, label %497, label %498

254:                                              ; preds = %129
  br i1 %0, label %499, label %500

255:                                              ; preds = %130
  br i1 %0, label %501, label %502

256:                                              ; preds = %130
  br i1 %0, label %503, label %504

257:                                              ; preds = %133
  br i1 %0, label %505, label %506

258:                                              ; preds = %133
  br i1 %0, label %507, label %508

259:                                              ; preds = %134
  br i1 %0, label %509, label %510

260:                                              ; preds = %134
  br i1 %0, label %511, label %512

261:                                              ; preds = %135
  br i1 %0, label %513, label %514

262:                                              ; preds = %135
  br i1 %0, label %515, label %516

263:                                              ; preds = %136
  br i1 %0, label %517, label %518

264:                                              ; preds = %136
  br i1 %0, label %519, label %520

265:                                              ; preds = %137
  tail call void @putchar(i8 -1)
  ret void

266:                                              ; preds = %137
  tail call void @putchar(i8 -3)
  ret void

267:                                              ; preds = %138
  tail call void @putchar(i8 -5)
  ret void

268:                                              ; preds = %138
  tail call void @putchar(i8 -7)
  ret void

269:                                              ; preds = %139
  tail call void @putchar(i8 -9)
  ret void

270:                                              ; preds = %139
  tail call void @putchar(i8 -11)
  ret void

271:                                              ; preds = %140
  tail call void @putchar(i8 -13)
  ret void

272:                                              ; preds = %140
  tail call void @putchar(i8 -15)
  ret void

273:                                              ; preds = %141
  tail call void @putchar(i8 -17)
  ret void

274:                                              ; preds = %141
  tail call void @putchar(i8 -19)
  ret void

275:                                              ; preds = %142
  tail call void @putchar(i8 -21)
  ret void

276:                                              ; preds = %142
  tail call void @putchar(i8 -23)
  ret void

277:                                              ; preds = %143
  tail call void @putchar(i8 -25)
  ret void

278:                                              ; preds = %143
  tail call void @putchar(i8 -27)
  ret void

279:                                              ; preds = %144
  tail call void @putchar(i8 -29)
  ret void

280:                                              ; preds = %144
  tail call void @putchar(i8 -31)
  ret void

281:                                              ; preds = %145
  tail call void @putchar(i8 -33)
  ret void

282:                                              ; preds = %145
  tail call void @putchar(i8 -35)
  ret void

283:                                              ; preds = %146
  tail call void @putchar(i8 -37)
  ret void

284:                                              ; preds = %146
  tail call void @putchar(i8 -39)
  ret void

285:                                              ; preds = %147
  tail call void @putchar(i8 -41)
  ret void

286:                                              ; preds = %147
  tail call void @putchar(i8 -43)
  ret void

287:                                              ; preds = %148
  tail call void @putchar(i8 -45)
  ret void

288:                                              ; preds = %148
  tail call void @putchar(i8 -47)
  ret void

289:                                              ; preds = %149
  tail call void @putchar(i8 -49)
  ret void

290:                                              ; preds = %149
  tail call void @putchar(i8 -51)
  ret void

291:                                              ; preds = %150
  tail call void @putchar(i8 -53)
  ret void

292:                                              ; preds = %150
  tail call void @putchar(i8 -55)
  ret void

293:                                              ; preds = %151
  tail call void @putchar(i8 -57)
  ret void

294:                                              ; preds = %151
  tail call void @putchar(i8 -59)
  ret void

295:                                              ; preds = %152
  tail call void @putchar(i8 -61)
  ret void

296:                                              ; preds = %152
  tail call void @putchar(i8 -63)
  ret void

297:                                              ; preds = %153
  tail call void @putchar(i8 -65)
  ret void

298:                                              ; preds = %153
  tail call void @putchar(i8 -67)
  ret void

299:                                              ; preds = %154
  tail call void @putchar(i8 -69)
  ret void

300:                                              ; preds = %154
  tail call void @putchar(i8 -71)
  ret void

301:                                              ; preds = %155
  tail call void @putchar(i8 -73)
  ret void

302:                                              ; preds = %155
  tail call void @putchar(i8 -75)
  ret void

303:                                              ; preds = %156
  tail call void @putchar(i8 -77)
  ret void

304:                                              ; preds = %156
  tail call void @putchar(i8 -79)
  ret void

305:                                              ; preds = %157
  tail call void @putchar(i8 -81)
  ret void

306:                                              ; preds = %157
  tail call void @putchar(i8 -83)
  ret void

307:                                              ; preds = %158
  tail call void @putchar(i8 -85)
  ret void

308:                                              ; preds = %158
  tail call void @putchar(i8 -87)
  ret void

309:                                              ; preds = %159
  tail call void @putchar(i8 -89)
  ret void

310:                                              ; preds = %159
  tail call void @putchar(i8 -91)
  ret void

311:                                              ; preds = %160
  tail call void @putchar(i8 -93)
  ret void

312:                                              ; preds = %160
  tail call void @putchar(i8 -95)
  ret void

313:                                              ; preds = %161
  tail call void @putchar(i8 -97)
  ret void

314:                                              ; preds = %161
  tail call void @putchar(i8 -99)
  ret void

315:                                              ; preds = %162
  tail call void @putchar(i8 -101)
  ret void

316:                                              ; preds = %162
  tail call void @putchar(i8 -103)
  ret void

317:                                              ; preds = %163
  tail call void @putchar(i8 -105)
  ret void

318:                                              ; preds = %163
  tail call void @putchar(i8 -107)
  ret void

319:                                              ; preds = %164
  tail call void @putchar(i8 -109)
  ret void

320:                                              ; preds = %164
  tail call void @putchar(i8 -111)
  ret void

321:                                              ; preds = %165
  tail call void @putchar(i8 -113)
  ret void

322:                                              ; preds = %165
  tail call void @putchar(i8 -115)
  ret void

323:                                              ; preds = %166
  tail call void @putchar(i8 -117)
  ret void

324:                                              ; preds = %166
  tail call void @putchar(i8 -119)
  ret void

325:                                              ; preds = %167
  tail call void @putchar(i8 -121)
  ret void

326:                                              ; preds = %167
  tail call void @putchar(i8 -123)
  ret void

327:                                              ; preds = %168
  tail call void @putchar(i8 -125)
  ret void

328:                                              ; preds = %168
  tail call void @putchar(i8 -127)
  ret void

329:                                              ; preds = %169
  tail call void @putchar(i8 127)
  ret void

330:                                              ; preds = %169
  tail call void @putchar(i8 125)
  ret void

331:                                              ; preds = %170
  tail call void @putchar(i8 123)
  ret void

332:                                              ; preds = %170
  tail call void @putchar(i8 121)
  ret void

333:                                              ; preds = %171
  tail call void @putchar(i8 119)
  ret void

334:                                              ; preds = %171
  tail call void @putchar(i8 117)
  ret void

335:                                              ; preds = %172
  tail call void @putchar(i8 115)
  ret void

336:                                              ; preds = %172
  tail call void @putchar(i8 113)
  ret void

337:                                              ; preds = %173
  tail call void @putchar(i8 111)
  ret void

338:                                              ; preds = %173
  tail call void @putchar(i8 109)
  ret void

339:                                              ; preds = %174
  tail call void @putchar(i8 107)
  ret void

340:                                              ; preds = %174
  tail call void @putchar(i8 105)
  ret void

341:                                              ; preds = %175
  tail call void @putchar(i8 103)
  ret void

342:                                              ; preds = %175
  tail call void @putchar(i8 101)
  ret void

343:                                              ; preds = %176
  tail call void @putchar(i8 99)
  ret void

344:                                              ; preds = %176
  tail call void @putchar(i8 97)
  ret void

345:                                              ; preds = %177
  tail call void @putchar(i8 95)
  ret void

346:                                              ; preds = %177
  tail call void @putchar(i8 93)
  ret void

347:                                              ; preds = %178
  tail call void @putchar(i8 91)
  ret void

348:                                              ; preds = %178
  tail call void @putchar(i8 89)
  ret void

349:                                              ; preds = %179
  tail call void @putchar(i8 87)
  ret void

350:                                              ; preds = %179
  tail call void @putchar(i8 85)
  ret void

351:                                              ; preds = %180
  tail call void @putchar(i8 83)
  ret void

352:                                              ; preds = %180
  tail call void @putchar(i8 81)
  ret void

353:                                              ; preds = %181
  tail call void @putchar(i8 79)
  ret void

354:                                              ; preds = %181
  tail call void @putchar(i8 77)
  ret void

355:                                              ; preds = %182
  tail call void @putchar(i8 75)
  ret void

356:                                              ; preds = %182
  tail call void @putchar(i8 73)
  ret void

357:                                              ; preds = %183
  tail call void @putchar(i8 71)
  ret void

358:                                              ; preds = %183
  tail call void @putchar(i8 69)
  ret void

359:                                              ; preds = %184
  tail call void @putchar(i8 67)
  ret void

360:                                              ; preds = %184
  tail call void @putchar(i8 65)
  ret void

361:                                              ; preds = %185
  tail call void @putchar(i8 63)
  ret void

362:                                              ; preds = %185
  tail call void @putchar(i8 61)
  ret void

363:                                              ; preds = %186
  tail call void @putchar(i8 59)
  ret void

364:                                              ; preds = %186
  tail call void @putchar(i8 57)
  ret void

365:                                              ; preds = %187
  tail call void @putchar(i8 55)
  ret void

366:                                              ; preds = %187
  tail call void @putchar(i8 53)
  ret void

367:                                              ; preds = %188
  tail call void @putchar(i8 51)
  ret void

368:                                              ; preds = %188
  tail call void @putchar(i8 49)
  ret void

369:                                              ; preds = %189
  tail call void @putchar(i8 47)
  ret void

370:                                              ; preds = %189
  tail call void @putchar(i8 45)
  ret void

371:                                              ; preds = %190
  tail call void @putchar(i8 43)
  ret void

372:                                              ; preds = %190
  tail call void @putchar(i8 41)
  ret void

373:                                              ; preds = %191
  tail call void @putchar(i8 39)
  ret void

374:                                              ; preds = %191
  tail call void @putchar(i8 37)
  ret void

375:                                              ; preds = %192
  tail call void @putchar(i8 35)
  ret void

376:                                              ; preds = %192
  tail call void @putchar(i8 33)
  ret void

377:                                              ; preds = %193
  tail call void @putchar(i8 31)
  ret void

378:                                              ; preds = %193
  tail call void @putchar(i8 29)
  ret void

379:                                              ; preds = %194
  tail call void @putchar(i8 27)
  ret void

380:                                              ; preds = %194
  tail call void @putchar(i8 25)
  ret void

381:                                              ; preds = %195
  tail call void @putchar(i8 23)
  ret void

382:                                              ; preds = %195
  tail call void @putchar(i8 21)
  ret void

383:                                              ; preds = %196
  tail call void @putchar(i8 19)
  ret void

384:                                              ; preds = %196
  tail call void @putchar(i8 17)
  ret void

385:                                              ; preds = %197
  tail call void @putchar(i8 15)
  ret void

386:                                              ; preds = %197
  tail call void @putchar(i8 13)
  ret void

387:                                              ; preds = %198
  tail call void @putchar(i8 11)
  ret void

388:                                              ; preds = %198
  tail call void @putchar(i8 9)
  ret void

389:                                              ; preds = %199
  tail call void @putchar(i8 7)
  ret void

390:                                              ; preds = %199
  tail call void @putchar(i8 5)
  ret void

391:                                              ; preds = %200
  tail call void @putchar(i8 3)
  ret void

392:                                              ; preds = %200
  tail call void @putchar(i8 1)
  ret void

393:                                              ; preds = %201
  tail call void @putchar(i8 -2)
  ret void

394:                                              ; preds = %201
  tail call void @putchar(i8 -4)
  ret void

395:                                              ; preds = %202
  tail call void @putchar(i8 -6)
  ret void

396:                                              ; preds = %202
  tail call void @putchar(i8 -8)
  ret void

397:                                              ; preds = %203
  tail call void @putchar(i8 -10)
  ret void

398:                                              ; preds = %203
  tail call void @putchar(i8 -12)
  ret void

399:                                              ; preds = %204
  tail call void @putchar(i8 -14)
  ret void

400:                                              ; preds = %204
  tail call void @putchar(i8 -16)
  ret void

401:                                              ; preds = %205
  tail call void @putchar(i8 -18)
  ret void

402:                                              ; preds = %205
  tail call void @putchar(i8 -20)
  ret void

403:                                              ; preds = %206
  tail call void @putchar(i8 -22)
  ret void

404:                                              ; preds = %206
  tail call void @putchar(i8 -24)
  ret void

405:                                              ; preds = %207
  tail call void @putchar(i8 -26)
  ret void

406:                                              ; preds = %207
  tail call void @putchar(i8 -28)
  ret void

407:                                              ; preds = %208
  tail call void @putchar(i8 -30)
  ret void

408:                                              ; preds = %208
  tail call void @putchar(i8 -32)
  ret void

409:                                              ; preds = %209
  tail call void @putchar(i8 -34)
  ret void

410:                                              ; preds = %209
  tail call void @putchar(i8 -36)
  ret void

411:                                              ; preds = %210
  tail call void @putchar(i8 -38)
  ret void

412:                                              ; preds = %210
  tail call void @putchar(i8 -40)
  ret void

413:                                              ; preds = %211
  tail call void @putchar(i8 -42)
  ret void

414:                                              ; preds = %211
  tail call void @putchar(i8 -44)
  ret void

415:                                              ; preds = %212
  tail call void @putchar(i8 -46)
  ret void

416:                                              ; preds = %212
  tail call void @putchar(i8 -48)
  ret void

417:                                              ; preds = %213
  tail call void @putchar(i8 -50)
  ret void

418:                                              ; preds = %213
  tail call void @putchar(i8 -52)
  ret void

419:                                              ; preds = %214
  tail call void @putchar(i8 -54)
  ret void

420:                                              ; preds = %214
  tail call void @putchar(i8 -56)
  ret void

421:                                              ; preds = %215
  tail call void @putchar(i8 -58)
  ret void

422:                                              ; preds = %215
  tail call void @putchar(i8 -60)
  ret void

423:                                              ; preds = %216
  tail call void @putchar(i8 -62)
  ret void

424:                                              ; preds = %216
  tail call void @putchar(i8 -64)
  ret void

425:                                              ; preds = %217
  tail call void @putchar(i8 -66)
  ret void

426:                                              ; preds = %217
  tail call void @putchar(i8 -68)
  ret void

427:                                              ; preds = %218
  tail call void @putchar(i8 -70)
  ret void

428:                                              ; preds = %218
  tail call void @putchar(i8 -72)
  ret void

429:                                              ; preds = %219
  tail call void @putchar(i8 -74)
  ret void

430:                                              ; preds = %219
  tail call void @putchar(i8 -76)
  ret void

431:                                              ; preds = %220
  tail call void @putchar(i8 -78)
  ret void

432:                                              ; preds = %220
  tail call void @putchar(i8 -80)
  ret void

433:                                              ; preds = %221
  tail call void @putchar(i8 -82)
  ret void

434:                                              ; preds = %221
  tail call void @putchar(i8 -84)
  ret void

435:                                              ; preds = %222
  tail call void @putchar(i8 -86)
  ret void

436:                                              ; preds = %222
  tail call void @putchar(i8 -88)
  ret void

437:                                              ; preds = %223
  tail call void @putchar(i8 -90)
  ret void

438:                                              ; preds = %223
  tail call void @putchar(i8 -92)
  ret void

439:                                              ; preds = %224
  tail call void @putchar(i8 -94)
  ret void

440:                                              ; preds = %224
  tail call void @putchar(i8 -96)
  ret void

441:                                              ; preds = %225
  tail call void @putchar(i8 -98)
  ret void

442:                                              ; preds = %225
  tail call void @putchar(i8 -100)
  ret void

443:                                              ; preds = %226
  tail call void @putchar(i8 -102)
  ret void

444:                                              ; preds = %226
  tail call void @putchar(i8 -104)
  ret void

445:                                              ; preds = %227
  tail call void @putchar(i8 -106)
  ret void

446:                                              ; preds = %227
  tail call void @putchar(i8 -108)
  ret void

447:                                              ; preds = %228
  tail call void @putchar(i8 -110)
  ret void

448:                                              ; preds = %228
  tail call void @putchar(i8 -112)
  ret void

449:                                              ; preds = %229
  tail call void @putchar(i8 -114)
  ret void

450:                                              ; preds = %229
  tail call void @putchar(i8 -116)
  ret void

451:                                              ; preds = %230
  tail call void @putchar(i8 -118)
  ret void

452:                                              ; preds = %230
  tail call void @putchar(i8 -120)
  ret void

453:                                              ; preds = %231
  tail call void @putchar(i8 -122)
  ret void

454:                                              ; preds = %231
  tail call void @putchar(i8 -124)
  ret void

455:                                              ; preds = %232
  tail call void @putchar(i8 -126)
  ret void

456:                                              ; preds = %232
  tail call void @putchar(i8 -128)
  ret void

457:                                              ; preds = %233
  tail call void @putchar(i8 126)
  ret void

458:                                              ; preds = %233
  tail call void @putchar(i8 124)
  ret void

459:                                              ; preds = %234
  tail call void @putchar(i8 122)
  ret void

460:                                              ; preds = %234
  tail call void @putchar(i8 120)
  ret void

461:                                              ; preds = %235
  tail call void @putchar(i8 118)
  ret void

462:                                              ; preds = %235
  tail call void @putchar(i8 116)
  ret void

463:                                              ; preds = %236
  tail call void @putchar(i8 114)
  ret void

464:                                              ; preds = %236
  tail call void @putchar(i8 112)
  ret void

465:                                              ; preds = %237
  tail call void @putchar(i8 110)
  ret void

466:                                              ; preds = %237
  tail call void @putchar(i8 108)
  ret void

467:                                              ; preds = %238
  tail call void @putchar(i8 106)
  ret void

468:                                              ; preds = %238
  tail call void @putchar(i8 104)
  ret void

469:                                              ; preds = %239
  tail call void @putchar(i8 102)
  ret void

470:                                              ; preds = %239
  tail call void @putchar(i8 100)
  ret void

471:                                              ; preds = %240
  tail call void @putchar(i8 98)
  ret void

472:                                              ; preds = %240
  tail call void @putchar(i8 96)
  ret void

473:                                              ; preds = %241
  tail call void @putchar(i8 94)
  ret void

474:                                              ; preds = %241
  tail call void @putchar(i8 92)
  ret void

475:                                              ; preds = %242
  tail call void @putchar(i8 90)
  ret void

476:                                              ; preds = %242
  tail call void @putchar(i8 88)
  ret void

477:                                              ; preds = %243
  tail call void @putchar(i8 86)
  ret void

478:                                              ; preds = %243
  tail call void @putchar(i8 84)
  ret void

479:                                              ; preds = %244
  tail call void @putchar(i8 82)
  ret void

480:                                              ; preds = %244
  tail call void @putchar(i8 80)
  ret void

481:                                              ; preds = %245
  tail call void @putchar(i8 78)
  ret void

482:                                              ; preds = %245
  tail call void @putchar(i8 76)
  ret void

483:                                              ; preds = %246
  tail call void @putchar(i8 74)
  ret void

484:                                              ; preds = %246
  tail call void @putchar(i8 72)
  ret void

485:                                              ; preds = %247
  tail call void @putchar(i8 70)
  ret void

486:                                              ; preds = %247
  tail call void @putchar(i8 68)
  ret void

487:                                              ; preds = %248
  tail call void @putchar(i8 66)
  ret void

488:                                              ; preds = %248
  tail call void @putchar(i8 64)
  ret void

489:                                              ; preds = %249
  tail call void @putchar(i8 62)
  ret void

490:                                              ; preds = %249
  tail call void @putchar(i8 60)
  ret void

491:                                              ; preds = %250
  tail call void @putchar(i8 58)
  ret void

492:                                              ; preds = %250
  tail call void @putchar(i8 56)
  ret void

493:                                              ; preds = %251
  tail call void @putchar(i8 54)
  ret void

494:                                              ; preds = %251
  tail call void @putchar(i8 52)
  ret void

495:                                              ; preds = %252
  tail call void @putchar(i8 50)
  ret void

496:                                              ; preds = %252
  tail call void @putchar(i8 48)
  ret void

497:                                              ; preds = %253
  tail call void @putchar(i8 46)
  ret void

498:                                              ; preds = %253
  tail call void @putchar(i8 44)
  ret void

499:                                              ; preds = %254
  tail call void @putchar(i8 42)
  ret void

500:                                              ; preds = %254
  tail call void @putchar(i8 40)
  ret void

501:                                              ; preds = %255
  tail call void @putchar(i8 38)
  ret void

502:                                              ; preds = %255
  tail call void @putchar(i8 36)
  ret void

503:                                              ; preds = %256
  tail call void @putchar(i8 34)
  ret void

504:                                              ; preds = %256
  tail call void @putchar(i8 32)
  ret void

505:                                              ; preds = %257
  tail call void @putchar(i8 30)
  ret void

506:                                              ; preds = %257
  tail call void @putchar(i8 28)
  ret void

507:                                              ; preds = %258
  tail call void @putchar(i8 26)
  ret void

508:                                              ; preds = %258
  tail call void @putchar(i8 24)
  ret void

509:                                              ; preds = %259
  tail call void @putchar(i8 22)
  ret void

510:                                              ; preds = %259
  tail call void @putchar(i8 20)
  ret void

511:                                              ; preds = %260
  tail call void @putchar(i8 18)
  ret void

512:                                              ; preds = %260
  tail call void @putchar(i8 16)
  ret void

513:                                              ; preds = %261
  tail call void @putchar(i8 14)
  ret void

514:                                              ; preds = %261
  tail call void @putchar(i8 12)
  ret void

515:                                              ; preds = %262
  tail call void @putchar(i8 10)
  ret void

516:                                              ; preds = %262
  tail call void @putchar(i8 8)
  ret void

517:                                              ; preds = %263
  tail call void @putchar(i8 6)
  ret void

518:                                              ; preds = %263
  tail call void @putchar(i8 4)
  ret void

519:                                              ; preds = %264
  tail call void @putchar(i8 2)
  ret void

520:                                              ; preds = %264
  tail call void @putchar(i8 0)
  ret void
}

define void @main() local_unnamed_addr {
  %1 = tail call i8 @getchar()
  %2 = icmp sgt i8 %1, -1
  %3 = and i8 %1, 64
  %4 = icmp eq i8 %3, 0
  %5 = and i8 %1, 32
  %6 = icmp eq i8 %5, 0
  br i1 %2, label %12, label %7

7:                                                ; preds = %0
  br i1 %4, label %10, label %8

8:                                                ; preds = %7
  br i1 %6, label %9, label %codeRepl.i.i

codeRepl.i.i:                                     ; preds = %8
  tail call fastcc void @__elem_2(i1 true, i8 %1, i1 true, i1 true)
  br label %__elem_0.exit

9:                                                ; preds = %8
  tail call fastcc void @__elem_2(i1 false, i8 %1, i1 true, i1 true)
  br label %__elem_0.exit

10:                                               ; preds = %7
  br i1 %6, label %11, label %codeRepl.i1.i

codeRepl.i1.i:                                    ; preds = %10
  tail call fastcc void @__elem_2(i1 true, i8 %1, i1 true, i1 false)
  br label %__elem_0.exit

11:                                               ; preds = %10
  tail call fastcc void @__elem_2(i1 false, i8 %1, i1 true, i1 false)
  br label %__elem_0.exit

__elem_0.exit:                                    ; preds = %codeRepl.i1.i3, %16, %codeRepl.i.i1, %14, %codeRepl.i1.i, %11, %codeRepl.i.i, %9
  ret void

12:                                               ; preds = %0
  br i1 %4, label %15, label %13

13:                                               ; preds = %12
  br i1 %6, label %14, label %codeRepl.i.i1

codeRepl.i.i1:                                    ; preds = %13
  tail call fastcc void @__elem_2(i1 true, i8 %1, i1 false, i1 true)
  br label %__elem_0.exit

14:                                               ; preds = %13
  tail call fastcc void @__elem_2(i1 false, i8 %1, i1 false, i1 true)
  br label %__elem_0.exit

15:                                               ; preds = %12
  br i1 %6, label %16, label %codeRepl.i1.i3

codeRepl.i1.i3:                                   ; preds = %15
  tail call fastcc void @__elem_2(i1 true, i8 %1, i1 false, i1 false)
  br label %__elem_0.exit

16:                                               ; preds = %15
  tail call fastcc void @__elem_2(i1 false, i8 %1, i1 false, i1 false)
  br label %__elem_0.exit
}

attributes #0 = { nofree nounwind }
