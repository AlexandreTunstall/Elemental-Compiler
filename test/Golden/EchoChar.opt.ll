; ModuleID = '<string>'
source_filename = "test/Golden/EchoChar.elem"

declare void @putchar(i8) local_unnamed_addr

; Function Attrs: nofree nounwind
declare i8 @getchar() local_unnamed_addr #0

define void @main() local_unnamed_addr {
  %1 = tail call i8 @getchar()
  %2 = and i8 %1, 1
  %3 = icmp eq i8 %2, 0
  %4 = and i8 %1, 2
  %5 = icmp eq i8 %4, 0
  %6 = and i8 %1, 4
  %7 = icmp eq i8 %6, 0
  %8 = and i8 %1, 8
  %9 = icmp eq i8 %8, 0
  %10 = and i8 %1, 16
  %11 = icmp eq i8 %10, 0
  %12 = and i8 %1, 32
  %13 = icmp eq i8 %12, 0
  %14 = and i8 %1, 64
  %15 = icmp eq i8 %14, 0
  %16 = icmp sgt i8 %1, -1
  br i1 %3, label %144, label %17

17:                                               ; preds = %0
  br i1 %5, label %81, label %18

18:                                               ; preds = %17
  br i1 %7, label %50, label %19

19:                                               ; preds = %18
  br i1 %9, label %35, label %20

20:                                               ; preds = %19
  br i1 %11, label %28, label %21

21:                                               ; preds = %20
  br i1 %13, label %25, label %22

22:                                               ; preds = %21
  br i1 %15, label %24, label %23

23:                                               ; preds = %22
  %. = select i1 %16, i8 127, i8 -1
  br label %272

24:                                               ; preds = %22
  %.1 = select i1 %16, i8 63, i8 -65
  br label %272

25:                                               ; preds = %21
  br i1 %15, label %27, label %26

26:                                               ; preds = %25
  %.2 = select i1 %16, i8 95, i8 -33
  br label %272

27:                                               ; preds = %25
  %.3 = select i1 %16, i8 31, i8 -97
  br label %272

28:                                               ; preds = %20
  br i1 %13, label %32, label %29

29:                                               ; preds = %28
  br i1 %15, label %31, label %30

30:                                               ; preds = %29
  %.4 = select i1 %16, i8 111, i8 -17
  br label %272

31:                                               ; preds = %29
  %.5 = select i1 %16, i8 47, i8 -81
  br label %272

32:                                               ; preds = %28
  br i1 %15, label %34, label %33

33:                                               ; preds = %32
  %.6 = select i1 %16, i8 79, i8 -49
  br label %272

34:                                               ; preds = %32
  %.7 = select i1 %16, i8 15, i8 -113
  br label %272

35:                                               ; preds = %19
  br i1 %11, label %43, label %36

36:                                               ; preds = %35
  br i1 %13, label %40, label %37

37:                                               ; preds = %36
  br i1 %15, label %39, label %38

38:                                               ; preds = %37
  %.8 = select i1 %16, i8 119, i8 -9
  br label %272

39:                                               ; preds = %37
  %.9 = select i1 %16, i8 55, i8 -73
  br label %272

40:                                               ; preds = %36
  br i1 %15, label %42, label %41

41:                                               ; preds = %40
  %.10 = select i1 %16, i8 87, i8 -41
  br label %272

42:                                               ; preds = %40
  %.11 = select i1 %16, i8 23, i8 -105
  br label %272

43:                                               ; preds = %35
  br i1 %13, label %47, label %44

44:                                               ; preds = %43
  br i1 %15, label %46, label %45

45:                                               ; preds = %44
  %.12 = select i1 %16, i8 103, i8 -25
  br label %272

46:                                               ; preds = %44
  %.13 = select i1 %16, i8 39, i8 -89
  br label %272

47:                                               ; preds = %43
  br i1 %15, label %49, label %48

48:                                               ; preds = %47
  %.14 = select i1 %16, i8 71, i8 -57
  br label %272

49:                                               ; preds = %47
  %.15 = select i1 %16, i8 7, i8 -121
  br label %272

50:                                               ; preds = %18
  br i1 %9, label %66, label %51

51:                                               ; preds = %50
  br i1 %11, label %59, label %52

52:                                               ; preds = %51
  br i1 %13, label %56, label %53

53:                                               ; preds = %52
  br i1 %15, label %55, label %54

54:                                               ; preds = %53
  %.16 = select i1 %16, i8 123, i8 -5
  br label %272

55:                                               ; preds = %53
  %.17 = select i1 %16, i8 59, i8 -69
  br label %272

56:                                               ; preds = %52
  br i1 %15, label %58, label %57

57:                                               ; preds = %56
  %.18 = select i1 %16, i8 91, i8 -37
  br label %272

58:                                               ; preds = %56
  %.19 = select i1 %16, i8 27, i8 -101
  br label %272

59:                                               ; preds = %51
  br i1 %13, label %63, label %60

60:                                               ; preds = %59
  br i1 %15, label %62, label %61

61:                                               ; preds = %60
  %.20 = select i1 %16, i8 107, i8 -21
  br label %272

62:                                               ; preds = %60
  %.21 = select i1 %16, i8 43, i8 -85
  br label %272

63:                                               ; preds = %59
  br i1 %15, label %65, label %64

64:                                               ; preds = %63
  %.22 = select i1 %16, i8 75, i8 -53
  br label %272

65:                                               ; preds = %63
  %.23 = select i1 %16, i8 11, i8 -117
  br label %272

66:                                               ; preds = %50
  br i1 %11, label %74, label %67

67:                                               ; preds = %66
  br i1 %13, label %71, label %68

68:                                               ; preds = %67
  br i1 %15, label %70, label %69

69:                                               ; preds = %68
  %.24 = select i1 %16, i8 115, i8 -13
  br label %272

70:                                               ; preds = %68
  %.25 = select i1 %16, i8 51, i8 -77
  br label %272

71:                                               ; preds = %67
  br i1 %15, label %73, label %72

72:                                               ; preds = %71
  %.26 = select i1 %16, i8 83, i8 -45
  br label %272

73:                                               ; preds = %71
  %.27 = select i1 %16, i8 19, i8 -109
  br label %272

74:                                               ; preds = %66
  br i1 %13, label %78, label %75

75:                                               ; preds = %74
  br i1 %15, label %77, label %76

76:                                               ; preds = %75
  %.28 = select i1 %16, i8 99, i8 -29
  br label %272

77:                                               ; preds = %75
  %.29 = select i1 %16, i8 35, i8 -93
  br label %272

78:                                               ; preds = %74
  br i1 %15, label %80, label %79

79:                                               ; preds = %78
  %.30 = select i1 %16, i8 67, i8 -61
  br label %272

80:                                               ; preds = %78
  %.31 = select i1 %16, i8 3, i8 -125
  br label %272

81:                                               ; preds = %17
  br i1 %7, label %113, label %82

82:                                               ; preds = %81
  br i1 %9, label %98, label %83

83:                                               ; preds = %82
  br i1 %11, label %91, label %84

84:                                               ; preds = %83
  br i1 %13, label %88, label %85

85:                                               ; preds = %84
  br i1 %15, label %87, label %86

86:                                               ; preds = %85
  %.32 = select i1 %16, i8 125, i8 -3
  br label %272

87:                                               ; preds = %85
  %.33 = select i1 %16, i8 61, i8 -67
  br label %272

88:                                               ; preds = %84
  br i1 %15, label %90, label %89

89:                                               ; preds = %88
  %.34 = select i1 %16, i8 93, i8 -35
  br label %272

90:                                               ; preds = %88
  %.35 = select i1 %16, i8 29, i8 -99
  br label %272

91:                                               ; preds = %83
  br i1 %13, label %95, label %92

92:                                               ; preds = %91
  br i1 %15, label %94, label %93

93:                                               ; preds = %92
  %.36 = select i1 %16, i8 109, i8 -19
  br label %272

94:                                               ; preds = %92
  %.37 = select i1 %16, i8 45, i8 -83
  br label %272

95:                                               ; preds = %91
  br i1 %15, label %97, label %96

96:                                               ; preds = %95
  %.38 = select i1 %16, i8 77, i8 -51
  br label %272

97:                                               ; preds = %95
  %.39 = select i1 %16, i8 13, i8 -115
  br label %272

98:                                               ; preds = %82
  br i1 %11, label %106, label %99

99:                                               ; preds = %98
  br i1 %13, label %103, label %100

100:                                              ; preds = %99
  br i1 %15, label %102, label %101

101:                                              ; preds = %100
  %.40 = select i1 %16, i8 117, i8 -11
  br label %272

102:                                              ; preds = %100
  %.41 = select i1 %16, i8 53, i8 -75
  br label %272

103:                                              ; preds = %99
  br i1 %15, label %105, label %104

104:                                              ; preds = %103
  %.42 = select i1 %16, i8 85, i8 -43
  br label %272

105:                                              ; preds = %103
  %.43 = select i1 %16, i8 21, i8 -107
  br label %272

106:                                              ; preds = %98
  br i1 %13, label %110, label %107

107:                                              ; preds = %106
  br i1 %15, label %109, label %108

108:                                              ; preds = %107
  %.44 = select i1 %16, i8 101, i8 -27
  br label %272

109:                                              ; preds = %107
  %.45 = select i1 %16, i8 37, i8 -91
  br label %272

110:                                              ; preds = %106
  br i1 %15, label %112, label %111

111:                                              ; preds = %110
  %.46 = select i1 %16, i8 69, i8 -59
  br label %272

112:                                              ; preds = %110
  %.47 = select i1 %16, i8 5, i8 -123
  br label %272

113:                                              ; preds = %81
  br i1 %9, label %129, label %114

114:                                              ; preds = %113
  br i1 %11, label %122, label %115

115:                                              ; preds = %114
  br i1 %13, label %119, label %116

116:                                              ; preds = %115
  br i1 %15, label %118, label %117

117:                                              ; preds = %116
  %.48 = select i1 %16, i8 121, i8 -7
  br label %272

118:                                              ; preds = %116
  %.49 = select i1 %16, i8 57, i8 -71
  br label %272

119:                                              ; preds = %115
  br i1 %15, label %121, label %120

120:                                              ; preds = %119
  %.50 = select i1 %16, i8 89, i8 -39
  br label %272

121:                                              ; preds = %119
  %.51 = select i1 %16, i8 25, i8 -103
  br label %272

122:                                              ; preds = %114
  br i1 %13, label %126, label %123

123:                                              ; preds = %122
  br i1 %15, label %125, label %124

124:                                              ; preds = %123
  %.52 = select i1 %16, i8 105, i8 -23
  br label %272

125:                                              ; preds = %123
  %.53 = select i1 %16, i8 41, i8 -87
  br label %272

126:                                              ; preds = %122
  br i1 %15, label %128, label %127

127:                                              ; preds = %126
  %.54 = select i1 %16, i8 73, i8 -55
  br label %272

128:                                              ; preds = %126
  %.55 = select i1 %16, i8 9, i8 -119
  br label %272

129:                                              ; preds = %113
  br i1 %11, label %137, label %130

130:                                              ; preds = %129
  br i1 %13, label %134, label %131

131:                                              ; preds = %130
  br i1 %15, label %133, label %132

132:                                              ; preds = %131
  %.56 = select i1 %16, i8 113, i8 -15
  br label %272

133:                                              ; preds = %131
  %.57 = select i1 %16, i8 49, i8 -79
  br label %272

134:                                              ; preds = %130
  br i1 %15, label %136, label %135

135:                                              ; preds = %134
  %.58 = select i1 %16, i8 81, i8 -47
  br label %272

136:                                              ; preds = %134
  %.59 = select i1 %16, i8 17, i8 -111
  br label %272

137:                                              ; preds = %129
  br i1 %13, label %141, label %138

138:                                              ; preds = %137
  br i1 %15, label %140, label %139

139:                                              ; preds = %138
  %.60 = select i1 %16, i8 97, i8 -31
  br label %272

140:                                              ; preds = %138
  %.61 = select i1 %16, i8 33, i8 -95
  br label %272

141:                                              ; preds = %137
  br i1 %15, label %143, label %142

142:                                              ; preds = %141
  %.62 = select i1 %16, i8 65, i8 -63
  br label %272

143:                                              ; preds = %141
  %.63 = select i1 %16, i8 1, i8 -127
  br label %272

144:                                              ; preds = %0
  br i1 %5, label %208, label %145

145:                                              ; preds = %144
  br i1 %7, label %177, label %146

146:                                              ; preds = %145
  br i1 %9, label %162, label %147

147:                                              ; preds = %146
  br i1 %11, label %155, label %148

148:                                              ; preds = %147
  br i1 %13, label %152, label %149

149:                                              ; preds = %148
  br i1 %15, label %151, label %150

150:                                              ; preds = %149
  %.64 = select i1 %16, i8 126, i8 -2
  br label %272

151:                                              ; preds = %149
  %.65 = select i1 %16, i8 62, i8 -66
  br label %272

152:                                              ; preds = %148
  br i1 %15, label %154, label %153

153:                                              ; preds = %152
  %.66 = select i1 %16, i8 94, i8 -34
  br label %272

154:                                              ; preds = %152
  %.67 = select i1 %16, i8 30, i8 -98
  br label %272

155:                                              ; preds = %147
  br i1 %13, label %159, label %156

156:                                              ; preds = %155
  br i1 %15, label %158, label %157

157:                                              ; preds = %156
  %.68 = select i1 %16, i8 110, i8 -18
  br label %272

158:                                              ; preds = %156
  %.69 = select i1 %16, i8 46, i8 -82
  br label %272

159:                                              ; preds = %155
  br i1 %15, label %161, label %160

160:                                              ; preds = %159
  %.70 = select i1 %16, i8 78, i8 -50
  br label %272

161:                                              ; preds = %159
  %.71 = select i1 %16, i8 14, i8 -114
  br label %272

162:                                              ; preds = %146
  br i1 %11, label %170, label %163

163:                                              ; preds = %162
  br i1 %13, label %167, label %164

164:                                              ; preds = %163
  br i1 %15, label %166, label %165

165:                                              ; preds = %164
  %.72 = select i1 %16, i8 118, i8 -10
  br label %272

166:                                              ; preds = %164
  %.73 = select i1 %16, i8 54, i8 -74
  br label %272

167:                                              ; preds = %163
  br i1 %15, label %169, label %168

168:                                              ; preds = %167
  %.74 = select i1 %16, i8 86, i8 -42
  br label %272

169:                                              ; preds = %167
  %.75 = select i1 %16, i8 22, i8 -106
  br label %272

170:                                              ; preds = %162
  br i1 %13, label %174, label %171

171:                                              ; preds = %170
  br i1 %15, label %173, label %172

172:                                              ; preds = %171
  %.76 = select i1 %16, i8 102, i8 -26
  br label %272

173:                                              ; preds = %171
  %.77 = select i1 %16, i8 38, i8 -90
  br label %272

174:                                              ; preds = %170
  br i1 %15, label %176, label %175

175:                                              ; preds = %174
  %.78 = select i1 %16, i8 70, i8 -58
  br label %272

176:                                              ; preds = %174
  %.79 = select i1 %16, i8 6, i8 -122
  br label %272

177:                                              ; preds = %145
  br i1 %9, label %193, label %178

178:                                              ; preds = %177
  br i1 %11, label %186, label %179

179:                                              ; preds = %178
  br i1 %13, label %183, label %180

180:                                              ; preds = %179
  br i1 %15, label %182, label %181

181:                                              ; preds = %180
  %.80 = select i1 %16, i8 122, i8 -6
  br label %272

182:                                              ; preds = %180
  %.81 = select i1 %16, i8 58, i8 -70
  br label %272

183:                                              ; preds = %179
  br i1 %15, label %185, label %184

184:                                              ; preds = %183
  %.82 = select i1 %16, i8 90, i8 -38
  br label %272

185:                                              ; preds = %183
  %.83 = select i1 %16, i8 26, i8 -102
  br label %272

186:                                              ; preds = %178
  br i1 %13, label %190, label %187

187:                                              ; preds = %186
  br i1 %15, label %189, label %188

188:                                              ; preds = %187
  %.84 = select i1 %16, i8 106, i8 -22
  br label %272

189:                                              ; preds = %187
  %.85 = select i1 %16, i8 42, i8 -86
  br label %272

190:                                              ; preds = %186
  br i1 %15, label %192, label %191

191:                                              ; preds = %190
  %.86 = select i1 %16, i8 74, i8 -54
  br label %272

192:                                              ; preds = %190
  %.87 = select i1 %16, i8 10, i8 -118
  br label %272

193:                                              ; preds = %177
  br i1 %11, label %201, label %194

194:                                              ; preds = %193
  br i1 %13, label %198, label %195

195:                                              ; preds = %194
  br i1 %15, label %197, label %196

196:                                              ; preds = %195
  %.88 = select i1 %16, i8 114, i8 -14
  br label %272

197:                                              ; preds = %195
  %.89 = select i1 %16, i8 50, i8 -78
  br label %272

198:                                              ; preds = %194
  br i1 %15, label %200, label %199

199:                                              ; preds = %198
  %.90 = select i1 %16, i8 82, i8 -46
  br label %272

200:                                              ; preds = %198
  %.91 = select i1 %16, i8 18, i8 -110
  br label %272

201:                                              ; preds = %193
  br i1 %13, label %205, label %202

202:                                              ; preds = %201
  br i1 %15, label %204, label %203

203:                                              ; preds = %202
  %.92 = select i1 %16, i8 98, i8 -30
  br label %272

204:                                              ; preds = %202
  %.93 = select i1 %16, i8 34, i8 -94
  br label %272

205:                                              ; preds = %201
  br i1 %15, label %207, label %206

206:                                              ; preds = %205
  %.94 = select i1 %16, i8 66, i8 -62
  br label %272

207:                                              ; preds = %205
  %.95 = select i1 %16, i8 2, i8 -126
  br label %272

208:                                              ; preds = %144
  br i1 %7, label %240, label %209

209:                                              ; preds = %208
  br i1 %9, label %225, label %210

210:                                              ; preds = %209
  br i1 %11, label %218, label %211

211:                                              ; preds = %210
  br i1 %13, label %215, label %212

212:                                              ; preds = %211
  br i1 %15, label %214, label %213

213:                                              ; preds = %212
  %.96 = select i1 %16, i8 124, i8 -4
  br label %272

214:                                              ; preds = %212
  %.97 = select i1 %16, i8 60, i8 -68
  br label %272

215:                                              ; preds = %211
  br i1 %15, label %217, label %216

216:                                              ; preds = %215
  %.98 = select i1 %16, i8 92, i8 -36
  br label %272

217:                                              ; preds = %215
  %.99 = select i1 %16, i8 28, i8 -100
  br label %272

218:                                              ; preds = %210
  br i1 %13, label %222, label %219

219:                                              ; preds = %218
  br i1 %15, label %221, label %220

220:                                              ; preds = %219
  %.100 = select i1 %16, i8 108, i8 -20
  br label %272

221:                                              ; preds = %219
  %.101 = select i1 %16, i8 44, i8 -84
  br label %272

222:                                              ; preds = %218
  br i1 %15, label %224, label %223

223:                                              ; preds = %222
  %.102 = select i1 %16, i8 76, i8 -52
  br label %272

224:                                              ; preds = %222
  %.103 = select i1 %16, i8 12, i8 -116
  br label %272

225:                                              ; preds = %209
  br i1 %11, label %233, label %226

226:                                              ; preds = %225
  br i1 %13, label %230, label %227

227:                                              ; preds = %226
  br i1 %15, label %229, label %228

228:                                              ; preds = %227
  %.104 = select i1 %16, i8 116, i8 -12
  br label %272

229:                                              ; preds = %227
  %.105 = select i1 %16, i8 52, i8 -76
  br label %272

230:                                              ; preds = %226
  br i1 %15, label %232, label %231

231:                                              ; preds = %230
  %.106 = select i1 %16, i8 84, i8 -44
  br label %272

232:                                              ; preds = %230
  %.107 = select i1 %16, i8 20, i8 -108
  br label %272

233:                                              ; preds = %225
  br i1 %13, label %237, label %234

234:                                              ; preds = %233
  br i1 %15, label %236, label %235

235:                                              ; preds = %234
  %.108 = select i1 %16, i8 100, i8 -28
  br label %272

236:                                              ; preds = %234
  %.109 = select i1 %16, i8 36, i8 -92
  br label %272

237:                                              ; preds = %233
  br i1 %15, label %239, label %238

238:                                              ; preds = %237
  %.110 = select i1 %16, i8 68, i8 -60
  br label %272

239:                                              ; preds = %237
  %.111 = select i1 %16, i8 4, i8 -124
  br label %272

240:                                              ; preds = %208
  br i1 %9, label %256, label %241

241:                                              ; preds = %240
  br i1 %11, label %249, label %242

242:                                              ; preds = %241
  br i1 %13, label %246, label %243

243:                                              ; preds = %242
  br i1 %15, label %245, label %244

244:                                              ; preds = %243
  %.112 = select i1 %16, i8 120, i8 -8
  br label %272

245:                                              ; preds = %243
  %.113 = select i1 %16, i8 56, i8 -72
  br label %272

246:                                              ; preds = %242
  br i1 %15, label %248, label %247

247:                                              ; preds = %246
  %.114 = select i1 %16, i8 88, i8 -40
  br label %272

248:                                              ; preds = %246
  %.115 = select i1 %16, i8 24, i8 -104
  br label %272

249:                                              ; preds = %241
  br i1 %13, label %253, label %250

250:                                              ; preds = %249
  br i1 %15, label %252, label %251

251:                                              ; preds = %250
  %.116 = select i1 %16, i8 104, i8 -24
  br label %272

252:                                              ; preds = %250
  %.117 = select i1 %16, i8 40, i8 -88
  br label %272

253:                                              ; preds = %249
  br i1 %15, label %255, label %254

254:                                              ; preds = %253
  %.118 = select i1 %16, i8 72, i8 -56
  br label %272

255:                                              ; preds = %253
  %.119 = select i1 %16, i8 8, i8 -120
  br label %272

256:                                              ; preds = %240
  br i1 %11, label %264, label %257

257:                                              ; preds = %256
  br i1 %13, label %261, label %258

258:                                              ; preds = %257
  br i1 %15, label %260, label %259

259:                                              ; preds = %258
  %.120 = select i1 %16, i8 112, i8 -16
  br label %272

260:                                              ; preds = %258
  %.121 = select i1 %16, i8 48, i8 -80
  br label %272

261:                                              ; preds = %257
  br i1 %15, label %263, label %262

262:                                              ; preds = %261
  %.122 = select i1 %16, i8 80, i8 -48
  br label %272

263:                                              ; preds = %261
  %.123 = select i1 %16, i8 16, i8 -112
  br label %272

264:                                              ; preds = %256
  br i1 %13, label %268, label %265

265:                                              ; preds = %264
  br i1 %15, label %267, label %266

266:                                              ; preds = %265
  %.124 = select i1 %16, i8 96, i8 -32
  br label %272

267:                                              ; preds = %265
  %.125 = select i1 %16, i8 32, i8 -96
  br label %272

268:                                              ; preds = %264
  br i1 %15, label %270, label %269

269:                                              ; preds = %268
  %.126 = select i1 %16, i8 64, i8 -64
  br label %272

270:                                              ; preds = %268
  %271 = and i8 %1, -128
  br label %272

272:                                              ; preds = %270, %269, %267, %266, %263, %262, %260, %259, %255, %254, %252, %251, %248, %247, %245, %244, %239, %238, %236, %235, %232, %231, %229, %228, %224, %223, %221, %220, %217, %216, %214, %213, %207, %206, %204, %203, %200, %199, %197, %196, %192, %191, %189, %188, %185, %184, %182, %181, %176, %175, %173, %172, %169, %168, %166, %165, %161, %160, %158, %157, %154, %153, %151, %150, %143, %142, %140, %139, %136, %135, %133, %132, %128, %127, %125, %124, %121, %120, %118, %117, %112, %111, %109, %108, %105, %104, %102, %101, %97, %96, %94, %93, %90, %89, %87, %86, %80, %79, %77, %76, %73, %72, %70, %69, %65, %64, %62, %61, %58, %57, %55, %54, %49, %48, %46, %45, %42, %41, %39, %38, %34, %33, %31, %30, %27, %26, %24, %23
  %.sink = phi i8 [ %., %23 ], [ %.1, %24 ], [ %.2, %26 ], [ %.3, %27 ], [ %.4, %30 ], [ %.5, %31 ], [ %.6, %33 ], [ %.7, %34 ], [ %.8, %38 ], [ %.9, %39 ], [ %.10, %41 ], [ %.11, %42 ], [ %.12, %45 ], [ %.13, %46 ], [ %.14, %48 ], [ %.15, %49 ], [ %.16, %54 ], [ %.17, %55 ], [ %.18, %57 ], [ %.19, %58 ], [ %.20, %61 ], [ %.21, %62 ], [ %.22, %64 ], [ %.23, %65 ], [ %.24, %69 ], [ %.25, %70 ], [ %.26, %72 ], [ %.27, %73 ], [ %.28, %76 ], [ %.29, %77 ], [ %.30, %79 ], [ %.31, %80 ], [ %.32, %86 ], [ %.33, %87 ], [ %.34, %89 ], [ %.35, %90 ], [ %.36, %93 ], [ %.37, %94 ], [ %.38, %96 ], [ %.39, %97 ], [ %.40, %101 ], [ %.41, %102 ], [ %.42, %104 ], [ %.43, %105 ], [ %.44, %108 ], [ %.45, %109 ], [ %.46, %111 ], [ %.47, %112 ], [ %.48, %117 ], [ %.49, %118 ], [ %.50, %120 ], [ %.51, %121 ], [ %.52, %124 ], [ %.53, %125 ], [ %.54, %127 ], [ %.55, %128 ], [ %.56, %132 ], [ %.57, %133 ], [ %.58, %135 ], [ %.59, %136 ], [ %.60, %139 ], [ %.61, %140 ], [ %.62, %142 ], [ %.63, %143 ], [ %.64, %150 ], [ %.65, %151 ], [ %.66, %153 ], [ %.67, %154 ], [ %.68, %157 ], [ %.69, %158 ], [ %.70, %160 ], [ %.71, %161 ], [ %.72, %165 ], [ %.73, %166 ], [ %.74, %168 ], [ %.75, %169 ], [ %.76, %172 ], [ %.77, %173 ], [ %.78, %175 ], [ %.79, %176 ], [ %.80, %181 ], [ %.81, %182 ], [ %.82, %184 ], [ %.83, %185 ], [ %.84, %188 ], [ %.85, %189 ], [ %.86, %191 ], [ %.87, %192 ], [ %.88, %196 ], [ %.89, %197 ], [ %.90, %199 ], [ %.91, %200 ], [ %.92, %203 ], [ %.93, %204 ], [ %.94, %206 ], [ %.95, %207 ], [ %.96, %213 ], [ %.97, %214 ], [ %.98, %216 ], [ %.99, %217 ], [ %.100, %220 ], [ %.101, %221 ], [ %.102, %223 ], [ %.103, %224 ], [ %.104, %228 ], [ %.105, %229 ], [ %.106, %231 ], [ %.107, %232 ], [ %.108, %235 ], [ %.109, %236 ], [ %.110, %238 ], [ %.111, %239 ], [ %.112, %244 ], [ %.113, %245 ], [ %.114, %247 ], [ %.115, %248 ], [ %.116, %251 ], [ %.117, %252 ], [ %.118, %254 ], [ %.119, %255 ], [ %.120, %259 ], [ %.121, %260 ], [ %.122, %262 ], [ %.123, %263 ], [ %.124, %266 ], [ %.125, %267 ], [ %.126, %269 ], [ %271, %270 ]
  tail call void @putchar(i8 %.sink)
  ret void
}

attributes #0 = { nofree nounwind }
