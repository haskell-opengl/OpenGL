###############################################################################
#
# OpenGL 1.3 enumerant specification, including extensions
#
# This file was created in the following way:
# 
# 1) Use the SI's enum.spec (rev. 1.3) as a basis (it's OpenGL 1.2.1 compliant).
# 2) Apply some bug fixes, see http://haskell.org/HOpenGL/spec_bugs.html.
# 3) Inline the 1.1, 1.2, and 1.2.1 changes.
# 4) Make OpenGL 1.3 changes according to Appendix F of the 1.3 specs.
# 5) Remove the extensions part.
# 6) Rearrange definitions and declarations to get a better naming.
# 7) Merge the extension registry's enumext.spec (TODO).
#
# For the original files, see SGI's OpenGL sample implementation at:
#
#    http://oss.sgi.com/projects/ogl-sample/
#
###############################################################################

AccumOp			enum:
			ACCUM					= 0x0100
			LOAD					= 0x0101
			RETURN					= 0x0102
			MULT					= 0x0103
			ADD					= 0x0104

AlphaFunction		enum:
			NEVER					= 0x0200
			LESS					= 0x0201
			EQUAL					= 0x0202
			LEQUAL					= 0x0203
			GREATER					= 0x0204
			NOTEQUAL				= 0x0205
			GEQUAL					= 0x0206
			ALWAYS					= 0x0207

AttribMask		mask:
			CURRENT_BIT				= 0x00000001
			POINT_BIT				= 0x00000002
			LINE_BIT				= 0x00000004
			POLYGON_BIT				= 0x00000008
			POLYGON_STIPPLE_BIT			= 0x00000010
			PIXEL_MODE_BIT				= 0x00000020
			LIGHTING_BIT				= 0x00000040
			FOG_BIT					= 0x00000080
			DEPTH_BUFFER_BIT			= 0x00000100
			ACCUM_BUFFER_BIT			= 0x00000200
			STENCIL_BUFFER_BIT			= 0x00000400
			VIEWPORT_BIT				= 0x00000800
			TRANSFORM_BIT				= 0x00001000
			ENABLE_BIT				= 0x00002000
			COLOR_BUFFER_BIT			= 0x00004000
			HINT_BIT				= 0x00008000
			EVAL_BIT				= 0x00010000
			LIST_BIT				= 0x00020000
			TEXTURE_BIT				= 0x00040000
			SCISSOR_BIT				= 0x00080000
			ALL_ATTRIB_BITS				= 0x000fffff

BeginMode		enum:
			POINTS					= 0x0000
			LINES					= 0x0001
			LINE_LOOP				= 0x0002
			LINE_STRIP				= 0x0003
			TRIANGLES				= 0x0004
			TRIANGLE_STRIP				= 0x0005
			TRIANGLE_FAN				= 0x0006
			QUADS					= 0x0007
			QUAD_STRIP				= 0x0008
			POLYGON					= 0x0009

BlendEquationMode	enum:
			LOGIC_OP				= 0x0BF1
			FUNC_ADD				= 0x8006
			MIN					= 0x8007
			MAX					= 0x8008
			FUNC_SUBTRACT				= 0x800A
			FUNC_REVERSE_SUBTRACT			= 0x800B

BlendingFactorDest	enum:
			ZERO					= 0
			ONE					= 1
			SRC_COLOR				= 0x0300
			ONE_MINUS_SRC_COLOR			= 0x0301
			SRC_ALPHA				= 0x0302
			ONE_MINUS_SRC_ALPHA			= 0x0303
			DST_ALPHA				= 0x0304
			ONE_MINUS_DST_ALPHA			= 0x0305
			CONSTANT_COLOR				= 0x8001
			ONE_MINUS_CONSTANT_COLOR		= 0x8002
			CONSTANT_ALPHA				= 0x8003
			ONE_MINUS_CONSTANT_ALPHA		= 0x8004

BlendingFactorSrc	enum:
			use BlendingFactorDest ZERO
			use BlendingFactorDest ONE
			DST_COLOR				= 0x0306
			ONE_MINUS_DST_COLOR			= 0x0307
			SRC_ALPHA_SATURATE			= 0x0308
			use BlendingFactorDest SRC_ALPHA
			use BlendingFactorDest ONE_MINUS_SRC_ALPHA
			use BlendingFactorDest DST_ALPHA
			use BlendingFactorDest ONE_MINUS_DST_ALPHA
			use BlendingFactorDest CONSTANT_COLOR
			use BlendingFactorDest ONE_MINUS_CONSTANT_COLOR
			use BlendingFactorDest CONSTANT_ALPHA
			use BlendingFactorDest ONE_MINUS_CONSTANT_ALPHA

Boolean			enum:
			TRUE					= 1
			FALSE					= 0

ClearBufferMask		mask:
			use AttribMask COLOR_BUFFER_BIT
			use AttribMask ACCUM_BUFFER_BIT
			use AttribMask STENCIL_BUFFER_BIT
			use AttribMask DEPTH_BUFFER_BIT

ClientArrayType		enum:
			VERTEX_ARRAY				= 0x8074
			NORMAL_ARRAY				= 0x8075
			COLOR_ARRAY				= 0x8076
			INDEX_ARRAY				= 0x8077
			TEXTURE_COORD_ARRAY			= 0x8078
			EDGE_FLAG_ARRAY				= 0x8079

ClientAttribMask	mask:
			CLIENT_PIXEL_STORE_BIT			= 0x00000001
			CLIENT_VERTEX_ARRAY_BIT			= 0x00000002
			CLIENT_ALL_ATTRIB_BITS			= 0xffffffff

# 0x3000 through 0x3FFF are reserved for clip planes
ClipPlaneName		enum:
			CLIP_PLANE0				= 0x3000
			CLIP_PLANE1				= 0x3001
			CLIP_PLANE2				= 0x3002
			CLIP_PLANE3				= 0x3003
			CLIP_PLANE4				= 0x3004
			CLIP_PLANE5				= 0x3005

ColorMaterialFace	enum:
			use DrawBufferMode FRONT
			use DrawBufferMode BACK
			use DrawBufferMode FRONT_AND_BACK

ColorMaterialParameter	enum:
			use LightParameter AMBIENT
			use LightParameter DIFFUSE
			use LightParameter SPECULAR
			use MaterialParameter EMISSION
			use MaterialParameter AMBIENT_AND_DIFFUSE

ColorPointerType	enum:
			use DataType BYTE
			use DataType UNSIGNED_BYTE
			use DataType SHORT
			use DataType UNSIGNED_SHORT
			use DataType INT
			use DataType UNSIGNED_INT
			use DataType FLOAT
			use DataType DOUBLE

ColorTableParameterPName enum:
			COLOR_TABLE_SCALE			= 0x80D6
			COLOR_TABLE_BIAS			= 0x80D7

ColorTableTarget	enum:
			COLOR_TABLE				= 0x80D0 # 1 I
			POST_CONVOLUTION_COLOR_TABLE		= 0x80D1 # 1 I
			POST_COLOR_MATRIX_COLOR_TABLE		= 0x80D2 # 1 I
			PROXY_COLOR_TABLE			= 0x80D3
			PROXY_POST_CONVOLUTION_COLOR_TABLE	= 0x80D4
			PROXY_POST_COLOR_MATRIX_COLOR_TABLE	= 0x80D5

ConvolutionBorderMode	enum:
			REDUCE					= 0x8016
			CONSTANT_BORDER				= 0x8151
			REPLICATE_BORDER			= 0x8153

ConvolutionParameter	enum:
			CONVOLUTION_BORDER_MODE			= 0x8013
			CONVOLUTION_FILTER_SCALE		= 0x8014
			CONVOLUTION_FILTER_BIAS			= 0x8015

ConvolutionTarget	enum:
			CONVOLUTION_1D				= 0x8010 # 1 I
			CONVOLUTION_2D				= 0x8011 # 1 I

CullFaceMode		enum:
			use DrawBufferMode FRONT
			use DrawBufferMode BACK
			use DrawBufferMode FRONT_AND_BACK

DataType		enum:
			BYTE					= 0x1400
			UNSIGNED_BYTE				= 0x1401
			SHORT					= 0x1402
			UNSIGNED_SHORT				= 0x1403
			INT					= 0x1404
			UNSIGNED_INT				= 0x1405
			FLOAT					= 0x1406
			2_BYTES					= 0x1407
			3_BYTES					= 0x1408
			4_BYTES					= 0x1409
			DOUBLE					= 0x140A

DepthFunction		enum:
			use AlphaFunction NEVER
			use AlphaFunction LESS
			use AlphaFunction EQUAL
			use AlphaFunction LEQUAL
			use AlphaFunction GREATER
			use AlphaFunction NOTEQUAL
			use AlphaFunction GEQUAL
			use AlphaFunction ALWAYS

DrawBufferMode		enum:
			NONE					= 0
			FRONT_LEFT				= 0x0400
			FRONT_RIGHT				= 0x0401
			BACK_LEFT				= 0x0402
			BACK_RIGHT				= 0x0403
			FRONT					= 0x0404
			BACK					= 0x0405
			LEFT					= 0x0406
			RIGHT					= 0x0407
			FRONT_AND_BACK				= 0x0408
			AUX0					= 0x0409
			AUX1					= 0x040A
			AUX2					= 0x040B
			AUX3					= 0x040C

Enable			enum:
			FOG					= 0x0B60
			LIGHTING				= 0x0B50
			use TextureTarget TEXTURE_1D
			use TextureTarget TEXTURE_2D
			LINE_STIPPLE				= 0x0B24
			POLYGON_STIPPLE				= 0x0B42
			CULL_FACE				= 0x0B44
			ALPHA_TEST				= 0x0BC0
			use TextureEnvMode BLEND
			INDEX_LOGIC_OP				= 0x0BF1
			COLOR_LOGIC_OP				= 0x0BF2
			DITHER					= 0x0BD0
			STENCIL_TEST				= 0x0B90
			DEPTH_TEST				= 0x0B71
			use ClipPlaneName CLIP_PLANE0
			use ClipPlaneName CLIP_PLANE1
			use ClipPlaneName CLIP_PLANE2
			use ClipPlaneName CLIP_PLANE3
			use ClipPlaneName CLIP_PLANE4
			use ClipPlaneName CLIP_PLANE5
			use LightName LIGHT0
			use LightName LIGHT1
			use LightName LIGHT2
			use LightName LIGHT3
			use LightName LIGHT4
			use LightName LIGHT5
			use LightName LIGHT6
			use LightName LIGHT7
			TEXTURE_GEN_S				= 0x0C60
			TEXTURE_GEN_T				= 0x0C61
			TEXTURE_GEN_R				= 0x0C62
			TEXTURE_GEN_Q				= 0x0C63
			use MapTarget MAP1_VERTEX_3
			use MapTarget MAP1_VERTEX_4
			use MapTarget MAP1_COLOR_4
			use MapTarget MAP1_INDEX
			use MapTarget MAP1_NORMAL
			use MapTarget MAP1_TEXTURE_COORD_1
			use MapTarget MAP1_TEXTURE_COORD_2
			use MapTarget MAP1_TEXTURE_COORD_3
			use MapTarget MAP1_TEXTURE_COORD_4
			use MapTarget MAP2_VERTEX_3
			use MapTarget MAP2_VERTEX_4
			use MapTarget MAP2_COLOR_4
			use MapTarget MAP2_INDEX
			use MapTarget MAP2_NORMAL
			use MapTarget MAP2_TEXTURE_COORD_1
			use MapTarget MAP2_TEXTURE_COORD_2
			use MapTarget MAP2_TEXTURE_COORD_3
			use MapTarget MAP2_TEXTURE_COORD_4
			POINT_SMOOTH				= 0x0B10
			LINE_SMOOTH				= 0x0B20
			POLYGON_SMOOTH				= 0x0B41
			SCISSOR_TEST				= 0x0C11
			COLOR_MATERIAL				= 0x0B57
			NORMALIZE				= 0x0BA1
			AUTO_NORMAL				= 0x0D80

			use ClientArrayType VERTEX_ARRAY
			use ClientArrayType NORMAL_ARRAY
			use ClientArrayType COLOR_ARRAY
			use ClientArrayType INDEX_ARRAY
			use ClientArrayType TEXTURE_COORD_ARRAY
			use ClientArrayType EDGE_FLAG_ARRAY

			POLYGON_OFFSET_POINT			= 0x2A01
			POLYGON_OFFSET_LINE			= 0x2A02
			POLYGON_OFFSET_FILL			= 0x8037

			use ColorTableTarget COLOR_TABLE
			use ColorTableTarget POST_CONVOLUTION_COLOR_TABLE
			use ColorTableTarget POST_COLOR_MATRIX_COLOR_TABLE

			use ConvolutionTarget CONVOLUTION_1D
			use ConvolutionTarget CONVOLUTION_2D
			use SeparableTarget SEPARABLE_2D

			use HistogramTarget HISTOGRAM
			use MinMaxTarget MINMAX

			RESCALE_NORMAL				= 0x803A # 1 I

			use TextureTarget TEXTURE_3D

ErrorCode		enum:
			NO_ERROR				= 0
			INVALID_ENUM				= 0x0500
			INVALID_VALUE				= 0x0501
			INVALID_OPERATION			= 0x0502
			STACK_OVERFLOW				= 0x0503
			STACK_UNDERFLOW				= 0x0504
			OUT_OF_MEMORY				= 0x0505
			TABLE_TOO_LARGE				= 0x8031

FeedBackMode		enum:
			2D					= 0x0600
			3D					= 0x0601
			3D_COLOR				= 0x0602
			3D_COLOR_TEXTURE			= 0x0603
			4D_COLOR_TEXTURE			= 0x0604

FeedBackToken		enum:
			PASS_THROUGH_TOKEN			= 0x0700
			POINT_TOKEN				= 0x0701
			LINE_TOKEN				= 0x0702
			POLYGON_TOKEN				= 0x0703
			BITMAP_TOKEN				= 0x0704
			DRAW_PIXEL_TOKEN			= 0x0705
			COPY_PIXEL_TOKEN			= 0x0706
			LINE_RESET_TOKEN			= 0x0707

FogMode			enum:
			use TextureMinFilter LINEAR
			EXP					= 0x0800
			EXP2					= 0x0801

FogParameter		enum:
			FOG_INDEX				= 0x0B61
			FOG_DENSITY				= 0x0B62
			FOG_START				= 0x0B63
			FOG_END					= 0x0B64
			FOG_MODE				= 0x0B65
			FOG_COLOR				= 0x0B66

FrontFaceDirection	enum:
			CW					= 0x0900
			CCW					= 0x0901

GetColorTableParameterPName enum:
			use ColorTableParameterPName COLOR_TABLE_SCALE
			use ColorTableParameterPName COLOR_TABLE_BIAS
			COLOR_TABLE_FORMAT			= 0x80D8
			COLOR_TABLE_WIDTH			= 0x80D9
			COLOR_TABLE_RED_SIZE			= 0x80DA
			COLOR_TABLE_GREEN_SIZE			= 0x80DB
			COLOR_TABLE_BLUE_SIZE			= 0x80DC
			COLOR_TABLE_ALPHA_SIZE			= 0x80DD
			COLOR_TABLE_LUMINANCE_SIZE		= 0x80DE
			COLOR_TABLE_INTENSITY_SIZE		= 0x80DF

GetConvolutionParameterPName enum:
			CONVOLUTION_BORDER_COLOR		= 0x8154
			use ConvolutionParameterPName CONVOLUTION_BORDER_MODE
			use ConvolutionParameterPName CONVOLUTION_FILTER_SCALE
			use ConvolutionParameterPName CONVOLUTION_FILTER_BIAS
			CONVOLUTION_FORMAT			= 0x8017
			CONVOLUTION_WIDTH			= 0x8018
			CONVOLUTION_HEIGHT			= 0x8019
			MAX_CONVOLUTION_WIDTH			= 0x801A
			MAX_CONVOLUTION_HEIGHT			= 0x801B

GetHistogramParameterPName enum:
			HISTOGRAM_WIDTH				= 0x8026
			HISTOGRAM_FORMAT			= 0x8027
			HISTOGRAM_RED_SIZE			= 0x8028
			HISTOGRAM_GREEN_SIZE			= 0x8029
			HISTOGRAM_BLUE_SIZE			= 0x802A
			HISTOGRAM_ALPHA_SIZE			= 0x802B
			HISTOGRAM_LUMINANCE_SIZE		= 0x802C
			HISTOGRAM_SINK				= 0x802D

GetMapTarget		enum:
			COEFF					= 0x0A00
			ORDER					= 0x0A01
			DOMAIN					= 0x0A02

GetMinmaxParameterPName enum:
			MINMAX_FORMAT				= 0x802F
			MINMAX_SINK				= 0x8030

GetPixelMap		enum:
			use PixelMap PIXEL_MAP_I_TO_I
			use PixelMap PIXEL_MAP_S_TO_S
			use PixelMap PIXEL_MAP_I_TO_R
			use PixelMap PIXEL_MAP_I_TO_G
			use PixelMap PIXEL_MAP_I_TO_B
			use PixelMap PIXEL_MAP_I_TO_A
			use PixelMap PIXEL_MAP_R_TO_R
			use PixelMap PIXEL_MAP_G_TO_G
			use PixelMap PIXEL_MAP_B_TO_B
			use PixelMap PIXEL_MAP_A_TO_A

GetPointerTarget	enum:
			VERTEX_ARRAY_POINTER			= 0x808E
			NORMAL_ARRAY_POINTER			= 0x808F
			COLOR_ARRAY_POINTER			= 0x8090
			INDEX_ARRAY_POINTER			= 0x8091
			TEXTURE_COORD_ARRAY_POINTER		= 0x8092
			EDGE_FLAG_ARRAY_POINTER			= 0x8093

GetTarget		enum:
			CURRENT_COLOR				= 0x0B00
			CURRENT_INDEX				= 0x0B01
			CURRENT_NORMAL				= 0x0B02
			CURRENT_TEXTURE_COORDS			= 0x0B03
			CURRENT_RASTER_COLOR			= 0x0B04
			CURRENT_RASTER_INDEX			= 0x0B05
			CURRENT_RASTER_TEXTURE_COORDS		= 0x0B06
			CURRENT_RASTER_POSITION			= 0x0B07
			CURRENT_RASTER_POSITION_VALID		= 0x0B08
			CURRENT_RASTER_DISTANCE			= 0x0B09

			use Enable POINT_SMOOTH
			POINT_SIZE				= 0x0B11
			POINT_SIZE_RANGE			= 0x0B12
			POINT_SIZE_GRANULARITY			= 0x0B13

			use Enable LINE_SMOOTH
			LINE_WIDTH				= 0x0B21
			LINE_WIDTH_RANGE			= 0x0B22
			LINE_WIDTH_GRANULARITY			= 0x0B23
			use Enable LINE_STIPPLE
			LINE_STIPPLE_PATTERN			= 0x0B25
			LINE_STIPPLE_REPEAT			= 0x0B26
			SMOOTH_POINT_SIZE_RANGE			= 0x0B12
			SMOOTH_POINT_SIZE_GRANULARITY		= 0x0B13
			SMOOTH_LINE_WIDTH_RANGE			= 0x0B22
			SMOOTH_LINE_WIDTH_GRANULARITY		= 0x0B23
			ALIASED_POINT_SIZE_RANGE		= 0x846D # 2 F
			ALIASED_LINE_WIDTH_RANGE		= 0x846E # 2 F

			LIST_MODE				= 0x0B30
			MAX_LIST_NESTING			= 0x0B31
			LIST_BASE				= 0x0B32
			LIST_INDEX				= 0x0B33

			POLYGON_MODE				= 0x0B40
			use Enable POLYGON_SMOOTH
			use Enable POLYGON_STIPPLE
			EDGE_FLAG				= 0x0B43
			use Enable CULL_FACE
			CULL_FACE_MODE				= 0x0B45
			FRONT_FACE				= 0x0B46

			use Enable LIGHTING

			use LightModelParameter LIGHT_MODEL_AMBIENT
			use LightModelParameter LIGHT_MODEL_LOCAL_VIEWER
			use LightModelParameter LIGHT_MODEL_TWO_SIDE
			use LightModelParameter LIGHT_MODEL_COLOR_CONTROL

			SHADE_MODEL				= 0x0B54
			COLOR_MATERIAL_FACE			= 0x0B55
			COLOR_MATERIAL_PARAMETER		= 0x0B56
			use Enable COLOR_MATERIAL

			use Enable FOG
			use FogParameter FOG_COLOR
			use FogParameter FOG_DENSITY
			use FogParameter FOG_END
			use FogParameter FOG_INDEX
			use FogParameter FOG_MODE
			use FogParameter FOG_START

			DEPTH_RANGE				= 0x0B70
			use Enable DEPTH_TEST
			DEPTH_WRITEMASK				= 0x0B72
			DEPTH_CLEAR_VALUE			= 0x0B73
			DEPTH_FUNC				= 0x0B74

			ACCUM_CLEAR_VALUE			= 0x0B80

			use Enable STENCIL_TEST
			STENCIL_CLEAR_VALUE			= 0x0B91
			STENCIL_FUNC				= 0x0B92
			STENCIL_VALUE_MASK			= 0x0B93
			STENCIL_FAIL				= 0x0B94
			STENCIL_PASS_DEPTH_FAIL			= 0x0B95
			STENCIL_PASS_DEPTH_PASS			= 0x0B96
			STENCIL_REF				= 0x0B97
			STENCIL_WRITEMASK			= 0x0B98

			MATRIX_MODE				= 0x0BA0
			use Enable NORMALIZE
			VIEWPORT				= 0x0BA2
			MODELVIEW_STACK_DEPTH			= 0x0BA3
			PROJECTION_STACK_DEPTH			= 0x0BA4
			TEXTURE_STACK_DEPTH			= 0x0BA5
			MODELVIEW_MATRIX			= 0x0BA6
			PROJECTION_MATRIX			= 0x0BA7
			TEXTURE_MATRIX				= 0x0BA8

			ATTRIB_STACK_DEPTH			= 0x0BB0
			CLIENT_ATTRIB_STACK_DEPTH		= 0x0BB1

			use Enable ALPHA_TEST
			ALPHA_TEST_FUNC				= 0x0BC1
			ALPHA_TEST_REF				= 0x0BC2

			use Enable DITHER

			BLEND_DST				= 0x0BE0
			BLEND_SRC				= 0x0BE1
			use TextureEnvMode BLEND

			LOGIC_OP_MODE				= 0x0BF0
			use Enable INDEX_LOGIC_OP
			use BlendEquationMode LOGIC_OP
			use Enable COLOR_LOGIC_OP

			AUX_BUFFERS				= 0x0C00
			DRAW_BUFFER				= 0x0C01
			READ_BUFFER				= 0x0C02

			SCISSOR_BOX				= 0x0C10
			use Enable SCISSOR_TEST

			INDEX_CLEAR_VALUE			= 0x0C20
			INDEX_WRITEMASK				= 0x0C21
			COLOR_CLEAR_VALUE			= 0x0C22
			COLOR_WRITEMASK				= 0x0C23

			INDEX_MODE				= 0x0C30
			RGBA_MODE				= 0x0C31
			DOUBLEBUFFER				= 0x0C32
			STEREO					= 0x0C33

			RENDER_MODE				= 0x0C40

			use HintTarget PERSPECTIVE_CORRECTION_HINT
			use HintTarget POINT_SMOOTH_HINT
			use HintTarget LINE_SMOOTH_HINT
			use HintTarget POLYGON_SMOOTH_HINT
			use HintTarget FOG_HINT

			use Enable TEXTURE_GEN_S
			use Enable TEXTURE_GEN_T
			use Enable TEXTURE_GEN_R
			use Enable TEXTURE_GEN_Q

			use PixelMap PIXEL_MAP_I_TO_I
			use PixelMap PIXEL_MAP_S_TO_S
			use PixelMap PIXEL_MAP_I_TO_R
			use PixelMap PIXEL_MAP_I_TO_G
			use PixelMap PIXEL_MAP_I_TO_B
			use PixelMap PIXEL_MAP_I_TO_A
			use PixelMap PIXEL_MAP_R_TO_R
			use PixelMap PIXEL_MAP_G_TO_G
			use PixelMap PIXEL_MAP_B_TO_B
			use PixelMap PIXEL_MAP_A_TO_A

			PIXEL_MAP_I_TO_I_SIZE			= 0x0CB0
			PIXEL_MAP_S_TO_S_SIZE			= 0x0CB1
			PIXEL_MAP_I_TO_R_SIZE			= 0x0CB2
			PIXEL_MAP_I_TO_G_SIZE			= 0x0CB3
			PIXEL_MAP_I_TO_B_SIZE			= 0x0CB4
			PIXEL_MAP_I_TO_A_SIZE			= 0x0CB5
			PIXEL_MAP_R_TO_R_SIZE			= 0x0CB6
			PIXEL_MAP_G_TO_G_SIZE			= 0x0CB7
			PIXEL_MAP_B_TO_B_SIZE			= 0x0CB8
			PIXEL_MAP_A_TO_A_SIZE			= 0x0CB9

			use PixelStore UNPACK_SWAP_BYTES
			use PixelStore UNPACK_LSB_FIRST
			use PixelStore UNPACK_ROW_LENGTH
			use PixelStore UNPACK_SKIP_ROWS
			use PixelStore UNPACK_SKIP_PIXELS
			use PixelStore UNPACK_ALIGNMENT

			use PixelStore PACK_SWAP_BYTES
			use PixelStore PACK_LSB_FIRST
			use PixelStore PACK_ROW_LENGTH
			use PixelStore PACK_SKIP_ROWS
			use PixelStore PACK_SKIP_PIXELS
			use PixelStore PACK_ALIGNMENT

			use PixelTransfer MAP_COLOR
			use PixelTransfer MAP_STENCIL
			use PixelTransfer INDEX_SHIFT
			use PixelTransfer INDEX_OFFSET
			use PixelTransfer RED_SCALE
			use PixelTransfer RED_BIAS
			use PixelTransfer GREEN_SCALE
			use PixelTransfer GREEN_BIAS
			use PixelTransfer BLUE_SCALE
			use PixelTransfer BLUE_BIAS
			use PixelTransfer ALPHA_SCALE
			use PixelTransfer ALPHA_BIAS
			use PixelTransfer DEPTH_SCALE
			use PixelTransfer DEPTH_BIAS

			ZOOM_X					= 0x0D16
			ZOOM_Y					= 0x0D17

			MAX_EVAL_ORDER				= 0x0D30
			MAX_LIGHTS				= 0x0D31
			MAX_CLIP_PLANES				= 0x0D32
			MAX_TEXTURE_SIZE			= 0x0D33
			MAX_PIXEL_MAP_TABLE			= 0x0D34
			MAX_ATTRIB_STACK_DEPTH			= 0x0D35
			MAX_MODELVIEW_STACK_DEPTH		= 0x0D36
			MAX_NAME_STACK_DEPTH			= 0x0D37
			MAX_PROJECTION_STACK_DEPTH		= 0x0D38
			MAX_TEXTURE_STACK_DEPTH			= 0x0D39
			MAX_VIEWPORT_DIMS			= 0x0D3A
			MAX_CLIENT_ATTRIB_STACK_DEPTH		= 0x0D3B

			SUBPIXEL_BITS				= 0x0D50
			INDEX_BITS				= 0x0D51
			RED_BITS				= 0x0D52
			GREEN_BITS				= 0x0D53
			BLUE_BITS				= 0x0D54
			ALPHA_BITS				= 0x0D55
			DEPTH_BITS				= 0x0D56
			STENCIL_BITS				= 0x0D57
			ACCUM_RED_BITS				= 0x0D58
			ACCUM_GREEN_BITS			= 0x0D59
			ACCUM_BLUE_BITS				= 0x0D5A
			ACCUM_ALPHA_BITS			= 0x0D5B

			NAME_STACK_DEPTH			= 0x0D70

			use Enable AUTO_NORMAL

			use MapTarget MAP1_COLOR_4
			use MapTarget MAP1_INDEX
			use MapTarget MAP1_NORMAL
			use MapTarget MAP1_TEXTURE_COORD_1
			use MapTarget MAP1_TEXTURE_COORD_2
			use MapTarget MAP1_TEXTURE_COORD_3
			use MapTarget MAP1_TEXTURE_COORD_4
			use MapTarget MAP1_VERTEX_3
			use MapTarget MAP1_VERTEX_4

			use MapTarget MAP2_COLOR_4
			use MapTarget MAP2_INDEX
			use MapTarget MAP2_NORMAL
			use MapTarget MAP2_TEXTURE_COORD_1
			use MapTarget MAP2_TEXTURE_COORD_2
			use MapTarget MAP2_TEXTURE_COORD_3
			use MapTarget MAP2_TEXTURE_COORD_4
			use MapTarget MAP2_VERTEX_3
			use MapTarget MAP2_VERTEX_4

			MAP1_GRID_DOMAIN			= 0x0DD0
			MAP1_GRID_SEGMENTS			= 0x0DD1
			MAP2_GRID_DOMAIN			= 0x0DD2
			MAP2_GRID_SEGMENTS			= 0x0DD3

			use TextureTarget TEXTURE_1D
			use TextureTarget TEXTURE_2D

			FEEDBACK_BUFFER_POINTER			= 0x0DF0
			FEEDBACK_BUFFER_SIZE			= 0x0DF1
			FEEDBACK_BUFFER_TYPE			= 0x0DF2

			SELECTION_BUFFER_POINTER		= 0x0DF3
			SELECTION_BUFFER_SIZE			= 0x0DF4

			TEXTURE_BINDING_1D			= 0x8068
			TEXTURE_BINDING_2D			= 0x8069
			TEXTURE_BINDING_3D			= 0x806A

			use ClientArrayType VERTEX_ARRAY
			use ClientArrayType NORMAL_ARRAY
			use ClientArrayType COLOR_ARRAY
			use ClientArrayType INDEX_ARRAY
			use ClientArrayType TEXTURE_COORD_ARRAY
			use ClientArrayType EDGE_FLAG_ARRAY

			VERTEX_ARRAY_SIZE			= 0x807A
			VERTEX_ARRAY_TYPE			= 0x807B
			VERTEX_ARRAY_STRIDE			= 0x807C
			NORMAL_ARRAY_TYPE			= 0x807E
			NORMAL_ARRAY_STRIDE			= 0x807F
			COLOR_ARRAY_SIZE			= 0x8081
			COLOR_ARRAY_TYPE			= 0x8082
			COLOR_ARRAY_STRIDE			= 0x8083
			INDEX_ARRAY_TYPE			= 0x8085
			INDEX_ARRAY_STRIDE			= 0x8086
			TEXTURE_COORD_ARRAY_SIZE		= 0x8088
			TEXTURE_COORD_ARRAY_TYPE		= 0x8089
			TEXTURE_COORD_ARRAY_STRIDE		= 0x808A
			EDGE_FLAG_ARRAY_STRIDE			= 0x808C

			POLYGON_OFFSET_UNITS			= 0x2A00
			use Enable POLYGON_OFFSET_POINT
			use Enable POLYGON_OFFSET_LINE
			use Enable POLYGON_OFFSET_FILL
			POLYGON_OFFSET_FACTOR			= 0x8038

			use ColorTableTarget COLOR_TABLE
			use ColorTableTarget POST_CONVOLUTION_COLOR_TABLE
			use ColorTableTarget POST_COLOR_MATRIX_COLOR_TABLE

			use ConvolutionTarget CONVOLUTION_1D
			use ConvolutionTarget CONVOLUTION_2D
			use SeparableTarget SEPARABLE_2D
			use PixelTransfer POST_CONVOLUTION_RED_SCALE
			use PixelTransfer POST_CONVOLUTION_GREEN_SCALE
			use PixelTransfer POST_CONVOLUTION_BLUE_SCALE
			use PixelTransfer POST_CONVOLUTION_ALPHA_SCALE
			use PixelTransfer POST_CONVOLUTION_RED_BIAS
			use PixelTransfer POST_CONVOLUTION_GREEN_BIAS
			use PixelTransfer POST_CONVOLUTION_BLUE_BIAS
			use PixelTransfer POST_CONVOLUTION_ALPHA_BIAS

			COLOR_MATRIX				= 0x80B1 # 16 F
			COLOR_MATRIX_STACK_DEPTH		= 0x80B2 # 1 I
			MAX_COLOR_MATRIX_STACK_DEPTH		= 0x80B3 # 1 I
			use PixelTransfer POST_COLOR_MATRIX_RED_SCALE
			use PixelTransfer POST_COLOR_MATRIX_GREEN_SCALE
			use PixelTransfer POST_COLOR_MATRIX_BLUE_SCALE
			use PixelTransfer POST_COLOR_MATRIX_ALPHA_SCALE
			use PixelTransfer POST_COLOR_MATRIX_RED_BIAS
			use PixelTransfer POST_COLOR_MATRIX_GREEN_BIAS
			use PixelTransfer POST_COLOR_MATRIX_BLUE_BIAS
			use PixelTransfer POST_COLOR_MATRIX_ALPHA_BIAS

			use HistogramTarget HISTOGRAM
			use MinMaxTarget MINMAX

			MAX_ELEMENTS_VERTICES			= 0x80E8
			MAX_ELEMENTS_INDICES			= 0x80E9

			use Enable RESCALE_NORMAL

			use PixelStore PACK_SKIP_IMAGES
			use PixelStore PACK_IMAGE_HEIGHT
			use PixelStore UNPACK_SKIP_IMAGES
			use PixelStore UNPACK_IMAGE_HEIGHT

			use TextureTarget TEXTURE_3D
			MAX_3D_TEXTURE_SIZE			= 0x8073 # 1 I

			BLEND_COLOR				= 0x8005 # 4 F

			BLEND_EQUATION				= 0x8009 # 1 I

			ACTIVE_TEXTURE_ARB			= 0x84E0
			CLIENT_ACTIVE_TEXTURE_ARB		= 0x84E1
			MAX_TEXTURE_UNITS_ARB			= 0x84E2

GetTextureParameter	enum:
			use TextureParameterName TEXTURE_MAG_FILTER
			use TextureParameterName TEXTURE_MIN_FILTER

			use TextureParameterName TEXTURE_WRAP_R
			use TextureParameterName TEXTURE_WRAP_S
			use TextureParameterName TEXTURE_WRAP_T

			TEXTURE_WIDTH				= 0x1000
			TEXTURE_HEIGHT				= 0x1001
			TEXTURE_DEPTH				= 0x8071

			TEXTURE_INTERNAL_FORMAT			= 0x1003
			use TextureParameterName TEXTURE_BORDER_COLOR
			TEXTURE_BORDER				= 0x1005

			TEXTURE_RED_SIZE			= 0x805C
			TEXTURE_GREEN_SIZE			= 0x805D
			TEXTURE_BLUE_SIZE			= 0x805E
			TEXTURE_ALPHA_SIZE			= 0x805F
			TEXTURE_LUMINANCE_SIZE			= 0x8060
			TEXTURE_INTENSITY_SIZE			= 0x8061

			use TextureParameterName TEXTURE_PRIORITY
			TEXTURE_RESIDENT			= 0x8067

			use TextureParameterName TEXTURE_MIN_LOD
			use TextureParameterName TEXTURE_MAX_LOD
			use TextureParameterName TEXTURE_BASE_LEVEL
			use TextureParameterName TEXTURE_MAX_LEVEL

HintMode		enum:
			DONT_CARE				= 0x1100
			FASTEST					= 0x1101
			NICEST					= 0x1102

HintTarget		enum:
			PERSPECTIVE_CORRECTION_HINT		= 0x0C50
			POINT_SMOOTH_HINT			= 0x0C51
			LINE_SMOOTH_HINT			= 0x0C52
			POLYGON_SMOOTH_HINT			= 0x0C53
			FOG_HINT				= 0x0C54

HistogramTarget		enum:
			HISTOGRAM				= 0x8024 # 1 I
			PROXY_HISTOGRAM				= 0x8025

IndexPointerType	enum:
			use DataType SHORT
			use DataType INT
			use DataType FLOAT
			use DataType DOUBLE

LightModelColorControl	enum:
			SINGLE_COLOR				= 0x81F9
			SEPARATE_SPECULAR_COLOR			= 0x81FA

LightModelParameter	enum:
			LIGHT_MODEL_AMBIENT			= 0x0B53
			LIGHT_MODEL_LOCAL_VIEWER		= 0x0B51
			LIGHT_MODEL_TWO_SIDE			= 0x0B52
			LIGHT_MODEL_COLOR_CONTROL		= 0x81F8 # 1 I

# 0x4000-0x4FFF are reserved for light numbers
LightName		enum:
			LIGHT0					= 0x4000
			LIGHT1					= 0x4001
			LIGHT2					= 0x4002
			LIGHT3					= 0x4003
			LIGHT4					= 0x4004
			LIGHT5					= 0x4005
			LIGHT6					= 0x4006
			LIGHT7					= 0x4007

LightParameter		enum:
			AMBIENT					= 0x1200
			DIFFUSE					= 0x1201
			SPECULAR				= 0x1202
			POSITION				= 0x1203
			SPOT_DIRECTION				= 0x1204
			SPOT_EXPONENT				= 0x1205
			SPOT_CUTOFF				= 0x1206
			CONSTANT_ATTENUATION			= 0x1207
			LINEAR_ATTENUATION			= 0x1208
			QUADRATIC_ATTENUATION			= 0x1209

InterleavedArrays	enum:
			V2F					= 0x2A20
			V3F					= 0x2A21
			C4UB_V2F				= 0x2A22
			C4UB_V3F				= 0x2A23
			C3F_V3F					= 0x2A24
			N3F_V3F					= 0x2A25
			C4F_N3F_V3F				= 0x2A26
			T2F_V3F					= 0x2A27
			T4F_V4F					= 0x2A28
			T2F_C4UB_V3F				= 0x2A29
			T2F_C3F_V3F				= 0x2A2A
			T2F_N3F_V3F				= 0x2A2B
			T2F_C4F_N3F_V3F				= 0x2A2C
			T4F_C4F_N3F_V4F				= 0x2A2D

ListMode		enum:
			COMPILE					= 0x1300
			COMPILE_AND_EXECUTE			= 0x1301

ListNameType		enum:
			use DataType BYTE
			use DataType UNSIGNED_BYTE
			use DataType SHORT
			use DataType UNSIGNED_SHORT
			use DataType INT
			use DataType UNSIGNED_INT
			use DataType FLOAT
			use DataType 2_BYTES
			use DataType 3_BYTES
			use DataType 4_BYTES

# same names as X, GL style
LogicOp			enum:
			CLEAR					= 0x1500
			AND					= 0x1501
			AND_REVERSE				= 0x1502
			COPY					= 0x1503
			AND_INVERTED				= 0x1504
			NOOP					= 0x1505
			XOR					= 0x1506
			OR					= 0x1507
			NOR					= 0x1508
			EQUIV					= 0x1509
			INVERT					= 0x150A
			OR_REVERSE				= 0x150B
			COPY_INVERTED				= 0x150C
			OR_INVERTED				= 0x150D
			NAND					= 0x150E
			SET					= 0x150F

MapTarget		enum:
			MAP1_COLOR_4				= 0x0D90
			MAP1_INDEX				= 0x0D91
			MAP1_NORMAL				= 0x0D92
			MAP1_TEXTURE_COORD_1			= 0x0D93
			MAP1_TEXTURE_COORD_2			= 0x0D94
			MAP1_TEXTURE_COORD_3			= 0x0D95
			MAP1_TEXTURE_COORD_4			= 0x0D96
			MAP1_VERTEX_3				= 0x0D97
			MAP1_VERTEX_4				= 0x0D98

			MAP2_COLOR_4				= 0x0DB0
			MAP2_INDEX				= 0x0DB1
			MAP2_NORMAL				= 0x0DB2
			MAP2_TEXTURE_COORD_1			= 0x0DB3
			MAP2_TEXTURE_COORD_2			= 0x0DB4
			MAP2_TEXTURE_COORD_3			= 0x0DB5
			MAP2_TEXTURE_COORD_4			= 0x0DB6
			MAP2_VERTEX_3				= 0x0DB7
			MAP2_VERTEX_4				= 0x0DB8

MaterialFace		enum:
			use DrawBufferMode FRONT
			use DrawBufferMode BACK
			use DrawBufferMode FRONT_AND_BACK

MaterialParameter	enum:
			EMISSION				= 0x1600
			SHININESS				= 0x1601
			AMBIENT_AND_DIFFUSE			= 0x1602
			COLOR_INDEXES				= 0x1603
			use LightParameter AMBIENT
			use LightParameter DIFFUSE
			use LightParameter SPECULAR

MatrixMode		enum:
			MODELVIEW				= 0x1700
			PROJECTION				= 0x1701
			TEXTURE					= 0x1702

MeshMode1		enum:
			use PolygonMode POINT
			use PolygonMode LINE

MeshMode2		enum:
			use PolygonMode POINT
			use PolygonMode LINE
			use PolygonMode FILL

MinmaxTarget		enum:
			MINMAX					= 0x802E # 1 I

NormalPointerType	enum:
			use DataType BYTE
			use DataType SHORT
			use DataType INT
			use DataType FLOAT
			use DataType DOUBLE

PixelCopyType		enum:
			COLOR					= 0x1800
			DEPTH					= 0x1801
			STENCIL					= 0x1802

PixelFormat		enum:
			COLOR_INDEX				= 0x1900
			STENCIL_INDEX				= 0x1901
			DEPTH_COMPONENT				= 0x1902
			RED					= 0x1903
			GREEN					= 0x1904
			BLUE					= 0x1905
			ALPHA					= 0x1906
			RGB					= 0x1907
			RGBA					= 0x1908
			LUMINANCE				= 0x1909
			LUMINANCE_ALPHA				= 0x190A

PixelInternalFormat	enum:
			ALPHA4					= 0x803B
			ALPHA8					= 0x803C
			ALPHA12					= 0x803D
			ALPHA16					= 0x803E
			LUMINANCE4				= 0x803F
			LUMINANCE8				= 0x8040
			LUMINANCE12				= 0x8041
			LUMINANCE16				= 0x8042
			LUMINANCE4_ALPHA4			= 0x8043
			LUMINANCE6_ALPHA2			= 0x8044
			LUMINANCE8_ALPHA8			= 0x8045
			LUMINANCE12_ALPHA4			= 0x8046
			LUMINANCE12_ALPHA12			= 0x8047
			LUMINANCE16_ALPHA16			= 0x8048
			INTENSITY				= 0x8049
			INTENSITY4				= 0x804A
			INTENSITY8				= 0x804B
			INTENSITY12				= 0x804C
			INTENSITY16				= 0x804D
			R3_G3_B2				= 0x2A10
			RGB4					= 0x804F
			RGB5					= 0x8050
			RGB8					= 0x8051
			RGB10					= 0x8052
			RGB12					= 0x8053
			RGB16					= 0x8054
			RGBA2					= 0x8055
			RGBA4					= 0x8056
			RGB5_A1					= 0x8057
			RGBA8					= 0x8058
			RGB10_A2				= 0x8059
			RGBA12					= 0x805A
			RGBA16					= 0x805B

PixelMap		enum:
			PIXEL_MAP_I_TO_I			= 0x0C70
			PIXEL_MAP_S_TO_S			= 0x0C71
			PIXEL_MAP_I_TO_R			= 0x0C72
			PIXEL_MAP_I_TO_G			= 0x0C73
			PIXEL_MAP_I_TO_B			= 0x0C74
			PIXEL_MAP_I_TO_A			= 0x0C75
			PIXEL_MAP_R_TO_R			= 0x0C76
			PIXEL_MAP_G_TO_G			= 0x0C77
			PIXEL_MAP_B_TO_B			= 0x0C78
			PIXEL_MAP_A_TO_A			= 0x0C79

PixelStore		enum:
			UNPACK_SWAP_BYTES			= 0x0CF0
			UNPACK_LSB_FIRST			= 0x0CF1
			UNPACK_ROW_LENGTH			= 0x0CF2
			UNPACK_SKIP_ROWS			= 0x0CF3
			UNPACK_SKIP_PIXELS			= 0x0CF4
			UNPACK_ALIGNMENT			= 0x0CF5

			PACK_SWAP_BYTES				= 0x0D00
			PACK_LSB_FIRST				= 0x0D01
			PACK_ROW_LENGTH				= 0x0D02
			PACK_SKIP_ROWS				= 0x0D03
			PACK_SKIP_PIXELS			= 0x0D04
			PACK_ALIGNMENT				= 0x0D05

			PACK_SKIP_IMAGES			= 0x806B # 1 I
			PACK_IMAGE_HEIGHT			= 0x806C # 1 F
			UNPACK_SKIP_IMAGES			= 0x806D # 1 I
			UNPACK_IMAGE_HEIGHT			= 0x806E # 1 F

PixelTransfer		enum:
			MAP_COLOR				= 0x0D10
			MAP_STENCIL				= 0x0D11
			INDEX_SHIFT				= 0x0D12
			INDEX_OFFSET				= 0x0D13
			RED_SCALE				= 0x0D14
			RED_BIAS				= 0x0D15
			GREEN_SCALE				= 0x0D18
			GREEN_BIAS				= 0x0D19
			BLUE_SCALE				= 0x0D1A
			BLUE_BIAS				= 0x0D1B
			ALPHA_SCALE				= 0x0D1C
			ALPHA_BIAS				= 0x0D1D
			DEPTH_SCALE				= 0x0D1E
			DEPTH_BIAS				= 0x0D1F

			POST_CONVOLUTION_RED_SCALE		= 0x801C # 1 F
			POST_CONVOLUTION_GREEN_SCALE		= 0x801D # 1 F
			POST_CONVOLUTION_BLUE_SCALE		= 0x801E # 1 F
			POST_CONVOLUTION_ALPHA_SCALE		= 0x801F # 1 F
			POST_CONVOLUTION_RED_BIAS		= 0x8020 # 1 F
			POST_CONVOLUTION_GREEN_BIAS		= 0x8021 # 1 F
			POST_CONVOLUTION_BLUE_BIAS		= 0x8022 # 1 F
			POST_CONVOLUTION_ALPHA_BIAS		= 0x8023 # 1 F

			POST_COLOR_MATRIX_RED_SCALE		= 0x80B4 # 1 F
			POST_COLOR_MATRIX_GREEN_SCALE		= 0x80B5 # 1 F
			POST_COLOR_MATRIX_BLUE_SCALE		= 0x80B6 # 1 F
			POST_COLOR_MATRIX_ALPHA_SCALE		= 0x80B7 # 1 F
			POST_COLOR_MATRIX_RED_BIAS		= 0x80B8 # 1 F
			POST_COLOR_MATRIX_GREEN_BIAS		= 0x80B9 # 1 F
			POST_COLOR_MATRIX_BLUE_BIAS		= 0x80BA # 1 F
			POST_COLOR_MATRIX_ALPHA_BIAS		= 0x80BB # 1 F

PixelType		enum:
			BITMAP					= 0x1A00
			use DataType BYTE
			use DataType UNSIGNED_BYTE
			use DataType SHORT
			use DataType UNSIGNED_SHORT
			use DataType INT
			use DataType UNSIGNED_INT
			use DataType FLOAT
			BGR					= 0x80E0
			BGRA					= 0x80E1
			UNSIGNED_BYTE_3_3_2			= 0x8032
			UNSIGNED_SHORT_4_4_4_4			= 0x8033
			UNSIGNED_SHORT_5_5_5_1			= 0x8034
			UNSIGNED_INT_8_8_8_8			= 0x8035
			UNSIGNED_INT_10_10_10_2			= 0x8036
			UNSIGNED_BYTE_2_3_3_REV			= 0x8362
			UNSIGNED_SHORT_5_6_5			= 0x8363
			UNSIGNED_SHORT_5_6_5_REV		= 0x8364
			UNSIGNED_SHORT_4_4_4_4_REV		= 0x8365
			UNSIGNED_SHORT_1_5_5_5_REV		= 0x8366
			UNSIGNED_INT_8_8_8_8_REV		= 0x8367
			UNSIGNED_INT_2_10_10_10_REV		= 0x8368

PolygonMode		enum:
			POINT					= 0x1B00
			LINE					= 0x1B01
			FILL					= 0x1B02

ReadBufferMode		enum:
			use DrawBufferMode FRONT_LEFT
			use DrawBufferMode FRONT_RIGHT
			use DrawBufferMode BACK_LEFT
			use DrawBufferMode BACK_RIGHT
			use DrawBufferMode FRONT
			use DrawBufferMode BACK
			use DrawBufferMode LEFT
			use DrawBufferMode RIGHT
			use DrawBufferMode AUX0
			use DrawBufferMode AUX1
			use DrawBufferMode AUX2
			use DrawBufferMode AUX3

RenderingMode		enum:
			RENDER					= 0x1C00
			FEEDBACK				= 0x1C01
			SELECT					= 0x1C02

SeparableTarget		enum:
			SEPARABLE_2D				= 0x8012 # 1 I

ShadingModel		enum:
			FLAT					= 0x1D00
			SMOOTH					= 0x1D01

StencilFunction		enum:
			use AlphaFunction NEVER
			use AlphaFunction LESS
			use AlphaFunction EQUAL
			use AlphaFunction LEQUAL
			use AlphaFunction GREATER
			use AlphaFunction NOTEQUAL
			use AlphaFunction GEQUAL
			use AlphaFunction ALWAYS

StencilOp		enum:
			use BlendingFactorDest ZERO
			KEEP					= 0x1E00
			use TextureEnvMode REPLACE
			INCR					= 0x1E02
			DECR					= 0x1E03
			use LogicOp INVERT

StringName		enum:
			VENDOR					= 0x1F00
			RENDERER				= 0x1F01
			VERSION					= 0x1F02
			EXTENSIONS				= 0x1F03

TextureCoordName	enum:
			S					= 0x2000
			T					= 0x2001
			R					= 0x2002
			Q					= 0x2003

TexCoordPointerType	enum:
			use DataType SHORT
			use DataType INT
			use DataType FLOAT
			use DataType DOUBLE

TextureEnvMode		enum:
			MODULATE				= 0x2100
			DECAL					= 0x2101
			BLEND					= 0x0BE2
			REPLACE					= 0x1E01

TextureEnvParameter	enum:
			TEXTURE_ENV_MODE			= 0x2200
			TEXTURE_ENV_COLOR			= 0x2201

TextureEnvTarget	enum:
			TEXTURE_ENV				= 0x2300

TextureGenMode		enum:
			EYE_LINEAR				= 0x2400
			OBJECT_LINEAR				= 0x2401
			SPHERE_MAP				= 0x2402

TextureGenParameter	enum:
			TEXTURE_GEN_MODE			= 0x2500
			OBJECT_PLANE				= 0x2501
			EYE_PLANE				= 0x2502

TextureMagFilter	enum:
			use TextureMinFilter NEAREST
			use TextureMinFilter LINEAR

TextureMinFilter	enum:
			NEAREST					= 0x2600
			LINEAR					= 0x2601
			NEAREST_MIPMAP_NEAREST			= 0x2700
			LINEAR_MIPMAP_NEAREST			= 0x2701
			NEAREST_MIPMAP_LINEAR			= 0x2702
			LINEAR_MIPMAP_LINEAR			= 0x2703

TextureParameterName	enum:
			TEXTURE_MAG_FILTER			= 0x2800
			TEXTURE_MIN_FILTER			= 0x2801

			TEXTURE_WRAP_R				= 0x8072
			TEXTURE_WRAP_S				= 0x2802
			TEXTURE_WRAP_T				= 0x2803

			TEXTURE_BORDER_COLOR			= 0x1004

			TEXTURE_PRIORITY			= 0x8066

			TEXTURE_MIN_LOD				= 0x813A
			TEXTURE_MAX_LOD				= 0x813B
			TEXTURE_BASE_LEVEL			= 0x813C
			TEXTURE_MAX_LEVEL			= 0x813D

TextureTarget		enum:
			TEXTURE_1D				= 0x0DE0
			TEXTURE_2D				= 0x0DE1
			TEXTURE_3D				= 0x806F # 1 I
			PROXY_TEXTURE_1D			= 0x8063
			PROXY_TEXTURE_2D			= 0x8064
			PROXY_TEXTURE_3D			= 0x8070

TextureUnit		enum:
			TEXTURE0				= 0x84C0
			TEXTURE1				= 0x84C1
			TEXTURE2				= 0x84C2
			TEXTURE3				= 0x84C3
			TEXTURE4				= 0x84C4
			TEXTURE5				= 0x84C5
			TEXTURE6				= 0x84C6
			TEXTURE7				= 0x84C7
			TEXTURE8				= 0x84C8
			TEXTURE9				= 0x84C9
			TEXTURE10				= 0x84CA
			TEXTURE11				= 0x84CB
			TEXTURE12				= 0x84CC
			TEXTURE13				= 0x84CD
			TEXTURE14				= 0x84CE
			TEXTURE15				= 0x84CF
			TEXTURE16				= 0x84D0
			TEXTURE17				= 0x84D1
			TEXTURE18				= 0x84D2
			TEXTURE19				= 0x84D3
			TEXTURE20				= 0x84D4
			TEXTURE21				= 0x84D5
			TEXTURE22				= 0x84D6
			TEXTURE23				= 0x84D7
			TEXTURE24				= 0x84D8
			TEXTURE25				= 0x84D9
			TEXTURE26				= 0x84DA
			TEXTURE27				= 0x84DB
			TEXTURE28				= 0x84DC
			TEXTURE29				= 0x84DD
			TEXTURE30				= 0x84DE
			TEXTURE31				= 0x84DF

TextureWrapMode		enum:
			CLAMP					= 0x2900
			REPEAT					= 0x2901
			CLAMP_TO_EDGE				= 0x812F

VertexPointerType	enum:
			use DataType SHORT
			use DataType INT
			use DataType FLOAT
			use DataType DOUBLE
