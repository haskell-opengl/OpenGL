###############################################################################
#
# OpenGL 1.4 enumerant specification, including all ARB extensions from #1 up
# to #25 which are relevant here (i.e. no GLX stuff, WGL stuff, etc.)
#
# This file was created in the following way:
# 
#  1) Use the SI's enum.spec (rev. 1.3) as a basis (it's OpenGL 1.2.1 compliant).
#  2) Apply some bug fixes, see http://haskell.org/HOpenGL/spec_bugs.html.
#  3) Inline the 1.1, 1.2, and 1.2.1 changes.
#  4) Make OpenGL 1.3 changes according to Appendix F of the 1.3 specs.
#  5) Make OpenGL 1.4 changes according to Appendix G of the 1.4 specs.
#  6) Remove the extensions part.
#  7) Rearrange definitions and declarations to get a better naming.
#  8) Merge the extension registry's enumext.spec (ongoing).
#  9) Unify ColorMaterialFace, CullFaceMode, and MaterialFace into Face.
# 10) Unify AlphaFunction, DepthFunction, and StencilFunction into ComparisonFunction.
# 11) Add USE_PREFIX special comments until constructors are unique.
#
# For the original files, see SGI's OpenGL sample implementation at:
#
#    http://oss.sgi.com/projects/ogl-sample/
#
###############################################################################

AccumOp enum:
	ACCUM						= 0x0100
	LOAD						= 0x0101
	RETURN						= 0x0102
	MULT						= 0x0103
	ADD						= 0x0104

# it's an enum: in the registry, not a mask:
# USE_PREFIX Server
AttribMask mask:
	CURRENT_BIT					= 0x00000001
	POINT_BIT					= 0x00000002
	LINE_BIT					= 0x00000004
	POLYGON_BIT					= 0x00000008
	POLYGON_STIPPLE_BIT				= 0x00000010
	PIXEL_MODE_BIT					= 0x00000020
	LIGHTING_BIT					= 0x00000040
	FOG_BIT						= 0x00000080
	DEPTH_BUFFER_BIT				= 0x00000100
	ACCUM_BUFFER_BIT				= 0x00000200
	STENCIL_BUFFER_BIT				= 0x00000400
	VIEWPORT_BIT					= 0x00000800
	TRANSFORM_BIT					= 0x00001000
	ENABLE_BIT					= 0x00002000
	COLOR_BUFFER_BIT				= 0x00004000
	HINT_BIT					= 0x00008000
	EVAL_BIT					= 0x00010000
	LIST_BIT					= 0x00020000
	TEXTURE_BIT					= 0x00040000
	SCISSOR_BIT					= 0x00080000
	MULTISAMPLE_BIT					= 0x20000000
	ALL_ATTRIB_BITS					= 0xFFFFFFFF

BeginMode enum:
	POINTS						= 0x0000
	LINES						= 0x0001
	LINE_LOOP					= 0x0002
	LINE_STRIP					= 0x0003
	TRIANGLES					= 0x0004
	TRIANGLE_STRIP					= 0x0005
	TRIANGLE_FAN					= 0x0006
	QUADS						= 0x0007
	QUAD_STRIP					= 0x0008
	POLYGON						= 0x0009

BlendEquationMode enum:
	LOGIC_OP					= 0x0BF1
	FUNC_ADD					= 0x8006
	MIN						= 0x8007
	MAX						= 0x8008
	FUNC_SUBTRACT					= 0x800A
	FUNC_REVERSE_SUBTRACT				= 0x800B

# USE_PREFIX DstFactor
BlendingFactorDest enum:
	ZERO						= 0
	ONE						= 1
	SRC_COLOR					= 0x0300
	ONE_MINUS_SRC_COLOR				= 0x0301
	SRC_ALPHA					= 0x0302
	ONE_MINUS_SRC_ALPHA				= 0x0303
	DST_ALPHA					= 0x0304
	ONE_MINUS_DST_ALPHA				= 0x0305
	CONSTANT_COLOR					= 0x8001
	ONE_MINUS_CONSTANT_COLOR			= 0x8002
	CONSTANT_ALPHA					= 0x8003
	ONE_MINUS_CONSTANT_ALPHA			= 0x8004

# USE_PREFIX SrcFactor
BlendingFactorSrc enum:
	use BlendingFactorDest ZERO
	use BlendingFactorDest ONE
	DST_COLOR					= 0x0306
	ONE_MINUS_DST_COLOR				= 0x0307
	SRC_ALPHA_SATURATE				= 0x0308
	use BlendingFactorDest SRC_ALPHA
	use BlendingFactorDest ONE_MINUS_SRC_ALPHA
	use BlendingFactorDest DST_ALPHA
	use BlendingFactorDest ONE_MINUS_DST_ALPHA
	use BlendingFactorDest CONSTANT_COLOR
	use BlendingFactorDest ONE_MINUS_CONSTANT_COLOR
	use BlendingFactorDest CONSTANT_ALPHA
	use BlendingFactorDest ONE_MINUS_CONSTANT_ALPHA

# USE_PREFIX Boolean
Boolean enum:
	FALSE						= 0
	TRUE						= 1

# it's an enum: in the registry, not a mask:
ClearBufferMask mask:
	use AttribMask COLOR_BUFFER_BIT
	use AttribMask ACCUM_BUFFER_BIT
	use AttribMask STENCIL_BUFFER_BIT
	use AttribMask DEPTH_BUFFER_BIT

ClientArrayType enum:
	VERTEX_ARRAY					= 0x8074
	NORMAL_ARRAY					= 0x8075
	COLOR_ARRAY					= 0x8076
	INDEX_ARRAY					= 0x8077
	TEXTURE_COORD_ARRAY				= 0x8078
	EDGE_FLAG_ARRAY					= 0x8079
	FOG_COORD_ARRAY					= 0x8457
	SECONDARY_COLOR_ARRAY				= 0x845E
	MATRIX_INDEX_ARRAY				= 0x8844	# ARB Extension #16: ARB_matrix_palette

# it's an enum: in the registry, not a mask:
ClientAttribMask mask:
	CLIENT_PIXEL_STORE_BIT				= 0x00000001
	CLIENT_VERTEX_ARRAY_BIT				= 0x00000002
	CLIENT_ALL_ATTRIB_BITS				= 0xFFFFFFFF

# 0x3000 through 0x3FFF are reserved for clip planes
ClipPlaneName enum:
	CLIP_PLANE0					= 0x3000
	CLIP_PLANE1					= 0x3001
	CLIP_PLANE2					= 0x3002
	CLIP_PLANE3					= 0x3003
	CLIP_PLANE4					= 0x3004
	CLIP_PLANE5					= 0x3005

# USE_PREFIX ColorMaterial
ColorMaterialParameter enum:
	use LightParameter AMBIENT
	use LightParameter DIFFUSE
	use LightParameter SPECULAR
	use MaterialParameter EMISSION
	use MaterialParameter AMBIENT_AND_DIFFUSE

# USE_PREFIX Color
ColorPointerType enum:
	use DataType BYTE
	use DataType UNSIGNED_BYTE
	use DataType SHORT
	use DataType UNSIGNED_SHORT
	use DataType INT
	use DataType UNSIGNED_INT
	use DataType FLOAT
	use DataType DOUBLE

ColorTableParameterPName enum:
	COLOR_TABLE_SCALE				= 0x80D6
	COLOR_TABLE_BIAS				= 0x80D7

ColorTableTarget enum:
	COLOR_TABLE					= 0x80D0
	POST_CONVOLUTION_COLOR_TABLE			= 0x80D1
	POST_COLOR_MATRIX_COLOR_TABLE			= 0x80D2
	PROXY_COLOR_TABLE				= 0x80D3
	PROXY_POST_CONVOLUTION_COLOR_TABLE		= 0x80D4
	PROXY_POST_COLOR_MATRIX_COLOR_TABLE		= 0x80D5

ComparisonFunction enum:
	NEVER						= 0x0200
	LESS						= 0x0201
	EQUAL						= 0x0202
	LEQUAL						= 0x0203
	GREATER						= 0x0204
	NOTEQUAL					= 0x0205
	GEQUAL						= 0x0206
	ALWAYS						= 0x0207

ConvolutionBorderMode enum:
	REDUCE						= 0x8016
	CONSTANT_BORDER					= 0x8151
	REPLICATE_BORDER				= 0x8153
	CONVOLUTION_BORDER_COLOR			= 0x8154

ConvolutionParameter enum:
	CONVOLUTION_BORDER_MODE				= 0x8013
	CONVOLUTION_FILTER_SCALE			= 0x8014
	CONVOLUTION_FILTER_BIAS				= 0x8015

ConvolutionTarget enum:
	CONVOLUTION_1D					= 0x8010
	CONVOLUTION_2D					= 0x8011

DataType enum:
	BYTE						= 0x1400
	UNSIGNED_BYTE					= 0x1401
	SHORT						= 0x1402
	UNSIGNED_SHORT					= 0x1403
	INT						= 0x1404
	UNSIGNED_INT					= 0x1405
	FLOAT						= 0x1406
	2_BYTES						= 0x1407
	3_BYTES						= 0x1408
	4_BYTES						= 0x1409
	DOUBLE						= 0x140A

# USE_PREFIX DrawBuffer
DrawBufferMode enum:
	NONE						= 0
	FRONT_LEFT					= 0x0400
	FRONT_RIGHT					= 0x0401
	BACK_LEFT					= 0x0402
	BACK_RIGHT					= 0x0403
	FRONT						= 0x0404
	BACK						= 0x0405
	LEFT						= 0x0406
	RIGHT						= 0x0407
	FRONT_AND_BACK					= 0x0408
	AUX0						= 0x0409
	AUX1						= 0x040A
	AUX2						= 0x040B
	AUX3						= 0x040C

# USE_PREFIX Cap
EnableCap enum:
	FOG						= 0x0B60
	LIGHTING					= 0x0B50
	use TextureTarget TEXTURE_1D
	use TextureTarget TEXTURE_2D
	LINE_STIPPLE					= 0x0B24
	POLYGON_STIPPLE					= 0x0B42
	CULL_FACE					= 0x0B44
	ALPHA_TEST					= 0x0BC0
	use TextureEnvMode BLEND
	INDEX_LOGIC_OP					= 0x0BF1
	COLOR_LOGIC_OP					= 0x0BF2
	DITHER						= 0x0BD0
	STENCIL_TEST					= 0x0B90
	DEPTH_TEST					= 0x0B71
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
	TEXTURE_GEN_S					= 0x0C60
	TEXTURE_GEN_T					= 0x0C61
	TEXTURE_GEN_R					= 0x0C62
	TEXTURE_GEN_Q					= 0x0C63
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
	POINT_SMOOTH					= 0x0B10
	LINE_SMOOTH					= 0x0B20
	POLYGON_SMOOTH					= 0x0B41
	SCISSOR_TEST					= 0x0C11
	COLOR_MATERIAL					= 0x0B57
	NORMALIZE					= 0x0BA1
	AUTO_NORMAL					= 0x0D80

	POLYGON_OFFSET_POINT				= 0x2A01
	POLYGON_OFFSET_LINE				= 0x2A02
	POLYGON_OFFSET_FILL				= 0x8037

	use ClientArrayType VERTEX_ARRAY
	use ClientArrayType NORMAL_ARRAY
	use ClientArrayType COLOR_ARRAY
	use ClientArrayType INDEX_ARRAY
	use ClientArrayType TEXTURE_COORD_ARRAY
	use ClientArrayType EDGE_FLAG_ARRAY
	use ClientArrayType FOG_COORD_ARRAY
	use ClientArrayType SECONDARY_COLOR_ARRAY
	use ClientArrayType MATRIX_INDEX_ARRAY				# ARB Extension #16: ARB_matrix_palette

	use ConvolutionTarget CONVOLUTION_1D
	use ConvolutionTarget CONVOLUTION_2D
	use SeparableTarget SEPARABLE_2D

	use HistogramTarget HISTOGRAM
	use MinMaxTarget MINMAX

	RESCALE_NORMAL					= 0x803A

	SHARED_TEXTURE_PALETTE				= 0x81FB

	use TextureTarget TEXTURE_3D

	MULTISAMPLE					= 0x809D	# ARB Extension #5: ARB_multisample
	SAMPLE_ALPHA_TO_COVERAGE			= 0x809E	# ARB Extension #5: ARB_multisample
	SAMPLE_ALPHA_TO_ONE				= 0x809F	# ARB Extension #5: ARB_multisample
	SAMPLE_COVERAGE					= 0x80A0	# ARB Extension #5: ARB_multisample

	use ColorTableTarget COLOR_TABLE
	use ColorTableTarget POST_CONVOLUTION_COLOR_TABLE
	use ColorTableTarget POST_COLOR_MATRIX_COLOR_TABLE

	use GetPName COLOR_SUM

	use TextureTarget TEXTURE_CUBE_MAP				# ARB Extension #7: ARB_texture_cube_map

	WEIGHT_SUM_UNITY				= 0x86A6	# ARB Extension #15: ARB_vertex_blend
	VERTEX_BLEND					= 0x86A7	# ARB Extension #15: ARB_vertex_blend
	WEIGHT_ARRAY					= 0x86AD	# ARB Extension #15: ARB_vertex_blend

	use MatrixMode MATRIX_PALETTE					# ARB Extension #16: ARB_matrix_palette

ErrorCode enum:
	NO_ERROR					= 0
	INVALID_ENUM					= 0x0500
	INVALID_VALUE					= 0x0501
	INVALID_OPERATION				= 0x0502
	STACK_OVERFLOW					= 0x0503
	STACK_UNDERFLOW					= 0x0504
	OUT_OF_MEMORY					= 0x0505
	TABLE_TOO_LARGE					= 0x8031

Face enum:
	use DrawBufferMode FRONT
	use DrawBufferMode BACK
	use DrawBufferMode FRONT_AND_BACK

FeedBackType enum:
	2D						= 0x0600
	3D						= 0x0601
	3D_COLOR					= 0x0602
	3D_COLOR_TEXTURE				= 0x0603
	4D_COLOR_TEXTURE				= 0x0604

FeedBackToken enum:
	PASS_THROUGH_TOKEN				= 0x0700
	POINT_TOKEN					= 0x0701
	LINE_TOKEN					= 0x0702
	POLYGON_TOKEN					= 0x0703
	BITMAP_TOKEN					= 0x0704
	DRAW_PIXEL_TOKEN				= 0x0705
	COPY_PIXEL_TOKEN				= 0x0706
	LINE_RESET_TOKEN				= 0x0707

FogCoord enum:
	FOG_COORD					= 0x8451
	FRAGMENT_DEPTH					= 0x8452

FogMode enum:
	use TextureMinFilter LINEAR
	EXP						= 0x0800
	EXP2						= 0x0801

FogParameter enum:
	FOG_INDEX					= 0x0B61
	FOG_DENSITY					= 0x0B62
	FOG_START					= 0x0B63
	FOG_END						= 0x0B64
	FOG_MODE					= 0x0B65
	FOG_COLOR					= 0x0B66
	FOG_COORD_SRC					= 0x8450

FrontFaceDirection enum:
	CW						= 0x0900
	CCW						= 0x0901

# USE_PREFIX Get
GetColorTableParameterPName enum:
	use ColorTableParameterPName COLOR_TABLE_SCALE
	use ColorTableParameterPName COLOR_TABLE_BIAS
	COLOR_TABLE_FORMAT				= 0x80D8
	COLOR_TABLE_WIDTH				= 0x80D9
	COLOR_TABLE_RED_SIZE				= 0x80DA
	COLOR_TABLE_GREEN_SIZE				= 0x80DB
	COLOR_TABLE_BLUE_SIZE				= 0x80DC
	COLOR_TABLE_ALPHA_SIZE				= 0x80DD
	COLOR_TABLE_LUMINANCE_SIZE			= 0x80DE
	COLOR_TABLE_INTENSITY_SIZE			= 0x80DF

# USE_PREFIX Get
GetConvolutionParameter enum:
	CONVOLUTION_BORDER_COLOR			= 0x8154	# Not in the registry, but in the OpenGL 1.3 spec
	use ConvolutionParameterPName CONVOLUTION_BORDER_MODE
	use ConvolutionParameterPName CONVOLUTION_FILTER_SCALE
	use ConvolutionParameterPName CONVOLUTION_FILTER_BIAS
	CONVOLUTION_FORMAT				= 0x8017
	CONVOLUTION_WIDTH				= 0x8018
	CONVOLUTION_HEIGHT				= 0x8019
	MAX_CONVOLUTION_WIDTH				= 0x801A
	MAX_CONVOLUTION_HEIGHT				= 0x801B

GetHistogramParameterPName enum:
	HISTOGRAM_WIDTH					= 0x8026
	HISTOGRAM_FORMAT				= 0x8027
	HISTOGRAM_RED_SIZE				= 0x8028
	HISTOGRAM_GREEN_SIZE				= 0x8029
	HISTOGRAM_BLUE_SIZE				= 0x802A
	HISTOGRAM_ALPHA_SIZE				= 0x802B
	HISTOGRAM_LUMINANCE_SIZE			= 0x802C
	HISTOGRAM_SINK					= 0x802D

GetMapQuery enum:
	COEFF						= 0x0A00
	ORDER						= 0x0A01
	DOMAIN						= 0x0A02

GetMinmaxParameterPName enum:
	MINMAX_FORMAT					= 0x802F
	MINMAX_SINK					= 0x8030

# USE_PREFIX Get
GetPixelMap enum:
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

GetPointervPName enum:
	VERTEX_ARRAY_POINTER				= 0x808E
	NORMAL_ARRAY_POINTER				= 0x808F
	COLOR_ARRAY_POINTER				= 0x8090
	INDEX_ARRAY_POINTER				= 0x8091
	TEXTURE_COORD_ARRAY_POINTER			= 0x8092
	EDGE_FLAG_ARRAY_POINTER				= 0x8093
	FOG_COORD_ARRAY_POINTER				= 0x8456
	SECONDARY_COLOR_ARRAY_POINTER			= 0x845D
	FEEDBACK_BUFFER_POINTER				= 0x0DF0
	SELECTION_BUFFER_POINTER			= 0x0DF3
	WEIGHT_ARRAY_POINTER				= 0x86AC	# ARB Extension #15: ARB_vertex_blend
	MATRIX_INDEX_ARRAY_POINTER			= 0x8849	# ARB Extension #16: ARB_matrix_palette

# USE_PREFIX Get
GetPName enum:
	CURRENT_COLOR					= 0x0B00	# 4 F
	CURRENT_INDEX					= 0x0B01	# 1 F
	CURRENT_NORMAL					= 0x0B02	# 3 F
	CURRENT_TEXTURE_COORDS				= 0x0B03	# 4 F
	CURRENT_RASTER_COLOR				= 0x0B04	# 4 F
	CURRENT_RASTER_INDEX				= 0x0B05	# 1 F
	CURRENT_RASTER_TEXTURE_COORDS			= 0x0B06	# 4 F
	CURRENT_RASTER_POSITION				= 0x0B07	# 4 F
	CURRENT_RASTER_POSITION_VALID			= 0x0B08	# 1 I
	CURRENT_RASTER_DISTANCE				= 0x0B09	# 1 F
	CURRENT_MATRIX_INDEX				= 0x8845	# 1 F, ARB Extension #16: ARB_matrix_palette

	use Enable POINT_SMOOTH						# 1 I
	POINT_SIZE					= 0x0B11	# 1 F
	POINT_SIZE_RANGE				= 0x0B12	# 2 F
	POINT_SIZE_GRANULARITY				= 0x0B13	# 1 F

	use Enable LINE_SMOOTH						# 1 I
	LINE_WIDTH					= 0x0B21	# 1 F
	LINE_WIDTH_RANGE				= 0x0B22	# 2 F
	LINE_WIDTH_GRANULARITY				= 0x0B23	# 1 F
	use Enable LINE_STIPPLE						# 1 I
	LINE_STIPPLE_PATTERN				= 0x0B25	# 1 I
	LINE_STIPPLE_REPEAT				= 0x0B26	# 1 I
	SMOOTH_POINT_SIZE_RANGE				= 0x0B12	# 2 F
	SMOOTH_POINT_SIZE_GRANULARITY			= 0x0B13	# 1 F
	SMOOTH_LINE_WIDTH_RANGE				= 0x0B22	# 2 F
	SMOOTH_LINE_WIDTH_GRANULARITY			= 0x0B23	# 1 F
	ALIASED_POINT_SIZE_RANGE			= 0x846D	# 2 F
	ALIASED_LINE_WIDTH_RANGE			= 0x846E	# 2 F

	LIST_MODE					= 0x0B30	# 1 I
	MAX_LIST_NESTING				= 0x0B31	# 1 I
	LIST_BASE					= 0x0B32	# 1 I
	LIST_INDEX					= 0x0B33	# 1 I

	POLYGON_MODE					= 0x0B40	# 2 I
	use Enable POLYGON_SMOOTH					# 1 I
	use Enable POLYGON_STIPPLE					# 1 I
	EDGE_FLAG					= 0x0B43	# 1 I
	use Enable CULL_FACE						# 1 I
	CULL_FACE_MODE					= 0x0B45	# 1 I
	FRONT_FACE					= 0x0B46	# 1 I

	use Enable LIGHTING						# 1 I
	use LightModelParameter LIGHT_MODEL_LOCAL_VIEWER		# 1 I
	use LightModelParameter LIGHT_MODEL_TWO_SIDE			# 1 I
	use LightModelParameter LIGHT_MODEL_AMBIENT			# 4 F
	SHADE_MODEL					= 0x0B54	# 1 I
	COLOR_MATERIAL_FACE				= 0x0B55	# 1 I
	COLOR_MATERIAL_PARAMETER			= 0x0B56	# 1 I
	use Enable COLOR_MATERIAL					# 1 I

	use Enable FOG							# 1 I
	use FogParameter FOG_INDEX					# 1 I
	use FogParameter FOG_DENSITY					# 1 F
	use FogParameter FOG_START					# 1 F
	use FogParameter FOG_END					# 1 F
	use FogParameter FOG_MODE					# 1 I
	use FogParameter FOG_COLOR					# 4 F
	use FogParameter FOG_COORD_SRC					# 1 I
	CURRENT_FOG_COORD				= 0x8453	# 1 F

	DEPTH_RANGE					= 0x0B70	# 2 F
	use Enable DEPTH_TEST						# 1 I
	DEPTH_WRITEMASK					= 0x0B72	# 1 I
	DEPTH_CLEAR_VALUE				= 0x0B73	# 1 F
	DEPTH_FUNC					= 0x0B74	# 1 I

	ACCUM_CLEAR_VALUE				= 0x0B80	# 4 F

	use Enable STENCIL_TEST						# 1 I
	STENCIL_CLEAR_VALUE				= 0x0B91	# 1 I
	STENCIL_FUNC					= 0x0B92	# 1 I
	STENCIL_VALUE_MASK				= 0x0B93	# 1 I
	STENCIL_FAIL					= 0x0B94	# 1 I
	STENCIL_PASS_DEPTH_FAIL				= 0x0B95	# 1 I
	STENCIL_PASS_DEPTH_PASS				= 0x0B96	# 1 I
	STENCIL_REF					= 0x0B97	# 1 I
	STENCIL_WRITEMASK				= 0x0B98	# 1 I

	MATRIX_MODE					= 0x0BA0	# 1 I 
	use Enable NORMALIZE						# 1 I 
	VIEWPORT					= 0x0BA2	# 4 I 
	MODELVIEW_STACK_DEPTH				= 0x0BA3	# 1 I 
	PROJECTION_STACK_DEPTH				= 0x0BA4	# 1 I 
	TEXTURE_STACK_DEPTH				= 0x0BA5	# 1 I 
	MODELVIEW_MATRIX				= 0x0BA6	# 16 F
	PROJECTION_MATRIX				= 0x0BA7	# 16 F
	TEXTURE_MATRIX					= 0x0BA8	# 16 F

	ATTRIB_STACK_DEPTH				= 0x0BB0	# 1 I
	CLIENT_ATTRIB_STACK_DEPTH			= 0x0BB1	# 1 I

	use Enable ALPHA_TEST						# 1 I
	ALPHA_TEST_FUNC					= 0x0BC1	# 1 I
	ALPHA_TEST_REF					= 0x0BC2	# 1 F

	use Enable DITHER						# 1 I

	BLEND_DST					= 0x0BE0	# 1 I
	BLEND_SRC					= 0x0BE1	# 1 I
	use TextureEnvMode BLEND					# 1 I

	LOGIC_OP_MODE					= 0x0BF0	# 1 I
	use Enable INDEX_LOGIC_OP					# 1 I
	use BlendEquationMode LOGIC_OP					# 1 I
	use Enable COLOR_LOGIC_OP					# 1 I

	AUX_BUFFERS					= 0x0C00	# 1 I
	DRAW_BUFFER					= 0x0C01	# 1 I
	READ_BUFFER					= 0x0C02	# 1 I

	SCISSOR_BOX					= 0x0C10	# 4 I
	use Enable SCISSOR_TEST						# 1 I

	INDEX_CLEAR_VALUE				= 0x0C20	# 1 I
	INDEX_WRITEMASK					= 0x0C21	# 1 I
	COLOR_CLEAR_VALUE				= 0x0C22	# 4 F
	COLOR_WRITEMASK					= 0x0C23	# 4 I

	INDEX_MODE					= 0x0C30	# 1 I
	RGBA_MODE					= 0x0C31	# 1 I
	DOUBLEBUFFER					= 0x0C32	# 1 I
	STEREO						= 0x0C33	# 1 I

	RENDER_MODE					= 0x0C40	# 1 I

	use HintTarget PERSPECTIVE_CORRECTION_HINT			# 1 I
	use HintTarget POINT_SMOOTH_HINT				# 1 I
	use HintTarget LINE_SMOOTH_HINT					# 1 I
	use HintTarget POLYGON_SMOOTH_HINT				# 1 I
	use HintTarget FOG_HINT						# 1 I
	use HintTarget GENERATE_MIPMAP_HINT				# 1 I
	use HintTarget TEXTURE_COMPRESSION_HINT				# 1 I, ARB Extension #12: ARB_texture_compression

	use Enable TEXTURE_GEN_S					# 1 I
	use Enable TEXTURE_GEN_T					# 1 I
	use Enable TEXTURE_GEN_R					# 1 I
	use Enable TEXTURE_GEN_Q					# 1 I

	PIXEL_MAP_I_TO_I_SIZE				= 0x0CB0	# 1 I
	PIXEL_MAP_S_TO_S_SIZE				= 0x0CB1	# 1 I
	PIXEL_MAP_I_TO_R_SIZE				= 0x0CB2	# 1 I
	PIXEL_MAP_I_TO_G_SIZE				= 0x0CB3	# 1 I
	PIXEL_MAP_I_TO_B_SIZE				= 0x0CB4	# 1 I
	PIXEL_MAP_I_TO_A_SIZE				= 0x0CB5	# 1 I
	PIXEL_MAP_R_TO_R_SIZE				= 0x0CB6	# 1 I
	PIXEL_MAP_G_TO_G_SIZE				= 0x0CB7	# 1 I
	PIXEL_MAP_B_TO_B_SIZE				= 0x0CB8	# 1 I
	PIXEL_MAP_A_TO_A_SIZE				= 0x0CB9	# 1 I

	use PixelStore UNPACK_SWAP_BYTES				# 1 I
	use PixelStore UNPACK_LSB_FIRST					# 1 I
	use PixelStore UNPACK_ROW_LENGTH				# 1 I
	use PixelStore UNPACK_SKIP_ROWS					# 1 I
	use PixelStore UNPACK_SKIP_PIXELS				# 1 I
	use PixelStore UNPACK_ALIGNMENT					# 1 I

	use PixelStore PACK_SWAP_BYTES					# 1 I
	use PixelStore PACK_LSB_FIRST					# 1 I
	use PixelStore PACK_ROW_LENGTH					# 1 I
	use PixelStore PACK_SKIP_ROWS					# 1 I
	use PixelStore PACK_SKIP_PIXELS					# 1 I
	use PixelStore PACK_ALIGNMENT					# 1 I

	use PixelTransfer MAP_COLOR					# 1 I
	use PixelTransfer MAP_STENCIL					# 1 I
	use PixelTransfer INDEX_SHIFT					# 1 I
	use PixelTransfer INDEX_OFFSET					# 1 I
	use PixelTransfer RED_SCALE					# 1 F
	use PixelTransfer RED_BIAS					# 1 F
	ZOOM_X						= 0x0D16	# 1 F
	ZOOM_Y						= 0x0D17	# 1 F
	use PixelTransfer GREEN_SCALE					# 1 F
	use PixelTransfer GREEN_BIAS					# 1 F
	use PixelTransfer BLUE_SCALE					# 1 F
	use PixelTransfer BLUE_BIAS					# 1 F
	use PixelTransfer ALPHA_SCALE					# 1 F
	use PixelTransfer ALPHA_BIAS					# 1 F
	use PixelTransfer DEPTH_SCALE					# 1 F
	use PixelTransfer DEPTH_BIAS					# 1 F

	MAX_EVAL_ORDER					= 0x0D30	# 1 I
	MAX_LIGHTS					= 0x0D31	# 1 I
	MAX_CLIP_PLANES					= 0x0D32	# 1 I
	MAX_TEXTURE_SIZE				= 0x0D33	# 1 I
	MAX_PIXEL_MAP_TABLE				= 0x0D34	# 1 I
	MAX_ATTRIB_STACK_DEPTH				= 0x0D35	# 1 I
	MAX_MODELVIEW_STACK_DEPTH			= 0x0D36	# 1 I
	MAX_NAME_STACK_DEPTH				= 0x0D37	# 1 I
	MAX_PROJECTION_STACK_DEPTH			= 0x0D38	# 1 I
	MAX_TEXTURE_STACK_DEPTH				= 0x0D39	# 1 I
	MAX_VIEWPORT_DIMS				= 0x0D3A	# 2 F
	MAX_CLIENT_ATTRIB_STACK_DEPTH			= 0x0D3B	# 1 I

	SUBPIXEL_BITS					= 0x0D50	# 1 I
	INDEX_BITS					= 0x0D51	# 1 I
	RED_BITS					= 0x0D52	# 1 I
	GREEN_BITS					= 0x0D53	# 1 I
	BLUE_BITS					= 0x0D54	# 1 I
	ALPHA_BITS					= 0x0D55	# 1 I
	DEPTH_BITS					= 0x0D56	# 1 I
	STENCIL_BITS					= 0x0D57	# 1 I
	ACCUM_RED_BITS					= 0x0D58	# 1 I
	ACCUM_GREEN_BITS				= 0x0D59	# 1 I
	ACCUM_BLUE_BITS					= 0x0D5A	# 1 I
	ACCUM_ALPHA_BITS				= 0x0D5B	# 1 I

	NAME_STACK_DEPTH				= 0x0D70	# 1 I

	use Enable AUTO_NORMAL						# 1 I

	use MapTarget MAP1_COLOR_4					# 1 I
	use MapTarget MAP1_INDEX					# 1 I
	use MapTarget MAP1_NORMAL					# 1 I
	use MapTarget MAP1_TEXTURE_COORD_1				# 1 I
	use MapTarget MAP1_TEXTURE_COORD_2				# 1 I
	use MapTarget MAP1_TEXTURE_COORD_3				# 1 I
	use MapTarget MAP1_TEXTURE_COORD_4				# 1 I
	use MapTarget MAP1_VERTEX_3					# 1 I
	use MapTarget MAP1_VERTEX_4					# 1 I

	use MapTarget MAP2_COLOR_4					# 1 I
	use MapTarget MAP2_INDEX					# 1 I
	use MapTarget MAP2_NORMAL					# 1 I
	use MapTarget MAP2_TEXTURE_COORD_1				# 1 I
	use MapTarget MAP2_TEXTURE_COORD_2				# 1 I
	use MapTarget MAP2_TEXTURE_COORD_3				# 1 I
	use MapTarget MAP2_TEXTURE_COORD_4				# 1 I
	use MapTarget MAP2_VERTEX_3					# 1 I
	use MapTarget MAP2_VERTEX_4					# 1 I

	MAP1_GRID_DOMAIN				= 0x0DD0	# 2 F
	MAP1_GRID_SEGMENTS				= 0x0DD1	# 1 I
	MAP2_GRID_DOMAIN				= 0x0DD2	# 4 F
	MAP2_GRID_SEGMENTS				= 0x0DD3	# 2 I

	use TextureTarget TEXTURE_1D					# 1 I
	use TextureTarget TEXTURE_2D					# 1 I

	FEEDBACK_BUFFER_SIZE				= 0x0DF1	# 1 I
	FEEDBACK_BUFFER_TYPE				= 0x0DF2	# 1 I

	SELECTION_BUFFER_SIZE				= 0x0DF4	# 1 I

	POLYGON_OFFSET_UNITS				= 0x2A00	# 1 F
	use Enable POLYGON_OFFSET_POINT					# 1 I
	use Enable POLYGON_OFFSET_LINE					# 1 I
	use Enable POLYGON_OFFSET_FILL					# 1 I
	POLYGON_OFFSET_FACTOR				= 0x8038	# 1 F

	TEXTURE_BINDING_1D				= 0x8068	# 1 I
	TEXTURE_BINDING_2D				= 0x8069	# 1 I
	TEXTURE_BINDING_3D				= 0x806A	# 1 I

	use ClientArrayType VERTEX_ARRAY				# 1 I
	use ClientArrayType NORMAL_ARRAY				# 1 I
	use ClientArrayType COLOR_ARRAY					# 1 I
	use ClientArrayType INDEX_ARRAY					# 1 I
	use ClientArrayType TEXTURE_COORD_ARRAY				# 1 I
	use ClientArrayType EDGE_FLAG_ARRAY				# 1 I
	use ClientArrayType FOG_COORD_ARRAY			# 1 I
	use ClientArrayType SECONDARY_COLOR_ARRAY			# 1 I
	use ClientArrayType MATRIX_INDEX_ARRAY				# 1 I, ARB Extension #16: ARB_matrix_palette

	VERTEX_ARRAY_SIZE				= 0x807A	# 1 I
	VERTEX_ARRAY_TYPE				= 0x807B	# 1 I
	VERTEX_ARRAY_STRIDE				= 0x807C	# 1 I

	NORMAL_ARRAY_TYPE				= 0x807E	# 1 I
	NORMAL_ARRAY_STRIDE				= 0x807F	# 1 I

	COLOR_ARRAY_SIZE				= 0x8081	# 1 I
	COLOR_ARRAY_TYPE				= 0x8082	# 1 I
	COLOR_ARRAY_STRIDE				= 0x8083	# 1 I

	INDEX_ARRAY_TYPE				= 0x8085	# 1 I
	INDEX_ARRAY_STRIDE				= 0x8086	# 1 I

	TEXTURE_COORD_ARRAY_SIZE			= 0x8088	# 1 I
	TEXTURE_COORD_ARRAY_TYPE			= 0x8089	# 1 I
	TEXTURE_COORD_ARRAY_STRIDE			= 0x808A	# 1 I

	EDGE_FLAG_ARRAY_STRIDE				= 0x808C	# 1 I

	FOG_COORD_ARRAY_TYPE				= 0x8454	# 1 I
	FOG_COORD_ARRAY_STRIDE				= 0x8455	# 1 I

	SECONDARY_COLOR_ARRAY_SIZE			= 0x845A	# 1 I
	SECONDARY_COLOR_ARRAY_TYPE			= 0x845B	# 1 I
	SECONDARY_COLOR_ARRAY_STRIDE			= 0x845C	# 1 I

	MATRIX_INDEX_ARRAY_SIZE				= 0x8846	# 1 I, ARB Extension #16: ARB_matrix_palette
	MATRIX_INDEX_ARRAY_TYPE				= 0x8847	# 1 I, ARB Extension #16: ARB_matrix_palette
	MATRIX_INDEX_ARRAY_STRIDE			= 0x8848	# 1 I, ARB Extension #16: ARB_matrix_palette

	use ClipPlaneName CLIP_PLANE0					# 1 I
	use ClipPlaneName CLIP_PLANE1					# 1 I
	use ClipPlaneName CLIP_PLANE2					# 1 I
	use ClipPlaneName CLIP_PLANE3					# 1 I
	use ClipPlaneName CLIP_PLANE4					# 1 I
	use ClipPlaneName CLIP_PLANE5					# 1 I

	use LightName LIGHT0						# 1 I
	use LightName LIGHT1						# 1 I
	use LightName LIGHT2						# 1 I
	use LightName LIGHT3						# 1 I
	use LightName LIGHT4						# 1 I
	use LightName LIGHT5						# 1 I
	use LightName LIGHT6						# 1 I
	use LightName LIGHT7						# 1 I

	TRANSPOSE_MODELVIEW_MATRIX			= 0x84E3	# 16 F, ARB Extension #3: ARB_transpose_matrix
	TRANSPOSE_PROJECTION_MATRIX			= 0x84E4	# 16 F, ARB Extension #3: ARB_transpose_matrix
	TRANSPOSE_TEXTURE_MATRIX			= 0x84E5	# 16 F, ARB Extension #3: ARB_transpose_matrix
	TRANSPOSE_COLOR_MATRIX				= 0x84E6	# 16 F, ARB Extension #3: ARB_transpose_matrix

	use LightModelParameter LIGHT_MODEL_COLOR_CONTROL		# 1 I

	BLEND_COLOR					= 0x8005	# 4 F

	BLEND_EQUATION					= 0x8009	# 1 I

	use ColorTableTarget COLOR_TABLE				# 1 I
	use ColorTableTarget POST_CONVOLUTION_COLOR_TABLE		# 1 I
	use ColorTableTarget POST_COLOR_MATRIX_COLOR_TABLE		# 1 I

	use ConvolutionTarget CONVOLUTION_1D				# 1 I
	use ConvolutionTarget CONVOLUTION_2D				# 1 I
	use SeparableTarget SEPARABLE_2D				# 1 I
	use PixelTransfer POST_CONVOLUTION_RED_SCALE			# 1 F
	use PixelTransfer POST_CONVOLUTION_GREEN_SCALE			# 1 F
	use PixelTransfer POST_CONVOLUTION_BLUE_SCALE			# 1 F
	use PixelTransfer POST_CONVOLUTION_ALPHA_SCALE			# 1 F
	use PixelTransfer POST_CONVOLUTION_RED_BIAS			# 1 F
	use PixelTransfer POST_CONVOLUTION_GREEN_BIAS			# 1 F
	use PixelTransfer POST_CONVOLUTION_BLUE_BIAS			# 1 F
	use PixelTransfer POST_CONVOLUTION_ALPHA_BIAS			# 1 F

	use HistogramTarget HISTOGRAM					# 1 I
	use MinMaxTarget MINMAX						# 1 I

	COLOR_SUM					= 0x8458	# 1 I
	CURRENT_SECONDARY_COLOR				= 0x8459	# 3 F

	use Enable RESCALE_NORMAL					# 1 I

	use EnableCap SHARED_TEXTURE_PALETTE				# 1 I

	TEXTURE_3D_BINDING				= 0x806A	# 1 I

	use PixelStore PACK_SKIP_IMAGES					# 1 I
	use PixelStore PACK_IMAGE_HEIGHT				# 1 F
	use PixelStore UNPACK_SKIP_IMAGES				# 1 I
	use PixelStore UNPACK_IMAGE_HEIGHT				# 1 F

	use TextureTarget TEXTURE_3D					# 1 I
	MAX_3D_TEXTURE_SIZE				= 0x8073	# 1 I

	MAX_TEXTURE_LOD_BIAS				= 0x84FD	# 1 F
	MAX_TEXTURE_MAX_ANISOTROPY			= 0x84FF	# 1 F

	use EnableCap MULTISAMPLE					# 1 I, ARB Extension #5: ARB_multisample
	use EnableCap SAMPLE_ALPHA_TO_COVERAGE				# 1 I, ARB Extension #5: ARB_multisample
	use EnableCap SAMPLE_ALPHA_TO_ONE				# 1 I, ARB Extension #5: ARB_multisample
	use EnableCap SAMPLE_COVERAGE					# 1 I, ARB Extension #5: ARB_multisample
	SAMPLE_BUFFERS					= 0x80A8	# 1 I, ARB Extension #5: ARB_multisample
	SAMPLES						= 0x80A9	# 1 I, ARB Extension #5: ARB_multisample
	SAMPLE_COVERAGE_VALUE				= 0x80AA	# 1 F, ARB Extension #5: ARB_multisample
	SAMPLE_COVERAGE_INVERT				= 0x80AB	# 1 I, ARB Extension #5: ARB_multisample

	use PointParameterName POINT_SIZE_MIN				# 1 F, ARB Extension #14: ARB_point_parameters (promoted to core in 1.4)
	use PointParameterName POINT_SIZE_MAX				# 1 F, ARB Extension #14: ARB_point_parameters (promoted to core in 1.4)
	use PointParameterName POINT_FADE_THRESHOLD_SIZE		# 1 F, ARB Extension #14: ARB_point_parameters (promoted to core in 1.4)
	use PointParameterName POINT_DISTANCE_ATTENUATION		# 3 F, ARB Extension #14: ARB_point_parameters (promoted to core in 1.4)

	COLOR_MATRIX					= 0x80B1	# 16 F
	COLOR_MATRIX_STACK_DEPTH			= 0x80B2	# 1 I
	MAX_COLOR_MATRIX_STACK_DEPTH			= 0x80B3	# 1 I
	use PixelTransfer POST_COLOR_MATRIX_RED_SCALE			# 1 F
	use PixelTransfer POST_COLOR_MATRIX_GREEN_SCALE			# 1 F
	use PixelTransfer POST_COLOR_MATRIX_BLUE_SCALE			# 1 F
	use PixelTransfer POST_COLOR_MATRIX_ALPHA_SCALE			# 1 F
	use PixelTransfer POST_COLOR_MATRIX_RED_BIAS			# 1 F
	use PixelTransfer POST_COLOR_MATRIX_GREEN_BIAS			# 1 F
	use PixelTransfer POST_COLOR_MATRIX_BLUE_BIAS			# 1 F
	use PixelTransfer POST_COLOR_MATRIX_ALPHA_BIAS			# 1 F

	MAX_ELEMENTS_VERTICES				= 0x80E8	# Not in the registry, but in the OpenGL 1.3 spec
	MAX_ELEMENTS_INDICES				= 0x80E9	# Not in the registry, but in the OpenGL 1.3 spec

	ACTIVE_TEXTURE					= 0x84E0	# 1 I, ARB Extension #1: ARB_multitexture
	CLIENT_ACTIVE_TEXTURE				= 0x84E1	# 1 I, ARB Extension #1: ARB_multitexture
	MAX_TEXTURE_UNITS				= 0x84E2	# 1 I, ARB Extension #1: ARB_multitexture

	use TextureTarget TEXTURE_CUBE_MAP				# ARB Extension #7: ARB_texture_cube_map
	MAX_CUBE_MAP_TEXTURE_SIZE			= 0x851C	# ARB Extension #7: ARB_texture_cube_map

	NUM_COMPRESSED_TEXTURE_FORMATS			= 0x86A2	# ARB Extension #12: ARB_texture_compression
	COMPRESSED_TEXTURE_FORMATS			= 0x86A3	# ARB Extension #12: ARB_texture_compression

	MAX_VERTEX_UNITS				= 0x86A4	# ARB Extension #15: ARB_vertex_blend
	ACTIVE_VERTEX_UNITS				= 0x86A5	# ARB Extension #15: ARB_vertex_blend
	use EnableCap WEIGHT_SUM_UNITY					# ARB Extension #15: ARB_vertex_blend
	use EnableCap VERTEX_BLEND					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW0					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW1					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW2					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW3					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW4					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW5					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW6					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW7					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW8					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW9					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW10					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW11					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW12					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW13					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW14					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW15					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW16					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW17					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW18					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW19					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW20					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW21					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW22					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW23					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW24					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW25					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW26					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW27					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW28					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW29					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW30					# ARB Extension #15: ARB_vertex_blend
	use MatrixMode MODELVIEW31					# ARB Extension #15: ARB_vertex_blend
	CURRENT_WEIGHT					= 0x86A8	# ARB Extension #15: ARB_vertex_blend
	WEIGHT_ARRAY_TYPE				= 0x86A9	# ARB Extension #15: ARB_vertex_blend
	WEIGHT_ARRAY_STRIDE				= 0x86AA	# ARB Extension #15: ARB_vertex_blend
	WEIGHT_ARRAY_SIZE				= 0x86AB	# ARB Extension #15: ARB_vertex_blend
	use EnableCap WEIGHT_ARRAY					# ARB Extension #15: ARB_vertex_blend

	use MatrixMode MATRIX_PALETTE					# ARB Extension #16: ARB_matrix_palette
	MAX_MATRIX_PALETTE_STACK_DEPTH			= 0x8841	# ARB Extension #16: ARB_matrix_palette
	MAX_PALETTE_MATRICES				= 0x8842	# ARB Extension #16: ARB_matrix_palette
	CURRENT_PALETTE_MATRIX				= 0x8843	# ARB Extension #16: ARB_matrix_palette

# USE_PREFIX Get
GetTextureParameter enum:
	use TextureParameterName TEXTURE_MAG_FILTER
	use TextureParameterName TEXTURE_MIN_FILTER

	use TextureParameterName TEXTURE_WRAP_R
	use TextureParameterName TEXTURE_WRAP_S
	use TextureParameterName TEXTURE_WRAP_T

	TEXTURE_WIDTH					= 0x1000
	TEXTURE_HEIGHT					= 0x1001
	TEXTURE_DEPTH					= 0x8071

	TEXTURE_INTERNAL_FORMAT				= 0x1003
	use TextureParameterName TEXTURE_BORDER_COLOR
	TEXTURE_BORDER					= 0x1005

	TEXTURE_RED_SIZE				= 0x805C
	TEXTURE_GREEN_SIZE				= 0x805D
	TEXTURE_BLUE_SIZE				= 0x805E
	TEXTURE_ALPHA_SIZE				= 0x805F
	TEXTURE_LUMINANCE_SIZE				= 0x8060
	TEXTURE_INTENSITY_SIZE				= 0x8061
	TEXTURE_DEPTH_SIZE				= 0x884A	# ARB Extension #22: ARB_depth_texture (promoted to core in 1.4)
	TEXTURE_COMPRESSED_IMAGE_SIZE			= 0x86A0	# ARB Extension #12: ARB_texture_compression
	TEXTURE_COMPRESSED				= 0x86A1	# ARB Extension #12: ARB_texture_compression

	use TextureParameterName TEXTURE_PRIORITY
	TEXTURE_RESIDENT				= 0x8067

	use TextureParameterName TEXTURE_MIN_LOD
	use TextureParameterName TEXTURE_MAX_LOD
	use TextureParameterName TEXTURE_BASE_LEVEL
	use TextureParameterName TEXTURE_MAX_LEVEL

	use TextureParameterName GENERATE_MIPMAP

	use TextureParameterName TEXTURE_MAX_ANISOTROPY

	use TextureParameterName DEPTH_TEXTURE_MODE			# ARB Extension #22: ARB_depth_texture (promoted to core in 1.4)

	use TextureParameterName TEXTURE_COMPARE_MODE			# ARB Extension #23: ARB_shadow (promoted to core in 1.4)
	use TextureParameterName TEXTURE_COMPARE_FUNC			# ARB Extension #23: ARB_shadow (promoted to core in 1.4)

	use TextureParameterName TEXTURE_COMPARE_FAIL_VALUE		# ARB Extension #24: ARB_shadow_ambient

HintMode enum:
	DONT_CARE					= 0x1100
	FASTEST						= 0x1101
	NICEST						= 0x1102

HintTarget enum:
	PERSPECTIVE_CORRECTION_HINT			= 0x0C50
	POINT_SMOOTH_HINT				= 0x0C51
	LINE_SMOOTH_HINT				= 0x0C52
	POLYGON_SMOOTH_HINT				= 0x0C53
	FOG_HINT					= 0x0C54
	GENERATE_MIPMAP_HINT				= 0x8192
	TEXTURE_COMPRESSION_HINT			= 0x84EF	# ARB Extension #12: ARB_texture_compression

HistogramTarget enum:
	HISTOGRAM					= 0x8024
	PROXY_HISTOGRAM					= 0x8025

# USE_PREFIX Index
IndexPointerType enum:
	use DataType SHORT
	use DataType INT
	use DataType FLOAT
	use DataType DOUBLE

InterleavedArrays enum:
	V2F						= 0x2A20
	V3F						= 0x2A21
	C4UB_V2F					= 0x2A22
	C4UB_V3F					= 0x2A23
	C3F_V3F						= 0x2A24
	N3F_V3F						= 0x2A25
	C4F_N3F_V3F					= 0x2A26
	T2F_V3F						= 0x2A27
	T4F_V4F						= 0x2A28
	T2F_C4UB_V3F					= 0x2A29
	T2F_C3F_V3F					= 0x2A2A
	T2F_N3F_V3F					= 0x2A2B
	T2F_C4F_N3F_V3F					= 0x2A2C
	T4F_C4F_N3F_V4F					= 0x2A2D

LightModelColorControl enum:
	SINGLE_COLOR					= 0x81F9
	SEPARATE_SPECULAR_COLOR				= 0x81FA

LightModelParameter enum:
	LIGHT_MODEL_AMBIENT				= 0x0B53
	LIGHT_MODEL_LOCAL_VIEWER			= 0x0B51
	LIGHT_MODEL_TWO_SIDE				= 0x0B52
	LIGHT_MODEL_COLOR_CONTROL			= 0x81F8

# 0x4000-0x4FFF are reserved for light numbers
LightName enum:
	LIGHT0						= 0x4000
	LIGHT1						= 0x4001
	LIGHT2						= 0x4002
	LIGHT3						= 0x4003
	LIGHT4						= 0x4004
	LIGHT5						= 0x4005
	LIGHT6						= 0x4006
	LIGHT7						= 0x4007

LightParameter enum:
	AMBIENT						= 0x1200
	DIFFUSE						= 0x1201
	SPECULAR					= 0x1202
	POSITION					= 0x1203
	SPOT_DIRECTION					= 0x1204
	SPOT_EXPONENT					= 0x1205
	SPOT_CUTOFF					= 0x1206
	CONSTANT_ATTENUATION				= 0x1207
	LINEAR_ATTENUATION				= 0x1208
	QUADRATIC_ATTENUATION				= 0x1209

ListMode enum:
	COMPILE						= 0x1300
	COMPILE_AND_EXECUTE				= 0x1301

# USE_PREFIX ListName
ListNameType enum:
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
LogicOp enum:
	CLEAR						= 0x1500
	AND						= 0x1501
	AND_REVERSE					= 0x1502
	COPY						= 0x1503
	AND_INVERTED					= 0x1504
	NOOP						= 0x1505
	XOR						= 0x1506
	OR						= 0x1507
	NOR						= 0x1508
	EQUIV						= 0x1509
	INVERT						= 0x150A
	OR_REVERSE					= 0x150B
	COPY_INVERTED					= 0x150C
	OR_INVERTED					= 0x150D
	NAND						= 0x150E
	SET						= 0x150F

MapTarget enum:
	MAP1_COLOR_4					= 0x0D90
	MAP1_INDEX					= 0x0D91
	MAP1_NORMAL					= 0x0D92
	MAP1_TEXTURE_COORD_1				= 0x0D93
	MAP1_TEXTURE_COORD_2				= 0x0D94
	MAP1_TEXTURE_COORD_3				= 0x0D95
	MAP1_TEXTURE_COORD_4				= 0x0D96
	MAP1_VERTEX_3					= 0x0D97
	MAP1_VERTEX_4					= 0x0D98

	MAP2_COLOR_4					= 0x0DB0
	MAP2_INDEX					= 0x0DB1
	MAP2_NORMAL					= 0x0DB2
	MAP2_TEXTURE_COORD_1				= 0x0DB3
	MAP2_TEXTURE_COORD_2				= 0x0DB4
	MAP2_TEXTURE_COORD_3				= 0x0DB5
	MAP2_TEXTURE_COORD_4				= 0x0DB6
	MAP2_VERTEX_3					= 0x0DB7
	MAP2_VERTEX_4					= 0x0DB8

# USE_PREFIX Material
MaterialParameter enum:
	EMISSION					= 0x1600
	SHININESS					= 0x1601
	AMBIENT_AND_DIFFUSE				= 0x1602
	COLOR_INDEXES					= 0x1603
	use LightParameter AMBIENT
	use LightParameter DIFFUSE
	use LightParameter SPECULAR

MatrixMode enum:
	MODELVIEW					= 0x1700
	PROJECTION					= 0x1701
	TEXTURE						= 0x1702
	MODELVIEW0					= 0x1700	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW1					= 0x850A	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW2					= 0x8722	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW3					= 0x8723	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW4					= 0x8724	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW5					= 0x8725	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW6					= 0x8726	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW7					= 0x8727	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW8					= 0x8728	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW9					= 0x8729	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW10					= 0x872A	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW11					= 0x872B	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW12					= 0x872C	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW13					= 0x872D	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW14					= 0x872E	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW15					= 0x872F	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW16					= 0x8730	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW17					= 0x8731	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW18					= 0x8732	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW19					= 0x8733	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW20					= 0x8734	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW21					= 0x8735	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW22					= 0x8736	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW23					= 0x8737	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW24					= 0x8738	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW25					= 0x8739	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW26					= 0x873A	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW27					= 0x873B	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW28					= 0x873C	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW29					= 0x873D	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW30					= 0x873E	# ARB Extension #15: ARB_vertex_blend
	MODELVIEW31					= 0x873F	# ARB Extension #15: ARB_vertex_blend
	MATRIX_PALETTE					= 0x8840	# ARB Extension #16: ARB_matrix_palette

# USE_PREFIX Mesh1
MeshMode1 enum:
	use PolygonMode POINT
	use PolygonMode LINE

# USE_PREFIX Mesh2
MeshMode2 enum:
	use PolygonMode POINT
	use PolygonMode LINE
	use PolygonMode FILL

MinmaxTarget enum:
	MINMAX						= 0x802E

# USE_PREFIX Normal
NormalPointerType enum:
	use DataType BYTE
	use DataType SHORT
	use DataType INT
	use DataType FLOAT
	use DataType DOUBLE

PixelCopyType enum:
	COLOR						= 0x1800
	DEPTH						= 0x1801
	STENCIL						= 0x1802

PixelFormat enum:
	COLOR_INDEX					= 0x1900
	STENCIL_INDEX					= 0x1901
	DEPTH_COMPONENT					= 0x1902
	RED						= 0x1903
	GREEN						= 0x1904
	BLUE						= 0x1905
	ALPHA						= 0x1906
	RGB						= 0x1907
	RGBA						= 0x1908
	LUMINANCE					= 0x1909
	LUMINANCE_ALPHA					= 0x190A

PixelInternalFormat enum:
	ALPHA4						= 0x803B
	ALPHA8						= 0x803C
	ALPHA12						= 0x803D
	ALPHA16						= 0x803E
	DEPTH_COMPONENT16				= 0x81A5	# ARB Extension #22: ARB_depth_texture (promoted to core in 1.4)
	DEPTH_COMPONENT24				= 0x81A6	# ARB Extension #22: ARB_depth_texture (promoted to core in 1.4)
	DEPTH_COMPONENT32				= 0x81A7	# ARB Extension #22: ARB_depth_texture (promoted to core in 1.4)
	LUMINANCE4					= 0x803F
	LUMINANCE8					= 0x8040
	LUMINANCE12					= 0x8041
	LUMINANCE16					= 0x8042
	LUMINANCE4_ALPHA4				= 0x8043
	LUMINANCE6_ALPHA2				= 0x8044
	LUMINANCE8_ALPHA8				= 0x8045
	LUMINANCE12_ALPHA4				= 0x8046
	LUMINANCE12_ALPHA12				= 0x8047
	LUMINANCE16_ALPHA16				= 0x8048
	INTENSITY					= 0x8049
	INTENSITY4					= 0x804A
	INTENSITY8					= 0x804B
	INTENSITY12					= 0x804C
	INTENSITY16					= 0x804D
	R3_G3_B2					= 0x2A10
	RGB4						= 0x804F
	RGB5						= 0x8050
	RGB8						= 0x8051
	RGB10						= 0x8052
	RGB12						= 0x8053
	RGB16						= 0x8054
	RGBA2						= 0x8055
	RGBA4						= 0x8056
	RGB5_A1						= 0x8057
	RGBA8						= 0x8058
	RGB10_A2					= 0x8059
	RGBA12						= 0x805A
	RGBA16						= 0x805B
	COMPRESSED_ALPHA				= 0x84E9	# ARB Extension #12: ARB_texture_compression
	COMPRESSED_LUMINANCE				= 0x84EA	# ARB Extension #12: ARB_texture_compression
	COMPRESSED_LUMINANCE_ALPHA			= 0x84EB	# ARB Extension #12: ARB_texture_compression
	COMPRESSED_INTENSITY				= 0x84EC	# ARB Extension #12: ARB_texture_compression
	COMPRESSED_RGB					= 0x84ED	# ARB Extension #12: ARB_texture_compression
	COMPRESSED_RGBA					= 0x84EE	# ARB Extension #12: ARB_texture_compression

PixelMap enum:
	PIXEL_MAP_I_TO_I				= 0x0C70
	PIXEL_MAP_S_TO_S				= 0x0C71
	PIXEL_MAP_I_TO_R				= 0x0C72
	PIXEL_MAP_I_TO_G				= 0x0C73
	PIXEL_MAP_I_TO_B				= 0x0C74
	PIXEL_MAP_I_TO_A				= 0x0C75
	PIXEL_MAP_R_TO_R				= 0x0C76
	PIXEL_MAP_G_TO_G				= 0x0C77
	PIXEL_MAP_B_TO_B				= 0x0C78
	PIXEL_MAP_A_TO_A				= 0x0C79

PixelStore enum:
	UNPACK_SWAP_BYTES				= 0x0CF0
	UNPACK_LSB_FIRST				= 0x0CF1
	UNPACK_ROW_LENGTH				= 0x0CF2
	UNPACK_SKIP_ROWS				= 0x0CF3
	UNPACK_SKIP_PIXELS				= 0x0CF4
	UNPACK_ALIGNMENT				= 0x0CF5

	PACK_SWAP_BYTES					= 0x0D00
	PACK_LSB_FIRST					= 0x0D01
	PACK_ROW_LENGTH					= 0x0D02
	PACK_SKIP_ROWS					= 0x0D03
	PACK_SKIP_PIXELS				= 0x0D04
	PACK_ALIGNMENT					= 0x0D05

	PACK_SKIP_IMAGES				= 0x806B
	PACK_IMAGE_HEIGHT				= 0x806C
	UNPACK_SKIP_IMAGES				= 0x806D
	UNPACK_IMAGE_HEIGHT				= 0x806E

PixelTransfer enum:
	MAP_COLOR					= 0x0D10
	MAP_STENCIL					= 0x0D11
	INDEX_SHIFT					= 0x0D12
	INDEX_OFFSET					= 0x0D13
	RED_SCALE					= 0x0D14
	RED_BIAS					= 0x0D15
	GREEN_SCALE					= 0x0D18
	GREEN_BIAS					= 0x0D19
	BLUE_SCALE					= 0x0D1A
	BLUE_BIAS					= 0x0D1B
	ALPHA_SCALE					= 0x0D1C
	ALPHA_BIAS					= 0x0D1D
	DEPTH_SCALE					= 0x0D1E
	DEPTH_BIAS					= 0x0D1F

	POST_CONVOLUTION_RED_SCALE			= 0x801C
	POST_CONVOLUTION_GREEN_SCALE			= 0x801D
	POST_CONVOLUTION_BLUE_SCALE			= 0x801E
	POST_CONVOLUTION_ALPHA_SCALE			= 0x801F
	POST_CONVOLUTION_RED_BIAS			= 0x8020
	POST_CONVOLUTION_GREEN_BIAS			= 0x8021
	POST_CONVOLUTION_BLUE_BIAS			= 0x8022
	POST_CONVOLUTION_ALPHA_BIAS			= 0x8023

	POST_COLOR_MATRIX_RED_SCALE			= 0x80B4
	POST_COLOR_MATRIX_GREEN_SCALE			= 0x80B5
	POST_COLOR_MATRIX_BLUE_SCALE			= 0x80B6
	POST_COLOR_MATRIX_ALPHA_SCALE			= 0x80B7
	POST_COLOR_MATRIX_RED_BIAS			= 0x80B8
	POST_COLOR_MATRIX_GREEN_BIAS			= 0x80B9
	POST_COLOR_MATRIX_BLUE_BIAS			= 0x80BA
	POST_COLOR_MATRIX_ALPHA_BIAS			= 0x80BB

# USE_PREFIX Pixel
PixelType enum:
	BITMAP						= 0x1A00
	use DataType BYTE
	use DataType UNSIGNED_BYTE
	use DataType SHORT
	use DataType UNSIGNED_SHORT
	use DataType INT
	use DataType UNSIGNED_INT
	use DataType FLOAT
	BGR						= 0x80E0
	BGRA						= 0x80E1
	UNSIGNED_BYTE_3_3_2				= 0x8032
	UNSIGNED_SHORT_4_4_4_4				= 0x8033
	UNSIGNED_SHORT_5_5_5_1				= 0x8034
	UNSIGNED_INT_8_8_8_8				= 0x8035
	UNSIGNED_INT_10_10_10_2				= 0x8036
	UNSIGNED_BYTE_2_3_3_REV				= 0x8362
	UNSIGNED_SHORT_5_6_5				= 0x8363
	UNSIGNED_SHORT_5_6_5_REV			= 0x8364
	UNSIGNED_SHORT_4_4_4_4_REV			= 0x8365
	UNSIGNED_SHORT_1_5_5_5_REV			= 0x8366
	UNSIGNED_INT_8_8_8_8_REV			= 0x8367
	UNSIGNED_INT_2_10_10_10_REV			= 0x8368

PointParameterName enum:
	POINT_SIZE_MIN					= 0x8126	# ARB Extension #14: ARB_point_parameters (promoted to core in 1.4)
	POINT_SIZE_MAX					= 0x8127	# ARB Extension #14: ARB_point_parameters (promoted to core in 1.4)
	POINT_FADE_THRESHOLD_SIZE			= 0x8128	# ARB Extension #14: ARB_point_parameters (promoted to core in 1.4)
	POINT_DISTANCE_ATTENUATION			= 0x8129	# ARB Extension #14: ARB_point_parameters (promoted to core in 1.4)

PolygonMode enum:
	POINT						= 0x1B00
	LINE						= 0x1B01
	FILL						= 0x1B02

# USE_PREFIX ReadBuffer
ReadBufferMode enum:
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

RenderingMode enum:
	RENDER						= 0x1C00
	FEEDBACK					= 0x1C01
	SELECT						= 0x1C02

SeparableTarget enum:
	SEPARABLE_2D					= 0x8012

ShadingModel enum:
	FLAT						= 0x1D00
	SMOOTH						= 0x1D01

# USE_PREFIX Op
StencilOp enum:
	use BlendingFactorDest ZERO
	KEEP						= 0x1E00
	use TextureEnvMode REPLACE
	INCR						= 0x1E02
	INCR_WRAP					= 0x8507
	DECR						= 0x1E03
	DECR_WRAP					= 0x8508
	use LogicOp INVERT

StringName enum:
	VENDOR						= 0x1F00
	RENDERER					= 0x1F01
	VERSION						= 0x1F02
	EXTENSIONS					= 0x1F03

TextureCompareMode enum:
	COMPARE_R_TO_TEXTURE				= 0x884E	# ARB Extension #23: ARB_shadow (promoted to core in 1.4)

# USE_PREFIX TexCoord
TexCoordPointerType enum:
	use DataType SHORT
	use DataType INT
	use DataType FLOAT
	use DataType DOUBLE

TextureCoordName enum:
	S						= 0x2000
	T						= 0x2001
	R						= 0x2002
	Q						= 0x2003

# USE_PREFIX Mode
TextureEnvMode enum:
	MODULATE					= 0x2100
	DECAL						= 0x2101
	BLEND						= 0x0BE2
	REPLACE						= 0x1E01
	use AccumOp ADD							# ARB Extension #6: ARB_texture_env_add
	COMBINE						= 0x8570	# ARB Extension #17: ARB_texture_env_combine

# USE_PREFIX Combine
TextureEnvCombine enum:
	use TextureEnvMode REPLACE					# ARB Extension #17: ARB_texture_env_combine
	use TextureEnvMode MODULATE					# ARB Extension #17: ARB_texture_env_combine
	use AccumOp ADD							# ARB Extension #17: ARB_texture_env_combine
	ADD_SIGNED					= 0x8574	# ARB Extension #17: ARB_texture_env_combine
	INTERPOLATE					= 0x8575	# ARB Extension #17: ARB_texture_env_combine
	SUBTRACT					= 0x84E7	# ARB Extension #17: ARB_texture_env_combine
	DOT3_RGB					= 0x86AE	# ARB Extension #19: ARB_texture_env_dot3
	DOT3_RGBA					= 0x86AF	# ARB Extension #19: ARB_texture_env_dot3

TextureEnvOperand enum:
	use BlendingFactorDest SRC_COLOR				# ARB Extension #17: ARB_texture_env_combine
	use BlendingFactorDest ONE_MINUS_SRC_COLOR			# ARB Extension #17: ARB_texture_env_combine
	use BlendingFactorDest SRC_ALPHA				# ARB Extension #17: ARB_texture_env_combine
	use BlendingFactorDest ONE_MINUS_SRC_ALPHA			# ARB Extension #17: ARB_texture_env_combine

# USE_PREFIX TexEnvParam
TextureEnvParameter enum:
	TEXTURE_ENV_MODE				= 0x2200
	TEXTURE_ENV_COLOR				= 0x2201
	COMBINE_RGB					= 0x8571	# ARB Extension #17: ARB_texture_env_combine
	COMBINE_ALPHA					= 0x8572	# ARB Extension #17: ARB_texture_env_combine
	SRC0_RGB					= 0x8580	# ARB Extension #17: ARB_texture_env_combine
	SRC1_RGB					= 0x8581	# ARB Extension #17: ARB_texture_env_combine
	SRC2_RGB					= 0x8582	# ARB Extension #17: ARB_texture_env_combine
	SRC0_ALPHA					= 0x8588	# ARB Extension #17: ARB_texture_env_combine
	SRC1_ALPHA					= 0x8589	# ARB Extension #17: ARB_texture_env_combine
	SRC2_ALPHA					= 0x858A	# ARB Extension #17: ARB_texture_env_combine
	OPERAND0_RGB					= 0x8590	# ARB Extension #17: ARB_texture_env_combine
	OPERAND1_RGB					= 0x8591	# ARB Extension #17: ARB_texture_env_combine
	OPERAND2_RGB					= 0x8592	# ARB Extension #17: ARB_texture_env_combine
	OPERAND0_ALPHA					= 0x8598	# ARB Extension #17: ARB_texture_env_combine
	OPERAND1_ALPHA					= 0x8599	# ARB Extension #17: ARB_texture_env_combine
	OPERAND2_ALPHA					= 0x859A	# ARB Extension #17: ARB_texture_env_combine
	RGB_SCALE					= 0x8573
	use PixelTransfer ALPHA_SCALE					# ARB Extension #17: ARB_texture_env_combine

# USE_PREFIX Src
TextureEnvSrc enum:
	use MatrixMode TEXTURE						# ARB Extension #17: ARB_texture_env_combine
	CONSTANT					= 0x8576	# ARB Extension #17: ARB_texture_env_combine
	PRIMARY_COLOR					= 0x8577	# ARB Extension #17: ARB_texture_env_combine
	PREVIOUS					= 0x8578	# ARB Extension #17: ARB_texture_env_combine
	use TextureUnit TEXTURE0					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE1					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE2					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE3					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE4					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE5					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE6					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE7					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE8					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE9					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE10					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE11					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE12					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE13					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE14					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE15					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE16					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE17					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE18					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE19					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE20					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE21					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE22					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE23					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE24					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE25					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE26					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE27					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE28					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE29					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE30					# ARB Extension #18: ARB_texture_env_crossbar
	use TextureUnit TEXTURE31					# ARB Extension #18: ARB_texture_env_crossbar

TextureEnvTarget enum:
	TEXTURE_ENV					= 0x2300
	TEXTURE_FILTER_CONTROL				= 0x8500

TextureFilterParameter enum:
	TEXTURE_LOD_BIAS				= 0x8501

TextureGenMode enum:
	EYE_LINEAR					= 0x2400
	OBJECT_LINEAR					= 0x2401
	SPHERE_MAP					= 0x2402
	NORMAL_MAP					= 0x8511	# ARB Extension #7: ARB_texture_cube_map
	REFLECTION_MAP					= 0x8512	# ARB Extension #7: ARB_texture_cube_map

TextureGenParameter enum:
	TEXTURE_GEN_MODE				= 0x2500
	OBJECT_PLANE					= 0x2501
	EYE_PLANE					= 0x2502

# USE_PREFIX Mag
TextureMagFilter enum:
	use TextureMinFilter NEAREST
	use TextureMinFilter LINEAR

# USE_PREFIX Min
TextureMinFilter enum:
	NEAREST						= 0x2600
	LINEAR						= 0x2601
	NEAREST_MIPMAP_NEAREST				= 0x2700
	LINEAR_MIPMAP_NEAREST				= 0x2701
	NEAREST_MIPMAP_LINEAR				= 0x2702
	LINEAR_MIPMAP_LINEAR				= 0x2703

TextureParameterName enum:
	TEXTURE_MAG_FILTER				= 0x2800
	TEXTURE_MIN_FILTER				= 0x2801

	TEXTURE_WRAP_R					= 0x8072
	TEXTURE_WRAP_S					= 0x2802
	TEXTURE_WRAP_T					= 0x2803

	TEXTURE_BORDER_COLOR				= 0x1004

	TEXTURE_PRIORITY				= 0x8066

	TEXTURE_MIN_LOD					= 0x813A
	TEXTURE_MAX_LOD					= 0x813B
	TEXTURE_BASE_LEVEL				= 0x813C
	TEXTURE_MAX_LEVEL				= 0x813D

	GENERATE_MIPMAP					= 0x8191

	TEXTURE_MAX_ANISOTROPY				= 0x84FE

	DEPTH_TEXTURE_MODE				= 0x884B	# ARB Extension #22: ARB_depth_texture (promoted to core in 1.4)

	TEXTURE_COMPARE_MODE				= 0x884C	# ARB Extension #23: ARB_shadow (promoted to core in 1.4)
	TEXTURE_COMPARE_FUNC				= 0x884D	# ARB Extension #23: ARB_shadow (promoted to core in 1.4)

	TEXTURE_COMPARE_FAIL_VALUE			= 0x80BF	# ARB Extension #24: ARB_shadow_ambient

TextureTarget enum:
	TEXTURE_1D					= 0x0DE0
	TEXTURE_2D					= 0x0DE1
	TEXTURE_3D					= 0x806F
	PROXY_TEXTURE_1D				= 0x8063
	PROXY_TEXTURE_2D				= 0x8064
	PROXY_TEXTURE_3D				= 0x8070
	TEXTURE_CUBE_MAP				= 0x8513	# ARB Extension #7: ARB_texture_cube_map
	PROXY_TEXTURE_CUBE_MAP				= 0x851B	# ARB Extension #7: ARB_texture_cube_map
	TEXTURE_CUBE_MAP_POSITIVE_X			= 0x8515	# ARB Extension #7: ARB_texture_cube_map
	TEXTURE_CUBE_MAP_NEGATIVE_X			= 0x8516	# ARB Extension #7: ARB_texture_cube_map
	TEXTURE_CUBE_MAP_POSITIVE_Y			= 0x8517	# ARB Extension #7: ARB_texture_cube_map
	TEXTURE_CUBE_MAP_NEGATIVE_Y			= 0x8518	# ARB Extension #7: ARB_texture_cube_map
	TEXTURE_CUBE_MAP_POSITIVE_Z			= 0x8519	# ARB Extension #7: ARB_texture_cube_map
	TEXTURE_CUBE_MAP_NEGATIVE_Z			= 0x851A	# ARB Extension #7: ARB_texture_cube_map

TextureUnit enum:
	TEXTURE0					= 0x84C0
	TEXTURE1					= 0x84C1
	TEXTURE2					= 0x84C2
	TEXTURE3					= 0x84C3
	TEXTURE4					= 0x84C4
	TEXTURE5					= 0x84C5
	TEXTURE6					= 0x84C6
	TEXTURE7					= 0x84C7
	TEXTURE8					= 0x84C8
	TEXTURE9					= 0x84C9
	TEXTURE10					= 0x84CA
	TEXTURE11					= 0x84CB
	TEXTURE12					= 0x84CC
	TEXTURE13					= 0x84CD
	TEXTURE14					= 0x84CE
	TEXTURE15					= 0x84CF
	TEXTURE16					= 0x84D0
	TEXTURE17					= 0x84D1
	TEXTURE18					= 0x84D2
	TEXTURE19					= 0x84D3
	TEXTURE20					= 0x84D4
	TEXTURE21					= 0x84D5
	TEXTURE22					= 0x84D6
	TEXTURE23					= 0x84D7
	TEXTURE24					= 0x84D8
	TEXTURE25					= 0x84D9
	TEXTURE26					= 0x84DA
	TEXTURE27					= 0x84DB
	TEXTURE28					= 0x84DC
	TEXTURE29					= 0x84DD
	TEXTURE30					= 0x84DE
	TEXTURE31					= 0x84DF

TextureWrapMode enum:
	CLAMP						= 0x2900
	REPEAT						= 0x2901
	CLAMP_TO_EDGE					= 0x812F
	CLAMP_TO_BORDER					= 0x812D	# ARB Extension #13: ARB_texture_border_clamp
	MIRRORED_REPEAT					= 0x8370	# ARB Extension #21: ARB_texture_mirrored_repeat (promoted to core in 1.4)

# USE_PREFIX Vertex
VertexPointerType enum:
	use DataType SHORT
	use DataType INT
	use DataType FLOAT
	use DataType DOUBLE
