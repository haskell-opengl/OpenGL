TypeMask mask:
	MASK_A
	MASK_B
	MASK_C
	MASK_D
	MASK_E
	use TypeEnum ENUM_C
	use TypeEnum LEFT
	RIGHT

TypeEnum enum:
	ENUM_A
	ENUM_B
	ENUM_C = 42
	ENUM_D
	use TypeDefine DEFINE_C
	ENUM_F
	use TypeMask MASK_D
	ENUM_H
	ENUM_I = $MASK_E
	ENUM_J = $ENUM_F + 42
	LEFT
	use TypeMask RIGHT

TypeDefine define:
	DEFINE_A
	DEFINE_B
	DEFINE_C = 128
	DEFINE_D
	DEFINE_E
	use TypeEnum ENUM_C
	use TypeEnum ENUM_H
