tag: *"latest" | string

let const = 42

#Def: {
	kind: "k1"
	some: "thing:\(tag)"
	inner: [name=string]: "Hello, \(name)"
} | {
	kind:  "k2"
	thing: "some:\(tag)"
	other: (#Def & {kind: "k1"}).some
} | {
	kind: "k3"
	val:  int & const
} | {
	kind: "k4"
	valu: [1, 2, 3]
}

value1: #Def & {
	kind: "k1"
	inner: World: _
}

value2: value2: {
	k2: #Def & {
		kind: "k2"
	}
}

/// --- ADT?

let _tag = #FieldKinds.Field & {
	fieldReferenceId: 0

	label: #LabelKinds.String & {value: "tag"}
	value: #Value.Disjunction & {items: [
		#Disjunctand & {
			default: true
			value:   #Value.BasicValue & #BasicValue.String & "latest"
		},
		#Disjunctand & {
			value: #Value.BasicType & #BasicType.String
		},
	]}
}

let _nameLabel = #LabelKinds.String & {
	labelReferenceId: 2
	value:            "name"
}

let _k1Inner = #ValueKinds.Struct & {
	closed: false
	fields: [
		#FieldKinds.BulkOptional & {
			label:  _nameLabel
			filter: #ValueKinds.BasicType & #BasicTypeKinds.String
			value:  #ValueKinds.Interpolation & {items: [
				#ValueKinds.BasicValue & #BasicValueKinds.String & {value: "Hello, "},
				#ValueKinds.LabelReference & {labelReferenceId:            _nameLabel.labelReferenceId},
			]}
		},
	]
}

let _defK1 = #ValueKinds.Struct & {
	closed: true
	fields: [
		#FieldKinds.Field & {
			label: #LabelKinds.String & {value: "kind"}
			value: #Value.BasicValue & #BasicValue.String & "k1"
		},
		#FieldKinds.Field & {
			label: #LabelKinds.String & {value: "some"}
			value: #Value.Interpolation & [
				#Value.BasicValue & #BasicValue.String & "thing:",
				#Value.FieldReference & {fieldReferenceId: _tag.fieldReferenceId},
			]
		},
		#FieldKinds.Field & {
			label: #LabelKinds.String & {value: "inner"}
			value: _k1Inner
		},
	]
}

let _defK2 = #ValueKinds.Struct & {
	closed: true
	fields: [
		#FieldKinds.Field & {
			label: #LabelKinds.String & {value:                               "kind"}
			value: #ValueKinds.BasicValue & #BasicValueKinds.String & {value: "k2"}
		},
		#FieldKinds.Field & {
			label: #LabelKinds.String & {value: "thing"}
			value: #Value.Interpolation & [
				#Value.BasicValue & #BasicValue.String & "some:",
				#Value.FieldReference & {fieldReferenceId: _tag.fieldReferenceId},
			]
		},
	]
}
let _defK3 = #ValueKinds.Struct & {
	closed: true
	fields: [
		#FieldKinds.Field & {
			label: #LabelKinds.String & {value:                               "kind"}
			value: #ValueKinds.BasicValue & #BasicValueKinds.String & {value: "k3"}
		},
		#FieldKinds.Field & {
			label: #LabelKinds.String & {value: "val"}
			value: #ValueKinds.Conjunction & {items: [
				#ValueKinds.BasicType & #BasicTypeKinds.Number,
				#ValueKinds.LetReference & {},
			]}
		},
	]
}
let _defK4 = #ValueKinds.Struct & {
	closed: true
	fields: [
		#FieldKinds.Field & {
			label: #LabelKinds.String & {value:                               "kind"}
			value: #ValueKinds.BasicValue & #BasicValueKinds.String & {value: "k4"}
		},
	]
}

#ValueKinds.Struct & {
	closed: false
	fields: [
		_tag,
		#Field & {
			label: #Label.Definition & "#Def"
			value: #Value.Disjunction & [
				_defK1,
				_defK2,
				_defK3,
				_defK4,
			]
		},
	]
}
