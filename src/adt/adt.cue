#Value: or([ for v in #ValueKinds {v}])

#ValueKinds: {
	[K=string]: valueKind: K

	Struct: #Struct
	List:   #List

	Disjunction: items: [...#Disjunctand]
	Conjunction: items: [...#Value]

	BasicType:  #BasicType
	BasicValue: #BasicValue

	Interpolation: items: [...#Value]

	FieldReference: #FieldReference
	LetReference:   #LetReference
}

#Struct: {
	valueKind?: "Struct"
	closed:     bool
	bindings: [...#Binding]
	fields: [...#Field]
}

#Binding: {

}

#List: {
	valueKind?: "List"
	closed:     bool
	fields: [...#Field]
}

#BasicValue: {
	or([ for k, v in #BasicValueKinds {v}])
	valueKind?: "BasicValue"
}
#BasicValueKinds: {
	[K=string]: basicValueKind: K
	String: value:              string
	Bytes: value:               bytes
	Number: value:              number
	Bool: value:                bool
	Null: _
}

#BasicType: {
	or([ for k, v in #BasicTypeKinds {v}])
	valueKind?: "BasicType"
}
#BasicTypeKinds: {
	[K=string]: basicTypeKind: K
	String: _
	Bytes:  _
	Number: _
	Bool:   _
	Null:   _
}

#Field: or([ for k, v in #FieldKinds {v}])
#FieldKinds: {
	[K=string]: {
		fieldKind: K
		label:     #Label
		value:     #Value
	}
	Field:    _
	Optional: _
	BulkOptional: filter: #Value
}

#Label: or([ for k, v in #LabelKinds {v}])
#LabelKinds: {
	[K=string]: {
		labelKind: K
		value:     string
	}
	String:           _
	Number:           _
	Definition:       _
	Hidden:           _
	HiddenDefinition: _
}

#Disjunctand: {
	default: *false | bool
	value:   #Value
}

#FieldReference: _
#LetReference:   _
