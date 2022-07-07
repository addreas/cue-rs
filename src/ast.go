// Copyright 2018 The CUE Authors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// Package ast declares the types used to represent syntax trees for CUE
// packages.
package ast // import "cuelang.org/go/cue/ast"

import (
	"fmt"
)

// ----------------------------------------------------------------------------
// Interfaces
//
// There are three main classes of nodes: expressions, clauses, and declaration
// nodes. The node names usually match the corresponding CUE spec production
// names to which they correspond. The node fields correspond to the individual
// parts of the respective productions.
//
// All nodes contain position information marking the beginning of the
// corresponding source text segment; it is accessible via the Pos accessor
// method. Nodes may contain additional position info for language constructs
// where comments may be found between parts of the construct (typically any
// larger, parenthesized subpart). That position information is needed to
// properly position comments when printing the construct.

// A Node represents any node in the abstract syntax tree.
type Node interface {
	Pos() token.Pos // position of first character belonging to the node
	End() token.Pos // position of first character immediately after the node

	// pos reports the pointer to the position of first character belonging to
	// the node or nil if there is no such position.
	pos() *token.Pos

	// Deprecated: use ast.Comments
	Comments() []*CommentGroup

	// Deprecated: use ast.AddComment
	AddComment(*CommentGroup)
	commentInfo() *comments
}

// An Expr is implemented by all expression nodes.
type Expr interface {
	Node
	declNode() // An expression can be used as a declaration.
	exprNode()
}

type expr struct{ decl }

func (expr) exprNode() {}

// A Decl node is implemented by all declarations.
type Decl interface {
	Node
	declNode()
}

type decl struct{}

func (decl) declNode() {}

// A Label is any production that can be used as a LHS label.
type Label interface {
	Node
	labelNode()
}

type label struct{}

func (l label) labelNode() {}

// Clause nodes are part of comprehensions.
type Clause interface {
	Node
	clauseNode()
}

type clause struct{}

func (clause) clauseNode() {}

func (x *ForClause) clauseNode() {}
func (x *IfClause) clauseNode()  {}
func (x *Alias) clauseNode()     {}

// Comments

type comments struct {
	groups *[]*CommentGroup
}

// A Comment node represents a single //-style or /*-style comment.
type Comment struct {
	Slash token.Pos // position of "/" starting the comment
	Text  string    // comment text (excluding '\n' for //-style comments)
}

func (c *Comment) Comments() []*CommentGroup { return nil }
func (c *Comment) AddComment(*CommentGroup)  {}
func (c *Comment) commentInfo() *comments    { return nil }

// A CommentGroup represents a sequence of comments
// with no other tokens and no empty lines between.
type CommentGroup struct {
	// TODO: remove and use the token position of the first comment.
	Doc  bool
	Line bool // true if it is on the same line as the node's end pos.

	// Position indicates where a comment should be attached if a node has
	// multiple tokens. 0 means before the first token, 1 means before the
	// second, etc. For instance, for a field, the positions are:
	//    <0> Label <1> ":" <2> Expr <3> "," <4>
	Position int8
	List     []*Comment // len(List) > 0

	decl
}

func (g *CommentGroup) Comments() []*CommentGroup { return nil }
func (g *CommentGroup) AddComment(*CommentGroup)  {}
func (g *CommentGroup) commentInfo() *comments    { return nil }

// An Attribute provides meta data about a field.
type Attribute struct {
	At   token.Pos
	Text string // must be a valid attribute format.

	comments
	decl
}

// A Field represents a field declaration in a struct.
type Field struct {
	Label    Label // must have at least one element.
	Optional token.Pos

	// No TokenPos: Value must be an StructLit with one field.
	TokenPos token.Pos
	Token    token.Token // ':' or '::', ILLEGAL implies ':'

	Value Expr // the value associated with this field.

	Attrs []*Attribute

	comments
	decl
}

// TODO: make Alias a type of Field. This is possible now we have different
// separator types.

// An Alias binds another field to the alias name in the current struct.
type Alias struct {
	Ident *Ident    // field name, always an Ident
	Equal token.Pos // position of "="
	Expr  Expr      // An Ident or SelectorExpr

	comments
	decl
	expr
	label
}

// A Comprehension node represents a comprehension declaration.
type Comprehension struct {
	Clauses []Clause // There must be at least one clause.
	Value   Expr     // Must be a struct TODO: change to Struct

	comments
	decl
	expr // TODO: only allow Comprehension in "Embedding" productions.
}

// ----------------------------------------------------------------------------
// Expressions and types
//
// An expression is represented by a tree consisting of one
// or more of the following concrete expression nodes.

// A BadExpr node is a placeholder for expressions containing
// syntax errors for which no correct expression nodes can be
// created. This is different from an ErrorExpr which represents
// an explicitly marked error in the source.
type BadExpr struct {
	From, To token.Pos // position range of bad expression

	comments
	expr
}

// A BottomLit indicates an error.
type BottomLit struct {
	Bottom token.Pos

	comments
	expr
}

// An Ident node represents an left-hand side identifier.
type Ident struct {
	NamePos token.Pos // identifier position

	// This LHS path element may be an identifier. Possible forms:
	//  foo:    a normal identifier
	//  "foo":  JSON compatible
	Name string

	Scope Node // scope in which node was found or nil if referring directly
	Node  Node

	comments
	label
	expr
}

// A BasicLit node represents a literal of basic type.
type BasicLit struct {
	ValuePos token.Pos   // literal position
	Kind     token.Token // INT, FLOAT, DURATION, or STRING
	Value    string      // literal string; e.g. 42, 0x7f, 3.14, 1_234_567, 1e-9, 2.4i, 'a', '\x7f', "foo", or '\m\n\o'

	comments
	expr
	label
}

// TODO: introduce and use NewLabel and NewBytes and perhaps NewText (in the
// later case NewString would return a string or bytes type) to distinguish from
// NewString. Consider how to pass indentation information.

// NewString creates a new BasicLit with a string value without position.
// It quotes the given string.
// Useful for ASTs generated by code other than the CUE parser.
func NewString(str string) *BasicLit {
	str = literal.String.Quote(str)
	return &BasicLit{Kind: token.STRING, ValuePos: token.NoPos, Value: str}
}

// NewNull creates a new BasicLit configured to be a null value.
// Useful for ASTs generated by code other than the CUE parser.
func NewNull() *BasicLit {
	return &BasicLit{Kind: token.NULL, Value: "null"}
}

// NewLit creates a new BasicLit with from a token type and string without
// position.
// Useful for ASTs generated by code other than the CUE parser.
func NewLit(tok token.Token, s string) *BasicLit {
	return &BasicLit{Kind: tok, Value: s}
}

// NewBool creates a new BasicLit with a bool value without position.
// Useful for ASTs generated by code other than the CUE parser.
func NewBool(b bool) *BasicLit {
	x := &BasicLit{}
	if b {
		x.Kind = token.TRUE
		x.Value = "true"
	} else {
		x.Kind = token.FALSE
		x.Value = "false"
	}
	return x
}

// TODO:
// - use CUE-specific quoting (hoist functionality in export)
// - NewBytes

// A Interpolation node represents a string or bytes interpolation.
type Interpolation struct {
	Elts []Expr // interleaving of strings and expressions.

	comments
	expr
	label
}

// A StructLit node represents a literal struct.
type StructLit struct {
	Lbrace token.Pos // position of "{"
	Elts   []Decl    // list of elements; or nil
	Rbrace token.Pos // position of "}"

	comments
	expr
}

// NewStruct creates a struct from the given fields.
//
// A field is either a *Field, an *Elipsis, *LetClause, a *CommentGroup, or a
// Label, optionally followed by a a token.OPTION to indicate the field is
// optional, optionally followed by a token.ISA to indicate the field is a
// definition followed by an expression for the field value.
//
// It will panic if a values not matching these patterns are given. Useful for
// ASTs generated by code other than the CUE parser.
func NewStruct(fields ...interface{}) *StructLit {
	s := &StructLit{
		// Set default positions so that comment attachment is as expected.
		Lbrace: token.NoSpace.Pos(),
	}
	for i := 0; i < len(fields); i++ {
		var (
			label    Label
			optional = token.NoPos
			tok      = token.ILLEGAL
			expr     Expr
		)

		switch x := fields[i].(type) {
		case *Field:
			s.Elts = append(s.Elts, x)
			continue
		case *CommentGroup:
			s.Elts = append(s.Elts, x)
			continue
		case *Ellipsis:
			s.Elts = append(s.Elts, x)
			continue
		case *LetClause:
			s.Elts = append(s.Elts, x)
			continue
		case *embedding:
			s.Elts = append(s.Elts, (*EmbedDecl)(x))
			continue
		case Label:
			label = x
		case string:
			label = NewString(x)
		default:
			panic(fmt.Sprintf("unsupported label type %T", x))
		}

	inner:
		for i++; i < len(fields); i++ {
			switch x := (fields[i]).(type) {
			case Expr:
				expr = x
				break inner
			case token.Token:
				switch x {
				case token.ISA:
					tok = x
				case token.OPTION:
					optional = token.Blank.Pos()
				case token.COLON, token.ILLEGAL:
				default:
					panic(fmt.Sprintf("invalid token %s", x))
				}
			default:
				panic(fmt.Sprintf("unsupported expression type %T", x))
			}
		}
		if expr == nil {
			panic("label not matched with expression")
		}
		s.Elts = append(s.Elts, &Field{
			Label:    label,
			Optional: optional,
			Token:    tok,
			Value:    expr,
		})
	}
	return s
}

// Embed can be used in conjunction with NewStruct to embed values.
func Embed(x Expr) *embedding {
	return (*embedding)(&EmbedDecl{Expr: x})
}

type embedding EmbedDecl

// A ListLit node represents a literal list.
type ListLit struct {
	Lbrack token.Pos // position of "["

	// TODO: change to embedding or similar.
	Elts   []Expr    // list of composite elements; or nil
	Rbrack token.Pos // position of "]"

	comments
	expr
	label
}

// NewList creates a list of Expressions.
// Useful for ASTs generated by code other than the CUE parser.
func NewList(exprs ...Expr) *ListLit {
	return &ListLit{Elts: exprs}
}

type Ellipsis struct {
	Ellipsis token.Pos // open list if set
	Type     Expr      // type for the remaining elements

	comments
	decl
	expr
}

// A ForClause node represents a for clause in a comprehension.
type ForClause struct {
	For token.Pos
	Key *Ident // allow pattern matching?
	// TODO: change to Comma
	Colon  token.Pos
	Value  *Ident // allow pattern matching?
	In     token.Pos
	Source Expr

	comments
	clause
}

// A IfClause node represents an if guard clause in a comprehension.
type IfClause struct {
	If        token.Pos
	Condition Expr

	comments
	clause
}

// A LetClause node represents a let clause in a comprehension.
type LetClause struct {
	Let   token.Pos
	Ident *Ident
	Equal token.Pos
	Expr  Expr

	comments
	clause
	decl
}

// A ParenExpr node represents a parenthesized expression.
type ParenExpr struct {
	Lparen token.Pos // position of "("
	X      Expr      // parenthesized expression
	Rparen token.Pos // position of ")"

	comments
	expr
	label
}

// A SelectorExpr node represents an expression followed by a selector.
type SelectorExpr struct {
	X   Expr  // expression
	Sel Label // field selector

	comments
	expr
}

// NewSel creates a sequence of selectors.
// Useful for ASTs generated by code other than the CUE parser.
func NewSel(x Expr, sel ...string) Expr {
	for _, s := range sel {
		x = &SelectorExpr{X: x, Sel: NewIdent(s)}
	}
	return x
}

// An IndexExpr node represents an expression followed by an index.
type IndexExpr struct {
	X      Expr      // expression
	Lbrack token.Pos // position of "["
	Index  Expr      // index expression
	Rbrack token.Pos // position of "]"

	comments
	expr
}

// An SliceExpr node represents an expression followed by slice indices.
type SliceExpr struct {
	X      Expr      // expression
	Lbrack token.Pos // position of "["
	Low    Expr      // begin of slice range; or nil
	High   Expr      // end of slice range; or nil
	Rbrack token.Pos // position of "]"

	comments
	expr
}

// A CallExpr node represents an expression followed by an argument list.
type CallExpr struct {
	Fun    Expr      // function expression
	Lparen token.Pos // position of "("
	Args   []Expr    // function arguments; or nil
	Rparen token.Pos // position of ")"

	comments
	expr
}

// NewCall creates a new CallExpr.
// Useful for ASTs generated by code other than the CUE parser.
func NewCall(fun Expr, args ...Expr) *CallExpr {
	return &CallExpr{Fun: fun, Args: args}
}

// A UnaryExpr node represents a unary expression.
type UnaryExpr struct {
	OpPos token.Pos   // position of Op
	Op    token.Token // operator
	X     Expr        // operand

	comments
	expr
}

// A BinaryExpr node represents a binary expression.
type BinaryExpr struct {
	X     Expr        // left operand
	OpPos token.Pos   // position of Op
	Op    token.Token // operator
	Y     Expr        // right operand

	comments
	expr
}

// NewBinExpr creates for list of expressions of length 2 or greater a chained
// binary expression of the form (((x1 op x2) op x3) ...). For lists of length
// 1 it returns the expression itself. It panics for empty lists.
// Useful for ASTs generated by code other than the CUE parser.
func NewBinExpr(op token.Token, operands ...Expr) Expr {
	if len(operands) == 0 {
		return nil
	}
	expr := operands[0]
	for _, e := range operands[1:] {
		expr = &BinaryExpr{X: expr, Op: op, Y: e}
	}
	return expr
}

// ----------------------------------------------------------------------------
// Convenience functions for Idents

// NewIdent creates a new Ident without position.
// Useful for ASTs generated by code other than the CUE parser.
func NewIdent(name string) *Ident {
	return &Ident{token.NoPos, name, nil, nil, comments{}, label{}, expr{}}
}

func (id *Ident) String() string {
	if id != nil {
		return id.Name
	}
	return "<nil>"
}

// ----------------------------------------------------------------------------
// Declarations

// An ImportSpec node represents a single package import.
type ImportSpec struct {
	Name   *Ident    // local package name (including "."); or nil
	Path   *BasicLit // import path
	EndPos token.Pos // end of spec (overrides Path.Pos if nonzero)

	comments
}

func (*ImportSpec) specNode() {}

func NewImport(name *Ident, importPath string) *ImportSpec {
	importPath = literal.String.Quote(importPath)
	path := &BasicLit{Kind: token.STRING, Value: importPath}
	return &ImportSpec{Name: name, Path: path}
}

// A BadDecl node is a placeholder for declarations containing
// syntax errors for which no correct declaration nodes can be
// created.
type BadDecl struct {
	From, To token.Pos // position range of bad declaration

	comments
	decl
}

// A ImportDecl node represents a series of import declarations. A valid
// Lparen position (Lparen.Line > 0) indicates a parenthesized declaration.
type ImportDecl struct {
	Import token.Pos
	Lparen token.Pos // position of '(', if any
	Specs  []*ImportSpec
	Rparen token.Pos // position of ')', if any

	comments
	decl
}

type Spec interface {
	Node
	specNode()
}

// An EmbedDecl node represents a single expression used as a declaration.
// The expressions in this declaration is what will be emitted as
// configuration output.
//
// An EmbedDecl may only appear at the top level.
type EmbedDecl struct {
	Expr Expr

	comments
	decl
}

// Pos and End implementations for declaration nodes.

// ----------------------------------------------------------------------------
// Files and packages

// A File node represents a Go source file.
//
// The Comments list contains all comments in the source file in order of
// appearance, including the comments that are pointed to from other nodes
// via Doc and Comment fields.
type File struct {
	Filename string
	Decls    []Decl // top-level declarations; or nil

	Imports    []*ImportSpec // imports in this file
	Unresolved []*Ident      // unresolved identifiers in this file

	comments
}

// Preamble returns the declarations of the preamble.
func (f *File) Preamble() []Decl {
	p := 0
outer:
	for i, d := range f.Decls {
		switch d.(type) {
		default:
			break outer

		case *Package:
			p = i + 1
		case *CommentGroup:
		case *Attribute:
		case *ImportDecl:
			p = i + 1
		}
	}
	return f.Decls[:p]
}

func (f *File) VisitImports(fn func(d *ImportDecl)) {
	for _, d := range f.Decls {
		switch x := d.(type) {
		case *CommentGroup:
		case *Package:
		case *Attribute:
		case *ImportDecl:
			fn(x)
		default:
			return
		}
	}
}

// PackageName returns the package name associated with this file or "" if no
// package is associated.
func (f *File) PackageName() string {
	for _, d := range f.Decls {
		switch x := d.(type) {
		case *Package:
			return x.Name.Name
		case *CommentGroup, *Attribute:
		default:
			return ""
		}
	}
	return ""
}

// A Package represents a package clause.
type Package struct {
	PackagePos token.Pos // position of "package" pseudo-keyword
	Name       *Ident    // package name

	comments
	decl
}
