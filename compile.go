package ethers_abi

import (
	"fmt"
	"go/scanner"
	"go/token"

	"github.com/ethereum/go-ethereum/accounts/abi"
)

type compiler struct {
	file    *token.File
	errors  scanner.ErrorList
	scanner scanner.Scanner

	// Next token
	pos token.Pos   // token position
	tok token.Token // one token look-ahead
	lit string      // token literal

	typeArgs map[string]abi.ArgumentMarshaling
}

func (p *compiler) init(fset *token.FileSet, filename string, src []byte) *compiler {
	p.file = fset.AddFile(filename, -1, len(src))
	eh := func(pos token.Position, msg string) { p.errors.Add(pos, msg) }
	p.scanner.Init(p.file, src, eh, 2)
	p.next()
	return p
}

func (p *compiler) next() {
	p.pos, p.tok, p.lit = p.scanner.Scan()
}

func (p *compiler) error(pos token.Pos, msg string) {
	epos := p.file.Position(pos)

	n := len(p.errors)
	if n > 0 && p.errors[n-1].Pos.Line == epos.Line {
		return // discard - likely a spurious error
	}

	p.errors.Add(epos, msg)
}

func (p *compiler) errorExpected(pos token.Pos, msg string) {
	msg = "expected " + msg
	if pos == p.pos {
		// the error happened at the current position;
		// make the error message more specific
		switch {
		case p.tok == token.SEMICOLON && p.lit == "\n":
			msg += ", found newline"
		case p.tok.IsLiteral():
			// print 123 rather than 'INT', etc.
			msg += ", found " + p.lit
		default:
			msg += ", found '" + p.tok.String() + "'"
		}
	}
	p.error(pos, msg)
}

func (p *compiler) expect(tok token.Token) token.Pos {
	pos := p.pos
	if p.tok != tok {
		p.errorExpected(pos, "'"+tok.String()+"'")
	}
	p.next() // make progress
	return pos
}

func (p *compiler) accept(tok token.Token) bool {
	if p.tok != tok {
		return false
	}
	p.next()
	return true
}

func (p *compiler) acceptKeyword(s string) bool {
	if p.lit != s {
		return false
	}
	p.next()
	return true
}

func (p *compiler) expectKeyword(s string) token.Pos {
	pos := p.pos
	if p.tok != token.IDENT {
		p.errorExpected(pos, "'"+token.IDENT.String()+"'")
	}
	if p.lit != s {
		p.errorExpected(pos, "'"+s+"'")
	}
	p.next() // make progress
	return pos
}

func (p *compiler) expectSemiOrEof() token.Pos {
	pos := p.pos
	switch p.tok {
	case token.SEMICOLON, token.EOF:
	default:
		p.errorExpected(pos, "';' or 'EOF'")
	}
	p.next() // make progress
	return pos
}

func (p *compiler) parseIdent() string {
	name := "_"
	if p.tok == token.IDENT {
		name = p.lit
		p.next()
	} else {
		p.expect(token.IDENT) // use expect() error handling
	}
	return name
}

func (p *compiler) parseTuple() abi.ArgumentMarshaling {
	args := p.parseParameters("func")

	components := make([]abi.ArgumentMarshaling, len(args))

	for i, arg := range args {
		components[i] = abi.ArgumentMarshaling{
			Name:    arg.Name,
			Type:    arg.Type.String(),
			Indexed: arg.Indexed,
		}
	}

	return abi.ArgumentMarshaling{Type: "tuple", Components: components}
}

func (p *compiler) parseType() (typ abi.Type) {
	var typeArg abi.ArgumentMarshaling

	switch p.tok {
	case token.LPAREN:
		typeArg = p.parseTuple()
	case token.IDENT:
		typeName := p.parseIdent()

		if targ, ok := p.typeArgs[typeName]; ok {
			typeArg = targ

			break
		}

		if typeName == "tuple" {
			typeArg = p.parseTuple()

			break
		}

		switch typeName {
		case "int":
			typeName = "int256"
		case "uint":
			typeName = "uint256"
		case "address":
			p.acceptKeyword("payable")
		}

		typeArg.Type = typeName
	default:
		p.errorExpected(p.pos, "'(' or 'IDENT'")
	}

	for p.tok == token.LBRACK {
		typeArg.Type += p.tok.String()

		for p.tok != token.RBRACK {
			p.next()

			if p.tok == token.EOF || p.tok == token.LBRACK {
				p.errorExpected(p.pos, "]")

				return abi.Type{}
			}

			typeArg.Type += p.tok.String()
		}

		p.next()
	}

	t, err := abi.NewType(typeArg.Type, typeArg.InternalType, typeArg.Components)
	if err != nil {
		p.error(p.pos, err.Error())
	}

	return t
}

func (p *compiler) parseParameters(paramType string) (args abi.Arguments) {
	p.expect(token.LPAREN)

	if p.tok != token.RPAREN {
	L:
		for {
			typ := p.parseType()

			var (
				name    string
				indexed bool
			)

			switch paramType {
			case "func":
				_ = p.parseDataLocation()

				if p.tok == token.IDENT {
					name = p.parseIdent()
				}
			case "event":
				if p.tok == token.IDENT {
					name = p.parseIdent()

					indexed = name == "indexed"

					if indexed {
						name = p.parseIdent()
					}
				}
			case "error":
				if p.tok == token.IDENT {
					name = p.parseIdent()
				}
			default:
				panic("Unreachable")
			}

			args = append(args, abi.Argument{
				Name:    name,
				Type:    typ,
				Indexed: indexed,
			})

			switch p.tok {
			case token.COMMA:
				p.next()

				continue L
			case token.RPAREN:
				break L
			default:
				p.errorExpected(p.pos, "',' or ')'")

				return abi.Arguments{}
			}
		}
	}

	p.expect(token.RPAREN)

	return args
}

func (p *compiler) parseDataLocation() (dataLocation string) {
	if p.tok == token.IDENT {
		switch p.lit {
		case "memory", "storage", "calldata":
			dataLocation = p.lit

			p.next()
		}
	}

	return dataLocation
}

func (p *compiler) parseVisibility() (visibility string) {
	if p.tok == token.IDENT {
		switch p.lit {
		// case "internal", "private":
		case "external", "public":
			visibility = p.lit

			p.next()
		}
	}

	return visibility
}

func (p *compiler) parseMutability() string {
	var (
		view       bool
		pure       bool
		payable    bool
		nonpayable bool
		constant   bool
	)

L:
	for {
		switch p.lit {
		case "view":
			view = true
		case "pure":
			pure = true
		case "payable":
			payable = true
		case "nonpayable":
			nonpayable = true
		case "constant":
			constant = true
		default:
			break L
		}

		p.next()
	}

	if view {
		return "view"
	}
	if pure {
		return "pure"
	}
	if payable {
		return "payable"
	}
	if nonpayable {
		return "nonpayable"
	}
	if constant {
		return "view"
	}
	return ""
}

func MustCompile(ss ...string) (a abi.ABI) {
	a, err := Compile(ss...)
	if err != nil {
		panic(err)
	}
	return a
}

func Compile(ss ...string) (a abi.ABI, err error) {
	p := new(compiler)

	p.typeArgs = make(map[string]abi.ArgumentMarshaling, 0)

	fset := token.NewFileSet() // positions are relative to fset

	for i, s := range ss {
		p.init(fset, fmt.Sprintf("elem%d", i), []byte(s))

	L:
		for {
			switch p.tok {
			case token.EOF:
				break L
			case token.STRUCT:
				typeArg := p.parseStruct()

				p.typeArgs[typeArg.Name] = typeArg
			case token.IDENT:
				switch p.lit {
				case "enum":
					typeArg := p.parseEnum()

					p.typeArgs[typeArg.Name] = typeArg
				case "constructor": // "constructor(string symbol, string name)",
					a.Constructor = p.parseConstructor()
				case "fallback": // "fallback() payable",
					a.Fallback = p.parseFallback()
				case "receive": // "receive() payable",
					a.Receive = p.parseReceive()
				case "function": // "function getPerson(uint id) view returns (tuple(string name, uint16 age))",
					if a.Methods == nil {
						a.Methods = make(map[string]abi.Method)
					}

					method := p.parseFunction()

					name := abi.ResolveNameConflict(method.Name, func(s string) bool { _, ok := a.Methods[s]; return ok })

					method.Name = name

					a.Methods[name] = method
				case "event": // "event Transfer(address indexed from, address indexed to, address value)",
					if a.Events == nil {
						a.Events = make(map[string]abi.Event)
					}

					event := p.parseEvent()

					name := abi.ResolveNameConflict(event.Name, func(s string) bool { _, ok := a.Events[s]; return ok })

					event.Name = name

					a.Events[name] = event
				case "error": // "error InsufficientBalance(account owner, uint balance)",
					if a.Errors == nil {
						a.Errors = make(map[string]abi.Error)
					}

					error := p.parseError()

					a.Errors[error.Name] = error
				default:
					// no statement found
					pos := p.pos
					p.errorExpected(pos, "decl")

					break L
				}
			default:
				// no statement found
				pos := p.pos
				p.errorExpected(pos, "decl")

				break L
			}
		}

		if len(p.errors) > 0 {
			return abi.ABI{}, p.errors
		}
	}

	return a, nil
}

func (p *compiler) parseStruct() abi.ArgumentMarshaling {
	p.expect(token.STRUCT)

	name := p.parseIdent()

	var components []abi.ArgumentMarshaling

	p.expect(token.LBRACE)

L:
	for {
		switch p.tok {
		case token.IDENT:
			ftype := p.parseType()
			fname := p.parseIdent()

			components = append(components, abi.ArgumentMarshaling{
				Name: fname,
				Type: ftype.String(),
			})

			p.expect(token.SEMICOLON)
		case token.RBRACE:
			break L
		default:
			p.errorExpected(p.pos, "'IDENT' or '}'")
			break L
		}
	}

	p.expect(token.RBRACE)

	typeArg := abi.ArgumentMarshaling{
		Name:         name,
		Type:         "tuple",
		InternalType: fmt.Sprintf("struct %s", name),
		Components:   components,
	}

	p.accept(token.SEMICOLON)

	return typeArg
}

func (p *compiler) parseEnum() abi.ArgumentMarshaling {
	p.expectKeyword("enum")

	name := p.parseIdent()

	p.expect(token.LBRACE)

L:
	for {
		switch p.tok {
		case token.IDENT:
			_ = p.parseIdent()

			if p.accept(token.COMMA) {
				continue L
			}

			break L
		case token.RBRACE:
			break L
		default:
			p.errorExpected(p.pos, "'IDENT' or '}'")
			break L
		}
	}

	p.expect(token.RBRACE)

	typeArg := abi.ArgumentMarshaling{Name: name, Type: "uint8", InternalType: fmt.Sprintf("enum %s", name)}

	p.accept(token.SEMICOLON)

	return typeArg
}

func (p *compiler) parseConstructor() abi.Method {
	p.expectKeyword("constructor")

	inputs := p.parseParameters("func")

	_ = p.parseVisibility()

	mutability := p.parseMutability()

	var (
		outputs abi.Arguments
	)

	if p.acceptKeyword("returns") {
		outputs = p.parseParameters("func")
	}

	p.expectSemiOrEof()

	return abi.NewMethod("", "", abi.Constructor, mutability, mutability == "view" || mutability == "pure", mutability == "payable", inputs, outputs)
}

func (p *compiler) parseFallback() abi.Method {
	p.expectKeyword("fallback")

	inputs := p.parseParameters("func")

	_ = p.parseVisibility()

	mutability := p.parseMutability()

	var (
		outputs abi.Arguments
	)

	if p.acceptKeyword("returns") {
		outputs = p.parseParameters("func")
	}

	p.expectSemiOrEof()

	return abi.NewMethod("", "", abi.Constructor, mutability, mutability == "view" || mutability == "pure", mutability == "payable", inputs, outputs)
}

func (p *compiler) parseReceive() abi.Method {
	p.expectKeyword("receive")

	inputs := p.parseParameters("func")

	_ = p.parseVisibility()

	mutability := p.parseMutability()

	var (
		outputs abi.Arguments
	)

	if p.acceptKeyword("returns") {
		outputs = p.parseParameters("func")
	}

	p.expectSemiOrEof()

	return abi.NewMethod("", "", abi.Constructor, mutability, mutability == "view" || mutability == "pure", mutability == "payable", inputs, outputs)
}

func (p *compiler) parseFunction() abi.Method {
	p.expectKeyword("function")

	name := p.parseIdent()

	inputs := p.parseParameters("func")

	_ = p.parseVisibility()

	mutability := p.parseMutability()

	var (
		outputs abi.Arguments
	)

	if p.acceptKeyword("returns") {
		outputs = p.parseParameters("func")
	}

	p.expectSemiOrEof()

	return abi.NewMethod(name, name, abi.Function, mutability, mutability == "view" || mutability == "pure", mutability == "payable", inputs, outputs)
}

func (p *compiler) parseEvent() abi.Event {
	p.expectKeyword("event")

	name := p.parseIdent()

	inputs := p.parseParameters("event")

	anonymous := p.acceptKeyword("anonymous")

	p.expectSemiOrEof()

	return abi.NewEvent(name, name, anonymous, inputs)
}

func (p *compiler) parseError() abi.Error {
	p.expectKeyword("error")

	name := p.parseIdent()
	inputs := p.parseParameters("error")

	p.expectSemiOrEof()

	return abi.NewError(name, inputs)
}
