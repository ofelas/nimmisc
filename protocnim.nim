# Attempt to parse Protobuf IDL in Nim
# Did? Work with conformance.proto and all POGO files, that's a start...
#
# While the code is a mess the general idea appears to be similar (by
# accident) to the Google Protobuf Compiler in C++ which I take as a
# good sign. I just pulled the new 3.0.0 release from Github to
# process some POGO*files.
#
{.deadCodeElim: on.}
{.passC: "-Wall".}

import system
import os
import strutils
import sequtils
import math
import macros
import parseutils
import hashes
import algorithm
import tables

# import protobuf

# Stolen from lexer.nim with a grain of guessing
const
  NimKeywords : array[0..69, string] = [
    "addr",
    "and",
    "as",
    "asm",
    "atomic",
    "bind",
    "block",
    "break",
    "case",
    "cast",
    "concept",
    "const",
    "continue",
    "converter",
    "defer",
    "discard",
    "distinct",
    "div",
    "do",
    "elif",
    "else",
    "end",
    "enum",
    "except",
    "export",
    "finally",
    "for",
    "from",
    "func",
    "generic",
    "if",
    "import",
    "in",
    "include",
    "interface",
    "is",
    "isnot",
    "iterator",
    "let",
    "macro",
    "method",
    "mixin",
    "mod",
    "nil",
    "not",
    "notin",
    "object",
    "of",
    "or",
    "out",
    "proc",
    "ptr",
    "raise",
    "ref",
    "return",
    "shl",
    "shr",
    "static",
    "template",
    "try",
    "tuple",
    "type",
    "using",
    "var",
    "when",
    "while",
    "with",
    "without",
    "xor",
    "yield",
  ]

const
  # previously ThriftKeywords, now in a flux
  ProtobufKeywords : array[0..36, string] = [
    "bool",
    "bytes",
    "double",
    "enum",
    "extend",
    "extensions", # ranges
    "false",
    "fixed32",
    "fixed64",
    "float",
    "import",
    "int32",
    "int64",
    "map",
    "message",
    "oneof",
    "option",
    "optional", # required
    "package",
    # "packed",
    "public",
    "repeated",
    "required", # optional
    "reserved", # extensions
    "returns",
    "rpc",
    "service",
    "sfixed32",
    "sfixed64",
    "sint32",
    "sint64",
    "stream",
    "string",
    "syntax",
    "true",
    "uint32",
    "uint64",
    "weak",
  ]

  ProtobufKeytypes : array[0..11, string] = [
    "bool",
    "fixed32",
    "fixed64",
    "int32",
    "int64",
    "sfixed32",
    "sfixed64",
    "sint32",
    "sint64",
    "string",
    "uint32",
    "uint64",
  ]

  # These seems to come from other .proto files...
  GoogleTypes: array[0..14, string] = [
    "google.protobuf.Any",
    "google.protobuf.BoolValue",
    "google.protobuf.BytesValue",
    "google.protobuf.DoubleValue",
    "google.protobuf.Duration",
    "google.protobuf.FieldMask",
    "google.protobuf.FloatValue",
    "google.protobuf.Int32Value",
    "google.protobuf.Int64Value",
    "google.protobuf.StringValue",
    "google.protobuf.Struct",
    "google.protobuf.Timestamp",
    "google.protobuf.UInt32Value",
    "google.protobuf.UInt64Value",
    "google.protobuf.Value",
    ]

# proto3 with some proto2 sprinkled...
# letter = "A" … "Z" | "a" … "z"
# decimalDigit = "0" … "9"
# octalDigit   = "0" … "7"
# hexDigit     = "0" … "9" | "A" … "F" | "a" … "f"

# ident = letter { letter | decimalDigit | "_" }
# fullIdent = ident { "." ident }
# messageName = ident
# enumName = ident
# fieldName = ident
# oneofName = ident
# mapName = ident
# serviceName = ident
# rpcName = ident
# messageType = [ "." ] { ident "." } messageName
# enumType = [ "." ] { ident "." } enumName

# intLit     = decimalLit | octalLit | hexLit
# decimalLit = ( "1" … "9" ) { decimalDigit }
# octalLit   = "0" { octalDigit }
# hexLit     = "0" ( "x" | "X" ) hexDigit { hexDigit }

# floatLit = ( decimals "." [ decimals ] [ exponent ] | decimals exponent | "."decimals [ exponent ] ) | "inf" | "nan"
# decimals  = decimalDigit { decimalDigit }
# exponent  = ( "e" | "E" ) [ "+" | "-" ] decimals

# boolLit = "true" | "false"

# strLit = ( "'" { charValue } "'" ) |  ( '"' { charValue } '"' )
# charValue = hexEscape | octEscape | charEscape | /[^\0\n\\]/
# hexEscape = '\' ( "x" | "X" ) hexDigit hexDigit
# octEscape = '\' octalDigit octalDigit octalDigit
# charEscape = '\' ( "a" | "b" | "f" | "n" | "r" | "t" | "v" | '\' | "'" | '"' )
# quote = "'" | '"'

# emptyStatement = ";"

# constant = fullIdent | ( [ "-" | "+" ] intLit ) | ( [ "-" | "+" ] floatLit ) | strLit | boolLit

# * syntax = "syntax" "=" quote "proto3" quote ";"
# * import = "import" [ "weak" | "public" ] strLit ";"
# * package = "package" fullIdent ";"
# * option = "option" optionName  "=" constant ";"
# * optionName = ( ident | "(" fullIdent ")" ) { "." ident }
# * type = "double" | "float" | "int32" | "int64" | "uint32" | "uint64"
#       | "sint32" | "sint64" | "fixed32" | "fixed64" | "sfixed32" | "sfixed64"
#       | "bool" | "string" | "bytes" | messageType | enumType
# * fieldNumber = intLit;
# * field = [ "repeated" ] type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
# * fieldOptions = fieldOption { ","  fieldOption }
# * fieldOption = optionName "=" constant
# * oneof = "oneof" oneofName "{" { oneofField | emptyStatement } "}"
# * oneofField = type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
# - mapField = "map" "<" keyType "," type ">" mapName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
# - keyType = "int32" | "int64" | "uint32" | "uint64" | "sint32" | "sint64" |
#           "fixed32" | "fixed64" | "sfixed32" | "sfixed64" | "bool" | "string"
# reserved = "reserved" ( ranges | fieldNames ) ";"
# e.g. ranges = 2, 15, 9 to 11
# fieldNames = fieldName { "," fieldName }
# enum = "enum" enumName enumBody
# enumBody = "{" { option | enumField | emptyStatement } "}"
# enumField = ident "=" intLit [ "[" enumValueOption { ","  enumValueOption } "]" ]";"
# enumValueOption = optionName "=" constant
# message = "message" messageName messageBody
# messageBody = "{" { field | enum | message | option | oneof | mapField |
# reserved | emptyStatement } "}"
# service = "service" serviceName "{" { option | rpc | stream | emptyStatement } "}"
# rpc = "rpc" rpcName "(" [ "stream" ] messageType ")" "returns" "(" [ "stream" ]
# messageType ")" (( "{" {option | emptyStatement } "}" ) | ";")
# proto = syntax { import | package | option | topLevelDef | emptyStatement }
# topLevelDef = message | enum | service

# A wee helper for tracking lexing/parsing source location
type
  Location = tuple[line, col: int]

proc `$`(location: Location): string =
  $location.line & ":" & $location.col

type
  TokenKind = enum
    tkIdentifier = (0, "Identifier"), tkString = "String", tkComment = "Comment",
    tkInt = "Integer", tkFloat = "Float",
    tkAssign, tkRef, tkDot, tkStar,  tkSemi, tkColon, tkComma,
    tkLparen, tkRparen, tkLcurly, tkRcurly,
    tkLsquare, tkRsquare, tkLangle, tkRangle,
    tkWs,
    tkEof = "End of File",
    tkError = "Error"

  Token = object
    literal*: string
    location: Location
    case kind: TokenKind
    of tkIdentifier, tkString, tkComment: discard
    of tkInt: intVal: int
    of tkFloat: floatVal: float
    of tkAssign, tkRef, tkDot, tkStar,
       tkSemi, tkColon, tkComma: discard
    of tkLparen, tkRparen, tkLcurly, tkRcurly,
       tkLsquare, tkRsquare, tkLangle, tkRangle: discard
    of tkWs: discard
    of tkEof: discard
    of tkError: discard

# let us do some simple token comparison
proc `==`(t: Token, tk: TokenKind): bool {.inline.} =
  t.kind == tk

proc contains(tks: set[TokenKind], t: Token): bool =
  t.kind in tks

proc isComment(t: Token): bool {.noSideEffect, procvar, inline.} =
  t.kind == tkComment

proc isStar(t: Token): bool {.inline.} =
  t.kind == tkDot

proc isIdentifier(t: Token): bool {.inline.} =
  t.kind == tkIdentifier and (t.literal notin ProtobufKeywords)

proc isKeyword(t: Token, kw: string = nil): bool {.inline.} =
  result = t.kind == tkIdentifier and t.literal in ProtobufKeywords
  if result and not kw.isNil:
    result = t.literal == kw

# well, this may be frown at but my excuse is that I'm learning Nim, 8)
proc `===`(t: Token, kw: string): bool {.inline.} =
  t.isKeyword(kw)

proc isBasetype(t: Token): bool =
  # protobuf basetypes
  t.isKeyword and t.literal in ["double",  "float", "int32", "int64",
                                "uint32", "uint64", "sint32", "sint64",
                                "fixed32", "fixed64", "sfixed32", "sfixed64",
                                "bool", "string", "bytes"]

proc isMaptype(t: Token): bool =
  # protobuf maptypes
  t.isKeyword and ProtobufKeytypes.binarySearch(t.literal) > -1

proc isGoogletype(t: Token): bool =
  # protobuf basetypes
  t.isKeyword and t.literal in GoogleTypes
  
proc isKeywords(t: Token, values: varargs[string]): bool =
  # binarySearch or in?!?
  t.kind == tkIdentifier and ProtobufKeywords.binarySearch(t.literal) > -1 and t.literal in values

proc newErrorToken(s: string, location: Location): Token =
  Token(kind: tkError, literal: s, location: location)

proc newIdentifierToken(s: string, location: Location): Token =
  if s notin ProtobufKeywords and s in NimKeywords:
    echo "WARNING: `" & s & "' may be reserved in Nim"
  Token(kind: tkIdentifier, literal: s, location: location)

proc newStringToken(s: string, loc: Location): Token =
  Token(kind: tkString, literal: s, location: loc)

proc newIntToken(s: string, value: int, location: Location): Token =
  Token(kind: tkInt, literal:s, intVal: value, location: location)

proc newFloatToken(s: string, value: float, location: Location): Token =
  Token(kind: tkFloat, literal:s, floatVal: value, location: location)

type
  Parser = ref object of RootObj
    buffer: string              # buffer
    pos*: int                   # current position in buffer
    line: int                   # current line
    bol: int                    # beginning of line
    rune: char                  # current character, used to do unicode...
    tokens: seq[Token]

proc reset(parser: var Parser) =
  parser.pos = 0
  parser.line = 1
  parser.bol = 0
  parser.tokens = newSeq[Token]()

proc currentLocation(parser: Parser): Location =
  (line: parser.line, col: parser.pos - parser.bol)

proc validatePos(parser: Parser): bool {.inline.} =
  result = parser.pos > -1 and parser.pos <= parser.buffer.high

proc nextChar(parser: Parser): char {.inline.}=
  if validatePos(parser):
    result = parser.buffer[parser.pos]
  else:
    result = '\0'

proc consumeChar(parser: var Parser): char {.inline.}=
  if validatePos(parser):
    parser.rune = parser.buffer[parser.pos]
    result = parser.rune
    parser.pos.inc
    if parser.rune == '\l':
      parser.line.inc
      parser.bol = parser.pos
  else:
    result = '\0'

const
  NumberChars = Digits
  LowerChars = {'a' .. 'z'}
  UpperChars = {'A' .. 'Z'}
  HexChars = {'0' .. '9', 'a' .. 'f', 'A' .. 'F'}
  IdentifierChars = {'0' .. '9', 'a' .. 'z', 'A' .. 'Z', '_', '.'}

# some remains from some Thrift parsing...
type
  ParseStatus = tuple[status: bool, consumed: int]
  ThriftTypeAnnotation = tuple[id, value: string]
  ThriftTypeAnnotations = OrderedTable[string, ThriftTypeAnnotation]
  TypeAnnotations = tuple[res: ParseStatus, ta: ThriftTypeAnnotations]

type
  ProtobufEnum = object
    name: string
    package: string
    unique: bool
    values: OrderedTable[string, int]

proc hash(pbe: ProtobufEnum): Hash =
  result = pbe.package.hash !& pbe.name.hash
  result = !$result
    
    
# include "cdefines.tmpl", not here yet, oh this was for something else...

type
  ProtobufSyntax = string
  ProtobufPackage = string      # this could be a container for all other stuff...
  ProtobufImport = tuple[path: string, style: string]
  ProtobufField = tuple[fieldname, fieldtype, fieldval: string, repeated, packed: bool]
  ProtobufMessage = object
    name: string
    package: string
    # fields OrderedTable of something
    fields: OrderedTable[string, ProtobufField]

proc hash(p: ProtobufMessage): Hash =
  result = p.package.hash !& p.name.hash
  result = !$result
    
# should live in the parser, but there again...
# These should be per package and not global...
var
  gEnums =  newSeq[ProtobufEnum]()
  gEnumsUnique = newSeq[string]()
  gMessagesUnique = newSeq[string]()
  gReqRspMsgs = newSeq[string]()
  gMessages = newOrderedTable[string, ProtobufMessage]()
  enumstable = newOrderedTable[string, ProtobufEnum]()
  pkgimports = newSeq[ProtobufImport]()
  gCurrentPackage: string = ""

let pbuf2nimMapping = {"bytes":"seq[uint8]", "string":"string", "int32":"int32", "int64":"int64",
                             "uint32":"uint32", "uint64":"uint64", "bool":"bool",
                             "double":"float64", "float":"float32", "fixed32":"int32", "fixed64":"int64"}.newTable
            
# end collection of stuff...

proc parseBinInt(s: string, strict: bool = false): int {.noSideEffect, procvar, raises: [ValueError].} =
  ## We might shift more than our int can take it seems, how do we catch overflow?
  ## Is there a Nim proc for converting ``0b0101`` to an ``int``?
  ## Or as often before, I misunderstood the whole thing
  var i = 0
  var max_shifts = sizeof(int) * 8
  var shifts = 0
  if s[i] == '0' and (s[i+1] == 'b' or s[i+1] == 'B'):
    inc(i, 2)
  while true:
    case s[i]
    of '1':
      result = (result shl 1) or 1
      inc(i)
      inc(shifts)
    of '0':
      result = result shl 1
      inc(i)
      inc(shifts)
    of '_':
      inc(i)
    of '\0':
      break
    else:
      raise newException(ValueError, "invalid integer: " & s & ", looking at " & $s[i])
  if strict and shifts > max_shifts:
    raise newException(ValueError, "invalid integer: " & s & ", " & $result & ", shifted=" & $shifts)

proc dottedIdentifier(t: seq[Token]): (bool, string, int) {.noSideEffect.} =
  ## parse an identifier optionally dotted
  if t[0].isIdentifier:
    if t[1].kind == tkDot and t[2].isIdentifier:
      result = (true, foldl(t, a & b.literal, ""), 3)
    else:
      result = (true, t[0].literal, 1)

proc consumeComments(tl: seq[Token]): int {.inline.} =
  ## consume leading comments (which actually could be anywhere)
  let m = tl.len
  while result < m and tl[result].isComment: inc(result)

# stmt -> typed, expr -> untyped
# Now, is this convenience or obfuscation?
template consumeif(test: bool, result: bool, stepper: int, stepby: int = 1) =
  result = test
  if result:
    inc(stepper, stepby)

template consume(test, result: bool, consumed: int, amount: int = 1): bool =
  ## produce test in result and update consumed by step if test is true
  result = test
  if result: inc(consumed, amount)
  result

template optional(test: bool, stepper: int, amount: int = 1) =
  if test:
    inc(stepper, amount)

import terminal
proc warning(msg: string) =
  styledEcho(terminal.fgBlack, terminal.bgYellow, "WARNING: ", terminal.resetStyle, msg)

# type annotations without cpp include
# this should go away as it is thrifty...
proc parseTypeAnnotations(tl: seq[Token]): TypeAnnotations =
  var i = 0
  result.ta = initOrderedTable[string, ThriftTypeAnnotation]()
  inc(i, consumeComments(tl[i..^1]))
  if i >= tl.len:
    return
  if tl[i] == tkLparen:
    inc(i)
    # consume until ')'...
    var
      keepGoing = true
      ident: string = ""
      step = 0
    while keepGoing:
      let loc = tl[i].location
      (keepGoing, ident, step) = dottedIdentifier(tl[i..i+2])
      if keepGoing:
        inc(i, step)
        # let's try some templates
        if consume(tl[i] == tkAssign, keepGoing, i): # keepGoing = tl[i] == tkAssign
          if consume(tl[i] == tkString, keepGoing, i, 0): # keepGoing = tl[i] == tkString
            let v : ThriftTypeAnnotation = (id: ident, value: tl[i].literal)
            inc(i)
            if result.ta.hasKeyOrPut(ident, v):
              # just keep going
              warning "Duplicate annotation near " & $loc & " " & $v & " already in " & $result.ta
            # optional: , or ;
            optional(tl[i] in {tkComma, tkSemi}, i) # keepGoing = tl[i] in {tkComma, tkSemi}
            #if keepGoing:
            #  inc(i)
    # may not be correct if we failed...
    if consume(tl[i] == tkRparen, keepGoing, i):
      # inc(i)
      result.res.status = true
      result.res.consumed = i
  else:
    result.res.status = true
    result.res.consumed = i

proc parseEnum(tl: seq[Token], owner: string = ""): ParseStatus =
  # should return the name and list of values, ProtobufEnum thingy
  # get rid of the gEnum... variables
  var
    i = 0
    ok = false
    ident: string = ""
    step = 0
    enumdef = ProtobufEnum(name: "", package: gCurrentPackage, unique: false, values: initOrderedTable[string, int]())
    revvalues = initOrderedTable[int, string]()
  inc(i, consumeComments(tl[i..^1]))
  if consume(tl[i] === "enum", ok, i):
    (ok, ident, step) = dottedIdentifier(tl[i..i+2])
    if ok:
      inc(i, step)
      # starts with a '{'
      if consume(tl[i] == tkLcurly, ok, i):
        # consume the fields: NAME = value | NAME
        # This should probably be something scope like...
        let longname = owner & ident
        ident = longname
        echo "ENUM[" & gCurrentPackage & "] " & longname
        let unique = longname notin gEnumsUnique
        if unique:
          gEnumsUnique &= longname
        enumdef.name = ident
        enumdef.unique = unique
        # now parse the individual fields
        var
          nextval: int = 0
          fname: string = ""
        while true:
          inc(i, consumeComments(tl[i..^1]))
          let loc = tl[i].location
          (ok, fname, step) = dottedIdentifier(tl[i..i+2])
          if ok:
            var val: int = 0
            inc(i, step)
            if consume(tl[i] == tkAssign, ok, i):
              # This will fail if we haven't got an int...
              val = tl[i].intVal
              nextval = val + 1
              inc(i)
            else:
              val = nextval
              inc(nextval)
            echo "EFIELD: " & $ident & "." & $fname & "=" & $val
            if enumdef.values.hasKeyOrPut(fname, val):
              warning "Duplicate ENUM name " & $loc & " " & fname & " already in " & $enumdef
            # check the reverse mapping
            if revvalues.hasKeyOrPut(val, fname):
              warning "Duplicate ENUM value " & $loc & " " & $val & " already in " & $revvalues
            # NOTE: there may be options in []
            # NOTE: required comma ';'
            optional(tl[i] == tkSemi, i)
            # eat comments
            inc(i, consumeComments(tl[i..^1]))
          else:
            break
          if tl[i] == tkRcurly:
            break
        # ends with a '}'
        if consume(tl[i] == tkRcurly, ok, i):
          # the first enum value should be 0 as per protobuf requirements...
          gEnums &= enumdef
          result = (true, i)

# field type
proc parseFieldType(tl: seq[Token]): (bool, int, string) =
  var i = 0
  inc(i, consumeComments(tl[i..^1]))
  # identifer | basetype(thrift base types) | containertype
  if tl[i].isIdentifier:
    let (ok, ft, step) = dottedIdentifier(tl[i..i+2])
    if ok:
      inc(i, step)
      # must be resolved...
      result = (true, i, ft)
  elif tl[i].isBasetype:
    let ft = tl[i].literal
    inc(i)
    result = (true, i, ft)
  elif tl[i].isKeywords("set", "map", "list"):
    let t = tl[i].literal
    var vs: string = ""
    vs &= t
    inc(i)
    if t == "map":
      if tl[i] == tkLangle:
        inc(i)
        let (ok, step, s) = parseFieldType(tl[i..^1])
        if ok:
          vs &= "(" & s
          inc(i, step)
          if tl[i] == tkComma:
            inc(i)
            let (ok, step, s) = parseFieldType(tl[i..^1])
            if ok:
              vs &= "->" & s
              inc(i, step)
              if tl[i] == tkRangle:
                vs &= ")"
                inc(i)
                result = (true, i, vs)
    else:
      if tl[i] == tkLangle:
        inc(i)
        let (ok, step, s) = parseFieldType(tl[i..^1])
        if ok:
          vs &= "." & s
          inc(i, step)
          if tl[i] == tkRangle:
            inc(i)
            result = (true, i, vs)
  else:
    #echo "Nope: " & $tl[i]
    result[0] = true

  if result[0] == true:
    if tl[i] == tkLparen:
      result[0] = false
      let ta = parseTypeAnnotations(tl[i..^1])
      if ta.res.status:
        if ta.ta.len > 0: echo "TA:" & $ta
        inc(i, ta.res.consumed)
        result[0] = true
        result[1] = i

proc parseFieldList(tl: seq[Token], start: TokenKind = tkLcurly, stop: TokenKind = tkRcurly ): (bool, int) =
  var
    i = 0
    ok = false
    step = 0
  inc(i, consumeComments(tl[i..^1]))
  # field list, refactor to separate proc...
  if consume(tl[i] == start, ok, i):
    inc(i, consumeComments(tl[i..^1]))
    if consume(tl[i] == stop, ok, i):
      # empty list
      result = (true, i)
      return
    var nextfi: int = 1
    while true:
      var
        fi = nextfi + 1
        ft: string = ""
      # optional field id (e.g. '1:'')
      if tl[i] == tkInt and tl[i+1] == tkColon:
        fi = tl[i].intVal
        inc(i, 2)
        nextfi = fi + 1
      else:
        fi = nextfi
        inc(nextfi)
      # optional requiredness
      if tl[i].isKeywords("optional", "required"):
        inc(i)
      (ok, step, ft) = parseFieldType(tl[i..^1])
      if ok:
        inc(i, step)
        # optional field reference '&'
        if tl[i] == tkRef:
          inc(i)
        if tl[i].isIdentifier:
          var
            ident: string = ""
          (ok, ident, step) = dottedIdentifier(tl[i..i+2])
          if ok:
            var
              dv : Token
              hasdv = false
            inc(i, step)
            # optional default value
            if consume(tl[i] == tkAssign, ok, i):
              # blindly get value
              dv = tl[i]
              hasdv = true
              inc(i)
            # no xsd this or that support
            # optional type annotation
            let ta = parseTypeAnnotations(tl[i..^1])
            if ta.res.status:
              inc(i, ta.res.consumed)
            optional(tl[i] in {tkComma, tkSemi}, i)
            if hasdv:
              echo "  " & $fi & ":" & ident & ", ft=" & ft & "=" & $dv
            else:
              echo "  " & $fi & ":" & ident & ", ft=" & ft
        else:
          break
      else:
        break
      inc(i, consumeComments(tl[i..^1]))
      if consume(tl[i] == stop, ok, i):
        result = (true, i)
        break
  else:
    result = (true, i)

proc parseOneof(tl: seq[Token]): ParseStatus =
  var i = 0
  let comments = consumeComments(tl[i..^1])
  inc(i, comments)
  result.consumed = comments
  if tl[i] === "oneof":
    inc(i)
    if tl[i].isIdentifier:
      let (ok, sname, step) = dottedIdentifier(tl[i..i+2])
      if ok:
        inc(i, step)
        echo "ONEOF: " & sname
        if tl[i] == tkLcurly:
          let (ok, step) = parseFieldList(tl[i..^1])
          if ok:
            inc(i, step)
            # optional type annotation, nope, that's thrift but harmless it seems...
            let (ps, ta) = parseTypeAnnotations(tl[i..^1])
            if ps.status:
              if ta.len > 0: echo "TA: " & $ta
              inc(i, ps.consumed)
              result = (true, i)
            # optional ',' or ';'
            # optional(tl[i] == tkSemi, i)
            if tl[i] == tkRcurly:
              # inc(i)
              result.status = true
              result.consumed = i

# service = "service" serviceName "{" { option | rpc | stream | emptyStatement } "}"
# rpc = "rpc" rpcName "(" [ "stream" ] messageType ")" "returns" "(" [ "stream" ]
#       messageType ")" (( "{" {option | emptyStatement } "}" ) | ";")

proc parseOption(tl: seq[Token]): ParseStatus # ah, forward decl

proc parseRpc(tl: seq[Token]): ParseStatus =
  ## this is thrift at the moment
  var
    i = 0
    ok = false
    rpcname: string = ""
    rpcstream = false
    rpcmsg: string = ""
    retmsg: string = ""
    retstream = false
  # echo "Looking for exception:" & $tl[i..i+4]
  inc(i, consumeComments(tl[i..^1]))
  if consume(tl[i] === "rpc", ok, i):
    if tl[i].isIdentifier:
      rpcname = tl[i].literal
      inc(i)
      if consume(tl[i] == tkLparen, ok, i):
        if tl[i] === "stream":
          inc(i)
          rpcstream = true
        if tl[i].isIdentifier:
          rpcmsg = tl[i].literal
          inc(i)
          if consume(tl[i] == tkRparen, ok, i):
            if consume(tl[i] === "returns", ok, i):
              if consume(tl[i] == tkLparen, ok, i):
                if tl[i] === "stream":
                  inc(i)
                  retstream = true
                if tl[i].isIdentifier:
                  retmsg = tl[i].literal
                  inc(i)
                  if consume(tl[i] == tkRparen, ok, i):
                    optional(tl[i] == tkSemi, i)
                    result = (true, i)
                    # could be option or empty inside curlies {}
                
          echo "RPC: " & rpcname & ":" & rpcmsg & ", s:" & $rpcstream & ", " & retmsg & ", s:" & $retstream

proc parseService(tl: seq[Token]): ParseStatus =
  ## this is thrift at the moment
  var
    i = 0
    ok = false
  # echo "Looking for exception:" & $tl[i..i+4]
  inc(i, consumeComments(tl[i..^1]))
  if consume(tl[i] === "service", ok, i):
    if tl[i].isIdentifier:
      let ident = tl[i].literal
      inc(i)
      echo "SERVICE: " & ident
      if consume(tl[i] == tkLcurly, ok, i):
        # functionlist...
        while true:
          inc(i, consumeComments(tl[i..^1]))
          # option, rpc, stream, emptystatement
          if tl[i] == tkSemi:
            inc(i)
          elif tl[i] === "option":
            let ps = parseOption(tl[i..^1])
            if ps.status:
              inc(i, ps.consumed)
          elif tl[i] === "rpc":
            let ps = parseRpc(tl[i..^1])
            if ps.status:
              inc(i, ps.consumed)
          elif tl[i] == tkRcurly:
            inc(i)
            result = (true, i)
            break
        if result.status:
          let (ps, ta) = parseTypeAnnotations(tl[i..^1])
          if ps.status:
            inc(i, ps.consumed)
            if ta.len > 0: echo "TA: " & $ta
            result[1] = i

# option = "option" optionName  "=" constant ";"
# optionName = ( ident | "(" fullIdent ")" ) { "." ident }
proc parseOption(tl: seq[Token]): ParseStatus =
  var
    i = 0
    ok = false
    oname: string = ""
    oconstant: string = ""
  inc(i, consumeComments(tl[i..^1]))
  result.consumed = i
  if consume(tl[i] === "option", ok, i):
    if consume(tl[i] == tkDot, ok, i):
      discard
    elif consume(tl[i] == tkLparen, ok, i):
      discard
    else:
      if tl[i].isIdentifier:
        oname = tl[i].literal
        inc(i)
        ok = true
    if ok:
      if consume(tl[i] == tkAssign, ok, i):
        if tl[i] in {tkString, tkIdentifier}:
          oconstant = tl[i].literal
          inc(i)
          if consume(tl[i] == tkSemi, ok, i):
            echo "OPTION: " & oname & ":" & oconstant
            result = (true, i)

proc depsList(m: ProtobufMessage): seq[string] =
  result = newSeq[string]()
  for v in m.fields.values:
    if pbuf2nimMapping.hasKey(v.fieldtype):
      continue
    elif enumstable.hasKey(v.fieldtype):
      continue
    elif v.fieldtype.count(".") > 0:
      let enn = v.fieldtype.split('.')[^2..^1].join("")
      if enumstable.hasKey(enn):
        continue
      elif v.fieldtype.find(".Enums.") < 0:
        result.add(v.fieldtype)
      else:
        result.add(v.fieldtype)
    else:
      result.add(v.fieldtype)
  return result
     
proc isNativeLike(m: ProtobufMessage): bool =
  for v in m.fields.values:
    let en = v.fieldtype.split('.')[^1]
    if pbuf2nimMapping.hasKey(v.fieldtype):
      result = true
    elif enumstable.hasKey(en):
      result = true
    elif gMessages.hasKey(en):
      result = false
      break
    elif v.fieldtype.count(".") > 0:
      let enn = v.fieldtype.split('.')[^2..^1].join("")
      if enumstable.hasKey(enn):
        result = true
      else:
        result = false
        break
    else:
      result = false
      break
  return result
    
# "ident:" & fieldtype & ":" & fieldname & ":" & fieldval & ", repeated:" & $repeated & ", packed:" $packed
# field = [ "repeated" ] type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
# messageBody = "{" { field | enum | message | option | oneof | mapField | reserved | emptyStatement
# message = "message" messageName messageBody
proc parseMessage(tl: seq[Token], owner: string = "", level: int = 1): ParseStatus =
  var
    i = 0
    ok = false
    unique = false
    rr = false
    mname: string = ""
    msg = ProtobufMessage(name: "", package: gCurrentPackage, fields: initOrderedTable[string, ProtobufField]())
  inc(i, consumeComments(tl[i..^1]))
  result.consumed = i
  if consume(tl[i] === "message", ok, i):
    if tl[i].isIdentifier:
      mname = tl[i].literal
      msg.name = mname
      echo ">> MESSAGE[" & $level & "]: " & $mname
      if owner.len == 0:
        unique = mname notin gMessagesUnique
        if mname.endsWith("Message") or mname.endsWith("Response"):
          gReqRspMsgs &= mname
          rr = true
      if unique and not rr:
        gMessagesUnique &= mname
      inc(i)
      if consume(tl[i] == tkLcurly, ok, i):
        # now any: enum, message, option, oneof, field, mapField, reserved, emptyStatement
        while ok:
          # eat comments
          inc(i, consumeComments(tl[i..^1]))
          # echo "At:" & $i & ":" & $tl[i]
          if tl[i] == tkSemi:
            # empty
            inc(i)
          elif tl[i] === "message":
            let ps = parseMessage(tl[i..^1], mname, level + 1)
            ok = ps.status
            if ok:
              inc(i, ps.consumed)
          elif tl[i] === "enum":
            let ps = parseEnum(tl[i..^1], mname)
            ok = ps.status
            if ok:
              inc(i, ps.consumed)
          elif tl[i] === "oneof":
            let ps = parseOneof(tl[i..^1])
            ok = ps.status
            if ok:
              inc(i, ps.consumed)
          elif tl[i] === "option":
            let ps = parseOption(tl[i..^1])
            ok = ps.status
            if ok:
              inc(i, ps.consumed)
          elif tl[i] === "extend":
            echo "Skipping NOT implemented: " & $tl[i].literal
            # skip until }
            while true:
              if tl[i] == tkRcurly:
                inc(i)
                break
              else:
                inc(i)
          elif tl[i] === "reserved" or tl[i] === "extensions":
            echo "Skipping NOT implemented: " & $tl[i].literal
            inc(i)
            # skip until ;
            while true:
              if tl[i] == tkSemi:
                inc(i)
                break
              else:
                inc(i)
          elif tl[i] === "map":
            # mapField
            inc(i)
            if tl[i] == tkLangle:
              inc(i)
              # isMaptype...
              while true:
                if tl[i] == tkRangle:
                  inc(i)
                  break
                else:
                  inc(i)
              # then as below, something = anotherthing
              var
                fieldname: string = ""
                fieldval: string = ""
              fieldname = tl[i].literal
              inc(i)
              if consume(tl[i] == tkAssign, ok, i):
                fieldval = tl[i].literal
                inc(i)
                if consume(tl[i] == tkSemi, ok, i):
                  echo "ident: MAP" & ":" & fieldname & ":" & fieldval
          elif tl[i].isIdentifier or tl[i].isBasetype or tl[i].isKeywords("repeated", "optional", "required"):
            # assume it is a field right now
            # echo $tl[i..i + 6]
            var
              repeated = false
              fieldname: string = ""
              fieldval: string = ""
              fieldtype: string = ""
              required = false
              packed = false
            if tl[i] === "repeated":
              repeated = true
              inc(i)
            if tl[i].isKeywords("optional", "required"):
              required = tl[i].literal == "required"
              inc(i)
            fieldtype = tl[i].literal
            inc(i)
            fieldname = tl[i].literal
            inc(i)
            if consume(tl[i] == tkAssign, ok, i):
              fieldval = tl[i].literal
              inc(i)
              # fieldOptions?
              if tl[i] == tkLsquare:
                # consume until tkRsquare, these may be packed hints for repeated fields...
                inc(i)
                if tl[i].literal == "packed":
                  inc(i)
                  if tl[i] == tkAssign and tl[i+1] === "true":
                    packed = true
                while true:
                  if tl[i] == tkRsquare:
                    inc(i)
                    break
                  else:
                    inc(i)
              if consume(tl[i] == tkSemi, ok, i):
                discard msg.fields.hasKeyOrPut(fieldname, (fieldname: fieldname, fieldtype: fieldtype, fieldval: fieldval, repeated: repeated, packed: packed))
                echo "ident:" & fieldtype & ":" & fieldname & ":" & fieldval & ", repeated:" & $repeated & ", packed:" & $packed
          elif tl[i] == tkRcurly:
            inc(i)
            result = (true, i)
            break
          else:
            echo $tl[i]
            break
      echo "<< MESSAGE[" & $level & "]: " & $mname
      discard gMessages.hasKeyOrPut(msg.name, msg)


# import = "import" [ "weak" | "public" ] strLit ";"

proc parseImport(tl: seq[Token]): (ParseStatus, ProtobufImport) =
  var
    i = 0
    ok = false
    iname: string = ""
    istyle: string = ""
  inc(i, consumeComments(tl[i..^1]))
  result[0].consumed = i
  if consume(tl[i] === "import", ok, i):
    if tl[i].isKeywords("weak", "public"):
      istyle = tl[i].literal
      inc(i)
    if tl[i] == tkString:
      iname = tl[i].literal
      inc(i)
      if consume(tl[i] == tkSemi, ok, i):
        result = ((true, i), (path: iname.replace("\"", ""), style: istyle))

# package = "package" fullIdent ";"
proc parsePackage(tl: seq[Token]): (ParseStatus, ProtobufPackage) =
  var
    i = 0
    ok = false
  inc(i, consumeComments(tl[i..^1]))
  result[0].consumed = i
  if consume(tl[i] === "package", ok, i):
    let (rc, pname, step) = dottedIdentifier(tl[i..i+2])
    if rc:
      inc(i, step)
      if consume(tl[i] == tkSemi, ok, i):
        # echo "PACKAGE: " & pname
        # set the current package name in a global, hmm...
        result = ((true, i), pname)

# syntax = "syntax" "=" quote "proto3" quote ";"
proc parseSyntax(tl: seq[Token]): (ParseStatus, ProtobufSyntax) =
  var
    i = 0
    ok = false
  inc(i, consumeComments(tl[i..^1]))
  result[0].consumed = i
  if consume(tl[i] === "syntax", ok, i):
    if consume(tl[i] == tkAssign, ok, i):
      if tl[i] == tkString:
        let syntax = tl[i].literal
        inc(i)
        if consume(tl[i] == tkSemi, ok, i):
          # echo "SYNTAX: " & syntax & ", good=" & $(syntax == "\"proto3\"")
          result = ((true, i), syntax)

proc parseNumeric(parser: var Parser, loc: Location) : Token =
  var base = 10
  let start = parser.pos - 1
  var i = start + 1
  case parser.buffer[i]
  of 'b':                       # not in thrift or protobuf
    base = 2
    inc(i)
  of 'o':
    base = 8
    inc(i)
  of 'x':
    base = 16
    inc(i)
  else:
    discard
  # consume stuff rather eagerly
  var c = parser.buffer[i]
  while c in HexChars or c in ['+', '-', '.']:
    inc(i)
    c = parser.buffer[i]
  parser.pos = i
  let s = substr(parser.buffer, start, i - 1)
  case base
  of 2:
    # parse binary thing
    result = newIntToken(s, s.parseBinInt(false), loc)
  of 8:
    result = newIntToken(s, s.parseOctInt(), loc)
  of 16:
    result = newIntToken(s, s.parseHexInt(), loc)
  else:
    # int or float
    try:
      result = newIntToken(s, s.parseInt(), loc)
    except ValueError:
      try:
        result = newFloatToken(s, s.parseFloat(), loc)
      except ValueError:
        result = newErrorToken(s, loc)

# must be var Parser to be mutable...
proc lexit(parser: var Parser): ParseStatus =
  var t : Token # = newErrorToken("", (0,0))
  var location: Location
  while parser.pos <= parser.buffer.len:
    location = parser.currentLocation()
    let r = parser.consumeChar()
    var s: string = $r
    case r:
    of ' ', '\l':
      while true:
        let nr = parser.nextChar()
        if not nr.isSpaceAscii():
          break
        discard parser.consumeChar()
      # t = Token(kind: tkWs, literal: s)
      continue
    of ';':
      t = Token(kind: tkSemi, literal: s, location: location)
    of ':':
      t = Token(kind: tkColon, literal: s, location: location)
    of ',':
      t = Token(kind: tkComma, literal: s, location: location)
    of '=':
      t = Token(kind: tkAssign, literal: s, location: location)
    of '(':
      t = Token(kind: tkLparen, literal: s, location: location)
    of ')':
      t = Token(kind: tkRparen, literal: s, location: location)
    of '{':
      t = Token(kind: tkLcurly, literal: s, location: location)
    of '}':
      t = Token(kind: tkRcurly, literal: s, location: location)
    of '[':
      t = Token(kind: tkLsquare, literal: s, location: location)
    of ']':
      t = Token(kind: tkRsquare, literal: s, location: location)
    of '<':
      t = Token(kind: tkLangle, literal: s, location: location)
    of '>':
      t = Token(kind: tkRangle, literal: s, location: location)
    of '*':
      t = Token(kind: tkStar, literal: s, location: location)
    of '&':
      t = Token(kind: tkRef, literal: s, location: location)
    of '"', '\'':
      # consume until unescaped endChar
      let endChar = r
      while true:
        let c = parser.consumeChar()
        s &= $c
        if c == '\\' and parser.nextChar() == endChar:
          s &= $parser.consumeChar()
        elif c == endChar:
          break
      t = newStringToken(s, location)
      # t = newStringToken(s, location)
    of '/':
      if parser.nextChar() == '*':
        s &= parser.consumeChar()
        # consume until '*/'
        while true:
          let c = parser.consumeChar()
          s &= c
          if c == '*' and parser.nextChar() == '/':
            s &= parser.consumeChar()
            break
        t = Token(kind: tkComment, literal: s)
      elif parser.nextChar() == '/':
        let i = parser.pos - 1
        var n = i
        while n < parser.buffer.len and parser.buffer[n] notin ['\l', '\0']: inc(n)
        s = substr(parser.buffer, i, n)
        parser.pos = n
        t = Token(kind: tkComment, literal: s, location: location)
      else:
        t = Token(kind: tkError, literal: s, location: location)
    of '-', '+':
      case parser.nextChar()
      of NumberChars:
        t = parser.parseNumeric(location)
      else:
        t = newErrorToken($r, location)
    of '.':
      case parser.nextChar():
      of NumberChars:
        t = parser.parseNumeric(location)
      of LowerChars, UpperChars:
        let i = parseWhile(parser.buffer, s, IdentifierChars, parser.pos - 1)
        inc(parser.pos, i - 1)
        t = newIdentifierToken(s, location)
      else:
        t = Token(kind: tkDot, literal: $r, location: location)
    of NumberChars:
      t = parser.parseNumeric(location)
    of LowerChars, '_':
      let i = parseWhile(parser.buffer, s, IdentifierChars, parser.pos - 1)
      inc(parser.pos, i - 1)
      t = newIdentifierToken(s, location)
    of UpperChars:
      # more or less like parseutils.parseWhile(...)
      # make sure we get the first character too
      let i = parser.pos - 1
      var n = i
      let m = parser.buffer.len
      # will always eat at least 1
      while n < m and parser.buffer[n] in IdentifierChars: inc(n)
      #inc(parser.pos, n - i - 1)
      parser.pos = n
      t = newIdentifierToken(substr(parser.buffer, i, n-1), location)
    of '\0':
      t = Token(kind: tkEof, literal: "", location: location)
    else:
      t = newErrorToken($r, location)
    #echo $t
    parser.tokens &= t
    if t in {tkEof, tkError}:
      # echo "done lexing: " & $t & ":'" & $r & "'"
      break

  # check for lexing errors, then process all tokens...
  let error = any(parser.tokens, proc (x: Token): bool = result = x == tkError)
  echo ">>" & $location & ":" $t
  result = ((not error, parser.tokens.len))
  
proc parse(parser: var Parser): ParseStatus =
  # Analyze "stream" of tokens
  var
    ps : ParseStatus
    cur = 0
  ps = parser.lexit()

  # check for lexing errors, then process all tokens...
  let error = any(parser.tokens, proc (x: Token): bool = result = x == tkError)
  echo "Has error:", $error, ", tokens:", $parser.tokens.len
  echo "Last token: " $parser.tokens[^1]

  while cur <= parser.tokens.high:
    # echo $cur
    let it = parser.tokens[cur]
    if it.kind == tkError:
      # quit on errors
      echo "ERROR:" & $it
      break
    elif it.isComment:
      # skip comments
      inc(cur)
      continue
    elif it === "syntax":
      var syntax: ProtobufSyntax
      (ps, syntax) = parseSyntax(parser.tokens[cur..^1])
      echo "syntax:" & $ps & "=" & $syntax
      if ps.status:
        # register syntax
        inc(cur, ps.consumed)
        continue
    elif it === "package":
      var pkgname: ProtobufPackage
      (ps, pkgname) = parsePackage(parser.tokens[cur..^1])
      echo "package:" & $ps & "=" & $pkgname
      if ps.status:
        # register package in a global for now...
        gCurrentPackage = pkgname
        inc(cur, ps.consumed)
        continue
    elif it === "option":
      ps = parseOption(parser.tokens[cur..^1])
      echo "option:" & $ps
      if ps.status:
        inc(cur, ps.consumed)
        continue
    elif it === "import":
      var imp: ProtobufImport
      (ps, imp) = parseImport(parser.tokens[cur..^1])
      echo "import:" & $ps & "-" & $imp
      if ps.status:
        # collect/register import
        if pkgimports.find(imp) < 0:
          pkgimports &= imp
        inc(cur, ps.consumed)
        continue
    elif it === "message":
      ps = parseMessage(parser.tokens[cur..^1])
      echo "message:" & $ps
      if ps.status:
        inc(cur, ps.consumed)
        continue
    elif it === "enum":
      ps = parseEnum(parser.tokens[cur..^1], "") # GPK? Global PoKemon, 8)
      echo "enum:" & $ps
      if ps.status:
        inc(cur, ps.consumed)
        continue
    elif it === "oneof":
      ps = parseOneof(parser.tokens[cur..^1])
      echo "oneof:" & $ps
      if ps.status:
        inc(cur, ps.consumed)
        continue
    elif it === "service":
      ps = parseService(parser.tokens[cur..^1])
      echo "service:" & $ps
      if ps.status:
        inc(cur, ps.consumed)
        continue
    elif it.isKeyword():
      echo "keyword:" & $it
    else:
      # just print it, it's an error...
      echo "?? " & $it
    inc(cur)

  echo "Line:" & $parser.line & ", pos:" & $parser.pos & ", col:" &
    $(parser.pos - parser.bol) & ":" & $parser.currentLocation()

  echo "ALL IMPORTS: " & $pkgimports.len & "->" & $pkgimports
  result = (status: true, consumed: cur)

  
import inflection  
import times
if isMainModule:
  var filename: string = "conformance.proto"
  var maxiter: int = 1
  if paramCount() > 0:
    filename = paramStr(1)
  if paramCount() > 1:
    maxiter = paramStr(2).parseInt
  let data = readFile(filename)
  if data.len > 0:
    # is it newSeq or initSeq?
    var parser = Parser(buffer: data, pos: 0, line: 1, bol: 0, tokens: newSeq[Token]())
    echo "dataLen:" & $parser.buffer.len
    var
      successful = 0
    let t1 = cpuTime()
    for _ in 1..maxiter:
      parser.reset
      let res = parser.parse
      if res.status:
        inc(successful)
    let elapsed = cpuTime() - t1
    writeLine stderr, "Successful:", $successful, ", elapsed ", $elapsed, ", ",  $(elapsed / maxiter.float)
  echo $GC_getStatistics()

