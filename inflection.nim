# simple Inflection, 8)
import strutils
import pegs

proc ucfirst(m: int, n: int, c:openArray[string]): string =
  result = "" & c[0].toUpperAscii()

proc ucsecond(m: int, n: int, c:openArray[string]): string =
  result = "" & c[0] & c[1].toUpperAscii()

proc dasherize*(s: string): string {.inline.} =
  ## dasherize: replace underscores with dashes
  result = s.replace("_", "-")

proc camelize*(s: string, uppercase_first_letter: bool = true): string =
  ## Convert string to CamelCase
  ## e.g. "THIS_is_IT" -> "ThisIsIt" or "thisIsIt"
  if uppercase_first_letter:
    result = s.toLowerAscii().replace(peg"(^ / '_')+{.}", ucfirst)
  else:
    result = s[0].toLowerAscii() & camelize(s[1..^1])

proc underscore*(s: string, uppercase: bool = false): string =
  ## underscore: This-IsIt -> this_is_it or THIS_IS_IT
  let step1 = s.replacef(peg"{\upper}{\upper\lower}", "$1_$2")
  result = step1.replacef(peg"{(\lower / \d / '-')+}{\upper}", "$1_$2")
            .toLowerAscii()
            .replace("-", "_")
  if uppercase:
    result = result.toUpperAscii

proc humanize*(s: string): string =
  result = s.replacef(peg"_id$", "").replace("_", " ").toLowerAscii().replace(peg"^{\w}", ucfirst)

proc titleize*(s: string): string =
  result = ""
  var n = 0
  let step1 = s.replacef(peg"{\upper}{\upper\lower}", "$1_$2")
            .replacef(peg"{(\lower / \d)+}{\upper}", "$1_$2")
            .toLowerAscii()
            .replace("_", " ")
  for ss in step1.split(" "):
    if n != 0:
      result &= " "
    result &= ss.humanize().replace(peg"{'-'}{\lower}", ucsecond)
    inc(n)
    
proc ordinal*(number: int | float): string =
  let num = number.int.abs
  if num mod 100 in [11, 12, 13]:
    result = "th"
  else:
    let t = num mod 10
    case t
    of 1:
      result = "st"
    of 2:
      result = "nd"
    of 3:
      result = "rd"
    else:
      result = "th"

      
proc ordinalize*(number: int | float): string =
  result = $number & ordinal(number)

if isMainModule:
  
  for i in 0..256:
    echo ordinal(i) & ", " & ordinalize(i)
    echo ordinal(-i) & ", " & ordinalize(-i)
    let f = i.float + 0.33
    echo ordinal(f) & ", " & ordinalize(f)
    echo ordinal(-f) & ", " & ordinalize(-f)

  assert ordinalize(0) == "0th"
  assert ordinalize(1) == "1st"
  assert ordinalize(2) == "2nd"
  assert ordinalize(3) == "3rd"
  assert ordinalize(4) == "4th"
  assert ordinalize(111) == "111th"
  assert ordinalize(112) == "112th"
  assert ordinalize(113) == "113th"
  assert ordinalize(121) == "121st"
  assert ordinalize(122) == "122nd"
  assert ordinalize(123) == "123rd"
  assert ordinalize(124) == "124th", "oh crap"

  assert ordinalize(-123.3) == "-123.3rd"
  assert ordinalize(-124.3) == "-124.3th"

  for s in ["Product", "SpecialGuest", "ApplicationController", "Area51Controller"]:
    let us = underscore(s)
    let cc = camelize(us)
    echo s & " = " & us & " = " & cc
    assert s == cc

  for s in ["HTMLTidy", "HTMLTidyGenerator", "FreeBSD", "HTML"]:
    let us = underscore(s)
    let cc = camelize(us)
    echo s & " = " & us & " = " & cc

  for s in ["employee_salary", "employee_id", "underground"]:
    let hm = humanize(s)
    echo s & " = " & hm

  for s in ["nim is fun", "x-men: the last stand", "raiders_of_the_lost_ark", "TheManWithoutAPast"]:
    let t = titleize(s)
    echo s & " = " & t
