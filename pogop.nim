  # pogop.nim
  # parsing completed, now try to process some of the info...
  # this is a bit POGO/Pokemon specific

  # better use a stdtmpl or something like that?!?
  # if only we had a plan for what to generate, 8)
  let ofilename = filename.changeFileExt("").replace(".", "").addFileExt("nim")
  echo "Writing to: " $ofilename
  var f = open(ofilename, fmWrite)
  defer: close(f)

  f.writeLine "# These could have been imports"
  for imp in pkgimports:
    f.writeLine "# import " & imp.path.changeFileExt("").replace(".", "")

  f.writeLine "{.push hint[XDeclaredButNotUsed]: off.}"
  f.writeLine "type"
  # enums first
  for en in gEnums:
    # {.pure.} ?
    var enn = en.name
    if not en.unique:
      enn = en.package.replace(".", "") & enn
    f.writeLine "\l  # " & en.package & ":" & en.name & ", unique:" & $en.unique
    let un = en.name.underscore(true) & "_"
    f.writeLine "  " & en.name & "* {.pure.} = enum #" & un
    discard enumstable.hasKeyOrPut(en.name, en)
    # NOTE: enum values must have proper order/be sorted in Nim it seems
    #
    # it may be better to make a Nim ProtobufEnum thingy or look at
    # the generated Python code and all the extra meta data for ideas
    var n: string
    for e, v in en.values:
      # replace potential common prefix, better to edit the proto file...
      if e.startsWith(un):
        n = e.replace(un, "")
      else:
        n = e
      # want a camelize style renaming like Python inflection
      f.writeLine "    " & n & " = (" & $v & ", \"" & camelize(n) & "\")"

  proc generateMsg(m: ProtobufMessage, f: File) =
    ## This does miss a few fields like nested enums and the like...
    f.writeLine "# Message: " & m.name & "[" & m.package & "]"
    f.writeLine "type " & m.name & "* = object"
    var ln: string = ""
    for v in m.fields.values:
      let dots = v.fieldtype.count('.')
      let en = v.fieldtype.split('.')[^1]
      var fn = v.fieldname
      if fn in NimKeywords:
        fn &= "_unnimmed"
      if dots > 2:
        ln = v.fieldtype.split(".")[^2..^1].join("")
      else:
        ln = v.fieldtype.replace(".", "")
      f.writeLine "  #  " & $v
      if pbuf2nimMapping.hasKey(v.fieldtype):
        let t = pbuf2nimMapping[v.fieldtype]
        if v.repeated:
          f.writeLine "  " & fn & ": seq[" & t & "]"
        else:
          f.writeLine "  " & fn & ": " & t
      elif enumstable.hasKey(ln):
        if v.repeated:
          f.writeLine "  " & fn & ": seq[" & ln & "] # repeated enum"
        else:
          f.writeLine "  " & fn & ": " & ln & " # enum"
      elif enumstable.hasKey(en):
        if v.repeated:
          f.writeLine "  " & fn & ": seq[" & en & "] # repeated enum"
        else:
          f.writeLine "  " & fn & ": " & en & " # enum"
      elif enumstable.hasKey(v.fieldtype):
        f.writeLine "  " & fn & ": " & v.fieldtype & " # enum, 8)"
      elif gMessages.hasKey(en):
        if v.repeated:
          f.writeLine "  " & fn & ": seq[" & en & "] # repeated message/struct, 8)"
        else:
          f.writeLine "  " & fn & ": " & en & " # message/struct, 8)"
      elif v.fieldtype.count(".") > 0:
        let enn = v.fieldtype.split('.')[^2..^1].join("")
        if enumstable.hasKey(enn):
          f.writeLine "  " & fn & ": " & enn & " # enum too, 8)"
        else:
          f.writeLine "  " & fn & ": " & enn & " # guessing, 8)"
      else:
        f.writeLine "  " & fn & ": " & en & " # unresolved"

  f.writeLine "# == message structures = " & $gMessages.len
  var
    deps = newTable[string, seq[string]]()
    generated = newSeq[string]()
  for k, v in gMessages:
    let d = v.depsList()
    if d.len > 0:
      discard deps.hasKeyOrPut(v.name, d)
  let topodeps = kahn_toposort(deps)
  echo "deps:" & $topodeps

  var
    unresolved = newSeq[string]()
    gencount = 0
  for d in topodeps:
    let sn = d.split(".")[^1]
    echo "# Checking: " & d
    if enumstable.hasKey(d) or enumstable.hasKey(sn):
      echo "# Skipping ENUM: " & d
    elif gMessages.hasKey(d):
      echo "# Message: " & d
      if generated.find(d) < 0 and generated.find(sn) < 0:
        inc(gencount)
        generateMsg(gMessages[d], f)
        generated &= d
        generated &= sn
    elif gMessages.hasKey(sn):
      echo "# Message: " & sn
      if generated.find(d) < 0 and generated.find(sn) < 0:
        inc(gencount)
        generateMsg(gMessages[sn], f)
        generated &= d
        generated &= sn
    else:
      echo "# Unresolved: " & d
      unresolved &= d

  f.writeLine "# Generated: " & $gencount
  f.writeLine "# We have: " & $unresolved.len & " unresolved items"
  for u in unresolved:
    f.writeLine "# " & u
