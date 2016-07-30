  # parsing completed, now try to process some of the info...
  # this is a bit POGO/Pokemon specific
  
  # better use a stdtmpl or something like that?!?
  # if only we had a plan for what to generate, 8)
  let ofilename = filename.changeFileExt("").replace(".", "").addFileExt("nim")
  echo "Writing to: " $ofilename
  var f = open(ofilename, fmWrite)
  defer: close(f)
  for imp in pkgimports:
    f.writeLine "# import " & imp.path.changeFileExt("").replace(".", "")
  f.writeLine "{.push hint[XDeclaredButNotUsed]: off.}"
  f.writeLine "type"
  for en in gEnums:
    # {.pure.} ?
    var enn = en.name
    if not en.unique:
      enn = en.package.replace(".", "") & enn
    f.writeLine "\l  # " & en.package & ":" & en.name & ", unique:" & $en.unique
    f.writeLine "  " & enn & " {.pure.} = enum"
    discard enumstable.hasKeyOrPut(enn, en)
    # NOTE: enum values must have proper order/be sorted in Nim
    # it may be better to make a Nim ProtobufEnum thingy
    for e, v in en.values.pairs:
      # want a camelize style renaming like Python inflection
      f.writeLine "    " & e & " = (" & $v & ", \"" & camelize(e) & "\")"

  proc generateMsg(m: ProtobufMessage, f: File) =
    f.writeLine "# Message: " & m.name & "[" & m.package & "]"
    f.writeLine "type " & m.name & " = object"
    for v in m.fields.values:
      let en = v.fieldtype.split('.')[^1]
      var fn = v.fieldname
      if fn in NimKeywords:
        fn &= "x"
      f.writeLine "  #  " & $v
      if pbuf2nimMapping.hasKey(v.fieldtype):
        let t = pbuf2nimMapping[v.fieldtype]
        if v.repeated:
          f.writeLine "  " & fn & ": seq[" & t & "]"
        else:
          f.writeLine "  " & fn & ": " & t
      elif enumstable.hasKey(en):
        f.writeLine "  " & fn & ": " & en & " # enum, 8)"
      elif gMessages.hasKey(en):
        f.writeLine "  " & fn & ": " & en & " # message/struct, 8)"
      elif v.fieldtype.count(".") > 0:
        let enn = v.fieldtype.split('.')[^2..^1].join("")
        if enumstable.hasKey(enn):
          f.writeLine "  " & fn & ": " & enn & " # enum too, 8)"


  # need to resolve the order of things unless there is some sort of forward declarations...
  # e.g. messages/struct with only native and enum fields or a proper tree'ish thing...
  # or by package...
  # well, a Kahn Toposort might be handy
  # or just process one file at a time and generate Nim imports
      
  f.writeLine "# == message structures = " & $gMessagesUnique.high
  var deps = newOrderedTable[string, seq[string]]()
  for m in gMessagesUnique:
    let mm = gMessages[m]
    if gReqRspMsgs.find(m) > -1:
      continue
    let d = mm.depsList()
    if d.len > 0:
      discard deps.hasKeyOrPut(mm.name, d)
    if d.len == 0 or mm.isNativeLike():
      f.writeLine "# structure: " & m
      generateMsg(mm, f)
  for m in gMessagesUnique:
    let mm = gMessages[m]
    let d = mm.depsList()
    if d.len > 0 or not isNativeLike(mm):
      f.writeLine "# structure: " & m
      generateMsg(mm, f)
  echo "deps:"
  for k,v in deps:
    echo $k & ":" & $v

  # These could probably match the RequestType enum... with an UPPER_CASE_UNDERSCORED_NAME
  # possibly these are services?!?
  # service SomeService {
  #    rpc get_hatched_eggs(GetHatchedEggsMessage) returns (GetHatchedEggsResponse);
  #    or
  #    rpc doit(RequestEnvelope) returns (ResponseEnvelope)
  # }
  var it = 0
  let requests = gReqRspMsgs.filter(proc(x: string): bool = x.endsWith("Message"))
  if requests.len > 0:
    f.writeLine "# reqs:" & $requests.len
    f.writeLine "type\l  pmMessageKind = enum"
  for m in requests:
    # do some inflection (borrowed from Python...)
    let urspname = m.replace("Message", "").underscore(uppercase = true)
    let rspname = m.replace("Message", "") & "Response"    
    f.writeLine "    pm" & m & " = (" & $it & ", \"" & m & "\") # " & urspname
    inc(it)
    let i = gReqRspMsgs.find(rspname)
    if i > -1:
      f.writeLine "    pm" & rspname & " = (" & $it & ", \"" & rspname & "\")"
      inc(it)
    else:
      f.writeLine "    # MISSING " & rspname & " = (" & $it & ", \"" & rspname & "\")"

  f.writeLine "# == message/responses = " & $gReqRspMsgs.high
  for m in requests:
    let rspname = m.replace("Message", "") & "Response"
    let mm = gMessages[m]
    generateMsg(mm, f)

    let i = gReqRspMsgs.find(rspname)
    if i > -1:
      let rm = gReqRspMsgs[i]
      # this should work if all is well
      let rmm = gMessages[rspname]
      generateMsg(rmm, f)
  f.writeLine "{.pop.}"
  
