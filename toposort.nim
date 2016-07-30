import tables
import sequtils
import hashes
import algorithm
import macros
import typetraits

proc kahn_toposort*[T](t: TableRef[T, seq[T]]): seq[T] =
  result = newSeq[T]()
  var queue = newSeq[T]()
  var in_degree = newTable[T, int]()

  for k, v in t.pairs():
    in_degree[k] = 0
    for vv in v:
      in_degree[vv] = 0

  for k, v in t.pairs():
    for vv in v:
      in_degree[vv] += 1

  for u in in_degree.keys():
    if in_degree[u] == 0:
      queue.insert([u], pos=0)

  while queue.len > 0:
    let u = queue[0]
    queue.delete(0)
    result.add(u)
    if t.hasKey(u):
      for v in t[u]:
        in_degree[v] -= 1
        if in_degree[v] == 0:
          queue.insert([v], pos=0)

  # check for cycle
  # echo $result.len & ":" & $in_degree.len
  result.reverse()


if isMainModule:
  var deps = {
    # "1": @[ "2", "3", "4" ],
    # "2": @[ "5" ],
    # "3": @[ "4" , "6" ],
    # "5": @[ "8" ],
    # "6": @[ "7" ],
    # "7": @[ "4" ],
    # "8": @[ "7", "6" ]
    "5": @["11"],
    "11": @["2", "9", "10"],
    "7": @["11", "8"],
    "3": @["8", "10"],
    "8": @["9"],
  }.newTable

  let topo = deps.kahn_toposort()
  echo $topo
