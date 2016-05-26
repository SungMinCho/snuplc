from random import *

def cut(n):
  if n < 0:
    return (0,0)
  a = randint(0, n)
  return (a, n-a)

def partition(num, n):
  ans = []
  for i in range(n):
    (x,num) = cut(num)
    ans.append(x)
  return ans

def indent(s):
  ss = s.split("\n")
  for i in range(len(ss)):
    ss[i] = "  " + ss[i]
  return "\n".join(ss)

class Symbol:
  def __init__(self, s):
    self.s = s
  def __str__(self):
    return self.s
  def __eq__(self, other):
    return self.s == other.s

PLUS = Symbol("+")
MINUS = Symbol("-")
MULT = Symbol("*")
DIV = Symbol("/")
POS = Symbol("+")
NEG = Symbol("-")
NOT = Symbol("!")
OR = Symbol("||")
AND = Symbol("&&")

EQ = Symbol("=")
NE = Symbol("#")
LT = Symbol("<")
LE = Symbol("<=")
GT = Symbol(">")
GE = Symbol(">=")

class Expr0:
  def __init__(self, simplexp):
    self.simplexp = simplexp
  def __str__(self):
    return self.simplexp.__str__()

class Expr1:
  def __init__(self, simplexp1, relOp, simplexp2):
    self.simplexp1 = simplexp1
    self.relOp = relOp
    self.simplexp2 = simplexp2
  def __str__(self):
    return self.simplexp1.__str__() + " " + self.relOp.__str__() + " " + self.simplexp2.__str__()

class Simplexp:
  def __init__(self, terms, termOps, unary=None):
    self.terms = terms
    self.termOps = termOps
    self.unary = unary
    assert(len(self.termOps) + 1 == len(self.terms))
  def __str__(self):
    res = ""
    if self.unary:
      res += self.unary.__str__() + " "
    res += self.terms[0].__str__()
    for (o, t) in list(zip(self.termOps, self.terms[1:])):
      res += " " + o.__str__() + " " + t.__str__()
    return res

class Term:
  def __init__(self, factors, factOps):
    self.factors = factors
    self.factOps = factOps
    assert(len(self.factOps) + 1 == len(self.factors))
  def __str__(self):
    res = self.factors[0].__str__()
    for (o, f) in list(zip(self.factOps, self.factors[1:])):
      res += " " + o.__str__() + " " + f.__str__()
    return res

# need to implement qualident

class FactorQual: # qualident
  def __init__(self, name, indices):
    self.name = name
    self.indices = indices
  def __str__(self):
    if len(self.indices) == 0:
      return self.name
    return self.name + "[" + "][".join([i.__str__() for i in self.indices]) + "]"

class FactorNum: # number
  def __init__(self, n):
    self.n = n
  def __str__(self):
    return str(self.n)

class FactorBool: # boolean
  def __init__(self, b):
    self.b = b
  def __str__(self):
    if self.b:
      return "true"
    else:
      return "false"

class FactorChar: # char
  def __init__(self, c):
    self.c = c
  def __str__(self):
    return "'" + self.c + "'"

class FactorStr: # str
  def __init__(self, s):
    self.string = s
  def __str__(self):
    return '"' + self.string + '"'

class FactorExp : # expression
  def __init__(self, e):
    self.e = e
  def __str__(self):
    return self.e.__str__()

class FactorCall: # call
  def __init__(self, name, args):
    self.name = name
    self.args = args
  def __str__(self):
    return self.name + "(" + ", ".join([a.__str__() for a in self.args]) + ")"

class FactorNot: # !
  def __init__(self, f):
    self.f = f
  def __str__(self):
    return "!" + self.f.__str__()

#########################################################################################################

class StatAssign():
  def __init__(self, q, e):
    self.q = q
    self.e = e
  def __str__(self):
    return self.q.__str__() + " := " + self.e.__str__()

class StatCall():
  def __init__(self, call):
    self.call = call
  def __str__(self):
    return self.call.__str__()

class StatIf():
  def __init__(self, cond, body1, body2):
    self.cond = cond
    self.body1 = body1
    self.body2 = body2
  def __str__(self):
    res = "if (" + self.cond.__str__() + ")\nthen"
    if len(self.body1) > 0:
      res += "\n"
    res += indent(";\n".join([b.__str__() for b in self.body1]))
    if len(self.body2) > 0:
      res += indent("\nelse\n" + ";\n".join([b.__str__() for b in self.body2]))
    res += "\nend"
    return res

class StatWhile():
  def __init__(self, cond, body):
    self.cond = cond
    self.body = body
  def __str__(self):
    res = "while (" + self.cond.__str__() + ")\ndo\n"
    res += indent(";\n".join([b.__str__() for b in self.body]))
    res += "\nend"
    return res

class StatReturn():
  def __init__(self, e=None):
    self.e = e
  def __str__(self):
    if self.e:
      return "return " + self.e.__str__()
    else:
      return "return"

#########################################################################################################

class Symtab:
  def __init__(self, parent=None):
    self.d = {}
    self.parent = parent

  def keyvalues(self): # return (k,v)s when k -> [v,v,v,v,v]
    for k in self.d:
      vs = self.d[k]
      for v in vs:
        yield (k,v)

  def fresh(self, typ):
    vs = []
    for k in self.d:
      vs += self.d[k]
    if self.parent:
      for k in self.parent.d:
        vs += self.parent.d[k]
    c = 0
    v = "v" + str(c)
    while v in vs:
      c += 1
      v = "v" + str(c)
    return v

  def add(self, typ, name):
    if typ not in self.d:
      self.d[typ] = []
    self.d[typ].append(name)

  def get(self, length, typ, scope=None): # return FactorQual
    if typ not in self.d:
      self.d[typ] = []
      v = self.fresh(typ)
      self.d[typ].append(v)
    if (typ == INT or typ == BOOL or typ == CHAR) and randint(0,1) == 0 and scope != None: # array indexing by half chance
      keys = list(self.d.keys())
      shuffle(keys)
      for t in keys:
        if t.isArray() and t.basetype == typ:
          v = choice(self.d[t])
          lens = partition(length, len(t.indices))
          return FactorQual(v, [scope.make_expression(lens[i], INT) for i in range(len(lens))])

    return FactorQual(choice(self.d[typ]), []) # just designator

#########################################################################################################

class Type:
  def __init__(self):
    pass
  def isArray(self):
    return False
  def __hash__(self):
    return hash(self.__dict__.values())
  def __ne__(self, other):
    return not (self==other)

class Basetype(Type):
  def __init__(self, typename):
    self.typename = typename
  def __str__(self):
    return self.typename
  def __hash__(self):
    return hash(self.typename)
  def __ne__(self, other):
    return not (self==other)
  def __eq__(self, other):
    if other == None:
      return False
    if self.typeclass() != other.typeclass():
      return False
    return self.typename == other.typename
  def typeclass(self):
    return 0

class Arraytype(Type):
  def __init__(self, basetype, indices):
    assert(len(indices) > 0)
    self.basetype = basetype
    self.indices = indices
  def isArray(self):
    return True
  def typeclass(self):
    return 1
  def __str__(self):
    return self.basetype.__str__() + "[" + "][".join([i.__str__() for i in self.indices]) + "]"
  def __hash__(self):
    return self.basetype.__hash__()
  def __ne__(self, other):
    return not (self==other)
  def __eq__(self, other):
    if other == None:
      return False
    if self.typeclass() != other.typeclass():
      return False
    if (not (self.basetype == other.basetype)):
      return False
    if (len(self.indices) != len(other.indices)):
      return False
    for i in range(len(self.indices)):
      if self.indices[i] != other.indices[i]:
        return False
    return True

class Pointertype(Type):
  def __init__(self, base):
    assert(base.isArray())
    self.base = base
  def __str__(self):
    return self.base.__str__()
  def typeclass(self):
    return 2
  def __hash__(self):
    return self.base.__hash__()
  def __ne__(self, other):
    return not (self==other)
  def __eq__(self, other):
    if other == None:
      return False
    if self.typeclass() != other.typeclass():
      return False
    return self.basetype == other.basetype

INT = Basetype("integer")
CHAR = Basetype("char")
BOOL = Basetype("boolean")
STRING = Basetype("string") # careful...

def randomBasetype():
  return choice([INT, CHAR, BOOL])

def randomBasetypeIncludingNone():
  return choice([INT, CHAR, BOOL, None])

def randomArgumentType():
  if randint(0,1) == 0:
    return randomBasetype()
  return Arraytype(randomBasetype(), [randint(0, 100) for x in range(5)])

def randomRelopAndType():
  r = choice([EQ, NE, LE, LT, GE, GT])
  if r == EQ or r == NE:
    return (r, randomBasetype())
  else:
    return (r, choice([CHAR, INT]))

#########################################################################################################

class Function:
  def __init__(self, name, arg_types, return_type, statnum, statlength, parent=None):
    self.name = name
    self.arg_types = arg_types
    self.return_type = return_type
    if parent:
      self.symtab = Symtab(parent.symtab)
      self.funcs = parent.funcs
    else:
      self.symtab = Symtab()
    self.arguments = []
    for t in arg_types:
      v = self.symtab.fresh(t)
      self.symtab.add(t, v)
      self.arguments.append((t, v))

    self.stats = []
    for i in range(statnum):
      self.stats.append(self.make_statement(statlength))

  def __str__(self):
    res = ""
    if self.return_type == None:
      res += "procedure " + self.name + "("
    else:
      res += "function " + self.name + "("

    args = []
    if len(self.arguments) > 0:
      (t0,v0) = self.arguments[0]
      args.append(v0)
      res += v0 + " : " + t0.__str__()
    for (t,v) in self.arguments[1:]:
      res += " ; "
      res += v + " : " + t.__str__()
      args.append(v)

    res += ")"
    if self.return_type:
      res += " : " + self.return_type.__str__()
    res += " ;\n"

    vardecl = ""
    for (t,v) in self.symtab.keyvalues():
      if v in args:
        continue
      vardecl += v + " : " + t.__str__() + ";\n"

    if len(vardecl) > 0:
      res += "var " + vardecl

    res += "begin\n"
    res += ";\n".join([s.__str__() for s in self.stats])
    res += "\nend " + self.name + ";\n"

    return res

  def make_call(self, length, rettype):
    funcs = self.funcs
    pick = list(range(len(funcs)))
    shuffle(pick)
    toCall = None
    for i in pick:
      f = funcs[i]
      if f.return_type == rettype:
        toCall = f
        break
    if toCall == None:
      print('no function of return type :', rettype)
      assert(False)
    lens = partition(length, len(toCall.arg_types))
    args = []
    for i in range(len(lens)):
      args.append(self.make_expression(lens[i], toCall.arg_types[i]))
    return FactorCall(toCall.name, args)


    return FactorCall("TODO", []) # temporary
    if rettype == None:
      pass
    else:
      pass

  def make_qual(self, length, typ):
    return self.symtab.get(length, typ, self)

  def make_factor(self, length, typ):
    if typ == INT:
      if length <= 0:
        return FactorNum(randint(0, 100000))
      elif randint(0,1) == 0:
        return FactorExp(self.make_expression(length-1, typ))
      else:
        return self.make_call(length, typ)
    elif typ == BOOL:
      if length <= 0:
        return FactorBool(choice([True, False]))
      elif randint(0,1) == 0:
        return FactorExp(self.make_expression(length-1, typ))
      else:
        return self.make_call(length, typ)
    elif typ == CHAR:
      if length <= 0:
        return FactorChar(choice(['a','b','c','d']))
      elif randint(0,1) == 0:
        return FactorExp(self.make_expression(length-1, typ))
      else:
        return self.make_call(length, typ)
    elif typ == STRING:
      return FactorStr(choice(["hello\\n", "bye\\n"]))
    else:
      return self.make_qual(length, typ)

  def make_term(self, length, typ):
    if length <= 0 or (not (typ == BOOL or typ == INT)):
      return Term([self.make_factor(length, typ)], [])
    (x, l) = cut(length)
    factors = [self.make_factor(x, typ)]
    factOps = []
    while l > 0:
      (x, l) = cut(l)
      factors.append(self.make_factor(x-1, typ))
      if typ == BOOL:
        factOps.append(AND)
      else:
        assert(typ == INT)
        factOps.append(choice([MULT, DIV]))
    return Term(factors, factOps)

  def make_simplexpr(self, length, typ):
    unary = None
    if randint(0, 1) == 1 and typ == INT:
      unary = choice([POS, NEG])
      length -= 1

    if length <= 0 or (not (typ == BOOL or typ == INT)):
      return Simplexp([self.make_term(length, typ)], [], unary)
    else:
      (x, l) = cut(length)
      terms = [self.make_term(x, typ)]
      termOps = []
      while l > 0:
        (x, l) = cut(l)
        terms.append(self.make_term(x-1, typ))
        if typ == BOOL:
          termOps.append(OR)
        else:
          assert(typ == INT)
          termOps.append(choice([PLUS, MINUS]))
      return Simplexp(terms, termOps, unary)

  def make_expression(self, length, typ):
    r = randint(0, 1)
    if r == 0 or (not (typ == BOOL)):
      return Expr0(self.make_simplexpr(length, typ))
    else:
      (r,t) = randomRelopAndType()
      (len1, len2) = cut(length)
      return Expr1(self.make_simplexpr(len1, t), r, self.make_simplexpr(len2, t))

  def make_statement(self, statlength):
    r = randint(0, 4)
    if r == 0:
      (len1,len2) = cut(statlength-1)
      t = randomBasetype()
      return StatAssign(self.make_qual(len1, t), self.make_expression(len2, t))
    elif r == 1:
      return StatCall(self.make_call(statlength, randomBasetypeIncludingNone()))
    elif r == 2:
      (len2, len3) = cut(statlength-1)
      (len1, len2) = cut(len2)
      body1 = []
      l = len2
      while l > 0:
        (x, l) = cut(l)
        body1.append(self.make_statement(x))

      body2 = []
      l = len3
      while l > 0:
        (x, l) = cut(l)
        body1.append(self.make_statement(x))
      return StatIf(self.make_expression(len1, BOOL), body1, body2)
    elif r == 3:
      (len1, len2) = cut(statlength-1)
      body = []
      l = len2
      l = len2
      while l > 0:
        (x, l) = cut(l)
        body.append(self.make_statement(x))
      return StatWhile(self.make_expression(len1, BOOL), body)
    else:
      if self.return_type == None:
        return StatReturn()
      else:
        return StatReturn(self.make_expression(statlength-1, self.return_type))

class Module(Function):
  def __init__(self, name, funcnum, statnum, statlength):
    (sn1, sn2) = cut(statnum)
    self.symtab = Symtab()
    self.funcs = []
    self.funcs.append(Function("ReadInt", [], INT, 0, 0, self))
    self.funcs.append(Function("WriteInt", [INT], None, 0, 0, self))
    self.funcs.append(Function("WriteChar", [CHAR], None, 0, 0, self))
    self.funcs.append(Function("WriteStr", [STRING], None, 0, 0, self))
    self.funcs.append(Function("WriteLn", [], None, 0, 0, self))

    self.IOFuncNum = len(self.funcs)

    self.funcs.append(Function("dummyINTfunc", [], INT, 0, 0, self))
    self.funcs.append(Function("dummyCHARfunc", [], CHAR, 0, 0, self))
    self.funcs.append(Function("dummyBOOLfunc", [], BOOL, 0, 0, self))
    self.funcs.append(Function("dummyProcedure", [], None, 0, 0, self))

    Function.__init__(self, name, [], None, sn1, statlength)
    for i in range(funcnum):
      self.funcs.append(Function(self.fresh_function_name(), [randomArgumentType() for x in range(randint(0, 4))],
      randomBasetypeIncludingNone(), statnum, statlength, self)) # argtypes currently []

    for i in range(statnum):
      self.stats.append(self.make_statement(statlength))

  def fresh_function_name(self):
    def valid(name):
      for f in self.funcs:
        if name == f.name:
          return False
      return True
    cnt = 0
    name = "f" + str(cnt)
    while not valid(name):
      cnt += 1
      name = "f" + str(cnt)
    return name

  def __str__(self):
    res = "module " + self.name + ";\n"

    vardecl = ""
    for (t,v) in self.symtab.keyvalues():
      res += v + " : " + t.__str__() + ";\n"

    if len(vardecl) > 0:
      res += "var " + vardecl

    for f in self.funcs[self.IOFuncNum:]:
      res += f.__str__() + "\n"

    res += "begin\n"
    res += ";\n".join([s.__str__() for s in self.stats])
    res += "\nend " + self.name + ".\n"
    return res

def main():
  #f = Function("f", [], None, 10, 200)
  m = Module("test", 5, 10, 30)
  print(m)


if __name__ == "__main__":
  main()
