import s-exp as S

#| 0. Write your name and OU ID (the part before the 
   "@" in your email address) below:
  
   NAME: Mark Dreitzler
   ID: md578017
|#

#|
   Scheme0 BNF grammar:
   
   Unary Operators
   u ::== neg           # negate a number (not a boolean)
   
   Binary Operators
   b ::== +             # add two numbers
        | -             # subtract two numbers
        | *             # multiply two numbers
        | /             # divide two numbers
        | and           # boolean conjunction
        | or            # boolean disjunction
   
   Values: 
   v ::== true
        | false
        | n             # a number
 
   Expressions:
   e ::== v               # a value
        | x               # an identifier
        | (u e)           # unary op u applied to expression e
        | (b e1 e2)       # binary op b applies to e1, e2
        | (cond e1 e2 e3) # if e1 then e2, else e3
        | (let x e2 e3)   # let x = the value of e2 in e3 (in which x may appear free) 
|#

#| Scheme0 Abstract Syntax |#

data Unop: 
  | neg
end

data Binop:
  | add
  | sub
  | mul
  | div
  | conj
  | disj
end

data Val:
  | bool(b :: Boolean)
  | num(n :: Number)
end

data Exp:
  | val(v :: Val)
  | id(s :: String)
  | unexp(op :: Unop, e :: Exp)
  | binexp(op :: Binop, e1 :: Exp, e2 :: Exp)  
  | cond(e1 :: Exp, e2 :: Exp, e3 :: Exp)
  | letx(s :: String, e1 :: Exp, e2 :: Exp)
end

#| END Scheme0 Abstract Syntax |#

# Example expressions
ex1 = val(num(3))
ex2 = val(num(100))
ex3 = val(bool(true))
ex4 = val(bool(false))
ex5 = binexp(add, ex1, ex2)
ex6 = binexp(sub, ex1, ex2)
ex7 = binexp(mul, ex1, ex2)
ex8 = binexp(div, ex1, ex2)
ex9 = binexp(mul, ex8, ex8)
ex10 = binexp(mul, ex7, ex5)
ex11 = binexp(add, id("x"), id("y"))
ex12 = binexp(conj, ex3, ex4)
ex13 = binexp(disj, ex3, ex4)
ex20 = unexp(neg, val(num(100)))
ex21 = unexp(neg, (binexp(sub, val(num(5)), val(num(5)))))
ex30 = letx("x", val(num(3)), id("x"))
ex31 = letx("yzw", binexp(add, val(num(4)), id("x")), id("x"))
ex32 = letx("x", binexp(add, val(num(4)), binexp(add, id("x"), id("x"))), 
  binexp(mul, id("x"), id("y")))
ex33 = cond(val(bool(true)), val(num(5)), id("x"))
ex34 = cond(val(bool(false)), val(num(5)), id("x"))
ex35 = letx("x", val(num(6)), cond(val(bool(false)), val(num(5)), id("x")))

# Type synonym for the s-expression type.
type Sexp = S.S-Exp

# Convert an s-expression to a Scheme0 expression.
fun parseExp(s :: Sexp) -> Exp:
  cases (Sexp) s:
    |s-num(n) => #if it's a number, return it
      val(num(n))
    |s-sym(sym) => #if it's a symbol or a string, return the appropriate result (other types of symbols will be done later and there won't be any calls to this function for symbols.)
      if sym == "true":
        val(bool(true))
      else if sym == "false":
        val(bool(false))
      else:
        id(sym)
      end
    |s-str(str) =>
      id(str)
    |s-list(l) => #if it's a list, call parseList
      parseList(l)
  end
end

#parseList contains all the stuff you need to parse lists.
fun parseList(s :: List<S.S-exp>) -> Exp:
  cases(List<S.S-exp>) s:
    |empty => raise("") #empty lists return an error
    |link(f,r) =>
      cases(Sexp) f: #check the first value in the list
        |s-num(n) => raise("") #anything but a symbol here will return an error
        |s-str(str) => raise("")
        |s-list(l) => raise("")
        |s-sym(sym) => #this is where every possible type is checked
          #note: I originally tried the following with a string of or operators and got errors. Hence why each of these has its own line.
          if sym == "+": #checks every possible binop to check if list is binop
            parseBinop(s)
          else if sym == "-": #checks every possible binop to check if list is binop
            parseBinop(s)
          else if sym == "*": #checks every possible binop to check if list is binop
            parseBinop(s)
          else if sym == "/": #checks every possible binop to check if list is binop
            parseBinop(s)
          else if sym == "and": #checks every possible binop to check if list is binop
            parseBinop(s)
          else if sym == "or": #checks every possible binop to check if list is binop
            parseBinop(s)
          else if sym == "neg": #checks if list is unop
            parseUnop(s)
          else if sym == "let": #checks if list is letx
            parseLetx(r)
          else if sym == "cond": #checks if list is conditional
            parseCond(r)
          else:
            raise("")
          end
      end
  end
end

#parseBinop returns the input in Binop form.
fun parseBinop(s :: List<S.S-exp>) -> Exp:
  cases(List<S.S-exp>) s:
    |link(f,r) =>
      if s.length() == 3: #make sure the length is correct
        #move into the checks for each of the symbols
        if f == S.s-sym("+"):
          binexp(add, parseExp(r.first), parseExp(r.rest.first))
        else if f == S.s-sym("-"):
          binexp(sub, parseExp(r.first), parseExp(r.rest.first))
        else if f == S.s-sym("*"):
          binexp(mul, parseExp(r.first), parseExp(r.rest.first))
        else if f == S.s-sym("/"):
          binexp(div, parseExp(r.first), parseExp(r.rest.first))
        else if f == S.s-sym("and"):
          binexp(conj, parseExp(r.first), parseExp(r.rest.first))
        else if f == S.s-sym("or"):
          binexp(disj, parseExp(r.first), parseExp(r.rest.first))
        else:
          raise("") #this check is just to make sure that this function wasn't called accidentally, and provides an extra level of error handling.
        end
      else:
          raise("") #raise error if length is incorrect
      end
  end
end  

#parseUnop returns the input in Unop form
fun parseUnop(s :: List<S.S-exp>) -> Exp:
  cases(List<S.S-exp>) s:
    |link(f,r) =>
      if s.length() == 2: #make sure the length is correct
        unexp(neg, parseExp(r.first))
      else:
          raise("") #raise error if length is incorrect
      end
  end
end

#parseLetx returns the input in Letx form
fun parseLetx(s :: List<S.S-exp>) -> Exp:
  cases(List<S.S-exp>) s:
    |link(f,r) =>
      if s.length() == 3: #make sure the length is correct
        letx(letxFixer(f), parseExp(r.first), parseExp(r.rest.first))
      else:
          raise("") #raise error if length is incorrect
      end
  end
end

#parseCond returns the input in conditional form
fun parseCond(s :: List<S.S-exp>) -> Exp:
  cases(List<S.S-exp>) s:
    |link(f,r) =>
      if s.length() == 3: #make sure the length is correct
        cond(parseExp(f), parseExp(r.first), parseExp(r.rest.first))
      else:
          raise("") #raise error if length is incorrect
      end
  end
end

#letxFixer returns strings given s-strings.
fun letxFixer(s :: Sexp) -> String:
  cases(Sexp) s:
    |empty => raise("")
    |s-sym(m) =>
      m
  end
end

# The overall parser is the composition of parseExp with S.read-s-exp.
fun parse(s :: String) -> Exp:
  parseExp(S.read-s-exp(s))
end

#| In this assignment, you will implement a Scheme0 interpreter.
   
   That is, define a function `interp` that takes an initial state
   (mapping identifiers to their possible values) and an expression,
   and returns the result of evaluating that expression to a value. As
   in the parser assignment, you may find it helpful to break the
   problem down into smaller functions. For example, you might define
   one function for interpreting binary expressions, one for
   conditional expressions, and so on.
|#

# An environment is a function from identifiers (strings) to values.
type Env = (String -> Val)

# The initial environment contains no bindings.
init-env = lam(x :: String): raise(x + " is unbound") end

# Look up the value bound to an identifier in an environment.
fun lookup(env :: Env, x :: String) -> Val:
  env(x)
end

# Update an environment with a new binding.
fun upd(s :: Env, x :: String, new-val :: Val) -> Env:
  lam(y :: String):
    if string-equal(x, y): new-val
    else: s(y)
    end
  end
end

# Evaluate an expression under a given environment, producing a value.
fun interp(s :: Env, e :: Exp) -> Val:
  cases (Exp) e:
    |val(v) =>
      v
    |unexp(x,y) =>
      interpUnexp(x,y,s)
    |binexp(a,b,c) =>
      interpBinexp(a,b,c,s)
    |cond(d,f,g) =>
      interpCond(d,f,g,s)
     |id(l) =>
      interpId(l, s)
    |letx(i,j,k) =>
      interpLetx(i,j,k,s)
  end
end

#interprets the unary expressions
fun interpUnexp(x :: Unop, y :: Exp, s :: Env) -> Val:
  cases (Exp) y:
    |val(v) =>
      cases (Val) v:
        |num(z) =>
          num(0 - z)
      end
    |binexp(a,b,c) =>
      num(0 - valFixer(interp(s, y)))
  end
end

#interprets binary expressions
fun interpBinexp(x :: Binop, y :: Exp, z :: Exp, s :: Env) -> Val:
  cases (Binop) x:
    |add =>
      num(valFixer(interp(s, y)) + valFixer(interp(s, z)))
    |sub =>
      num(valFixer(interp(s, y)) - valFixer(interp(s, z)))
    |mul =>
      num(valFixer(interp(s, y)) * valFixer(interp(s, z)))
    |div =>
      num(valFixer(interp(s, y)) / valFixer(interp(s, z)))
    |conj =>
      if (interp(s, y)) == bool(true):
        if (interp(s, z)) == bool(true):
          bool(true)
        else:
          bool(false)
        end
      else:
        bool(false)
      end
    |disj =>
      if (interp(s, y)) == bool(true):
        bool(true)
      else if (interp(s, z)) == bool(true):
        bool(true)
      else:
        bool(false)
      end
  end
end

#interprets conditional expressions
fun interpCond(x :: Exp, y :: Exp, z :: Exp, s :: Env) -> Val:
  if interp(s, x) == bool(true):
    interp(s, y)
  else:
    interp(s, z)
  end
end

#interprets the id expressions
fun interpId(x :: String, s :: Env) -> Val:
  lookup(s, x)
end

#interprets the letX expressions
fun interpLetx(x :: String, y :: Exp, z :: Exp, s :: Env) -> Val:
  interp(upd(s,x,interp(s,y)),z)
end

   
#fixes numbers to integers instead of the num(x) format, allowing for them to be manipulated like regular integers
fun valFixer(x :: Val) -> Number:
  cases (Val) x:
    |num(z) =>
      z
  end
end

# End-to-end parser and interpreter.
fun run(s :: String) -> Val:
  interp(init-env, parse(s))
end

# These tests provide evidence that your interpreter is working properly.

check "interp(...)":
  interp(init-env, ex1) is num(3)
  interp(init-env, ex2) is num(100)
  interp(init-env, ex3) is bool(true)
  interp(init-env, ex4) is bool(false)
  interp(init-env, ex5) is num(103)
  interp(init-env, ex6) is num(-97)
  interp(init-env, ex7) is num(300)
  interp(init-env, ex8) is num(0.03)
  interp(init-env, ex9) is num(0.0009)
  interp(init-env, ex10) is num(30900)
  interp(init-env, ex11) raises ""
  interp(init-env, ex20) is num(-100)
  interp(init-env, ex21) is num(0)
  interp(init-env, ex30) is num(3)
  interp(init-env, ex31) raises ""
  interp(init-env, ex32) raises ""
  interp(init-env, ex33) is num(5)
  interp(init-env, ex35) is num(6)
end

# These tests provide evidence that your parser and interpreter are both working properly.

check "run(...)":
  run("3") is num(3)
  run("true") is bool(true)
  run("(* 3 4)") is num(12)
  run("(let x 3 4)") is num(4)
  run("(let x 3 (* x true))") raises ""
  run("(4 * 5)") raises ""
  run("(* true false)") raises ""
  run("(and true false)") is bool(false)
  run("(or (or true false) false)") is bool(true)
  run("(let x 5 (* x x))") is num(25)
  run("(LET x 5 (* x x))") raises ""
  run("(let x 4 (let x 5 (* x x)))") is num(25)
  run("(+ (let x 0 (+ x x)) 0)") is num(0)
  run("(/ (let x 0 0) (let x 0 x))") raises ""
  run("(let x (let x (+ 3 4) x) (* x x))") is num(49)
  run("(* x 4)") raises ""
  #more tests to be added here
end
