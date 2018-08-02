from z3 import *
from termcolor import colored


# This function takes:
#   - list of generators (t1/t2/g/h/i) mfuns1 for the first polynomial interpretation
#   - list of generators (t1a/t2a/g/h/i) mfuns1 for the second polynomial interpretation
#   - vars1/vars2 are the free variables appearing the the first/scond polynomial interpretation equations
#   - funs, which are the polynomial interpertations of the string diagrams

def goalLex(mfuns1, vars1, mfuns2, vars2, funs):
  xs = [{
    "x1": u(*mfuns1)(*vars1), 
    "y1": v(*mfuns1)(*vars1),
    "x2": u(*mfuns2)(*vars2), 
    "y2": v(*mfuns2)(*vars2)
  } for (u,v) in funs]
  # x > 0, y > 0 ...
  var_bounds = And(*[ x > 0 for x in vars1 + vars2 ])
  # x1 > y1 \/ (x1 == y1 /\ x2 >= y2) ...
  nonstrict = [ Or(e['x1'] > e['y1'], And(e['x1'] == e['y1'], e['x2'] >= e['y2'])) for e in xs ]
  # x1 > y1 \/ (x1 == y1 /\ x2 > y2) ...
  strict = Or(*[ Or(e['x1'] > e['y1'], And(e['x1'] == e['y1'], e['x2'] > e['y2'])) for e in xs ])
  return And(var_bounds , Not(And(strict, *nonstrict)))


def runSolver(*args, **keywords):
	s = Solver()
	s.set(**keywords)
	s.add(*args)
	if keywords.get('show', False):
		print(s)
	r = s.check()
	if r == unsat:
		print(colored('VALID', 'green'))
	elif r == unknown:
		print(colored("failed to solve", 'red'))
		try:
			print(colored(s.model(), 'red'))
		except Z3Exception:
			return
	else:
		print(colored('INVALID', 'red'))
		print(colored(s.model(), 'red'))


x, y, z = Ints("x y z")
xa, ya, za = Ints("xa ya za")

#  x   y
#  |   |
#   \ /
#    X
#   / \
#  |   |
# x+y  x

# t1(x,y) = x+y
# t2(x,y) = x

t1 = Function('t1', IntSort(), IntSort(), IntSort())
t1_def = ForAll([x, y], t1(x, y) == x+y)
t2 = Function('t2', IntSort(), IntSort(), IntSort())
t2_def = ForAll([x, y], t2(x, y) == x)



#  x   y
#  |   |
#   \ /
#    X
#   / \
#  |   |
# x+2y x

t1a = Function('t1a', IntSort(), IntSort(), IntSort())
t1a_def = ForAll([x, y], t1a(x, y) == x+2*y) # x+2*y
t2a = Function('t2a', IntSort(), IntSort(), IntSort())
t2a_def = ForAll([x, y], t2a(x, y) == x)



# *
# |
# 1

g = Function('g', IntSort())
g_def = g() == 1


#  x   y
#  |   |
#   \ /
#    Y
#    |
#  2x+y

# h(x,y) = 2x+y

h = Function('h', IntSort(), IntSort(), IntSort())
h_def = ForAll([x, y], h(x, y) == 2*x+y)



#  x
#  |
# <*>
#  |
# x+1

# i(x) = x+1


i = Function('i', IntSort(), IntSort())
i_def = ForAll([x], i(x) == x+1)






print("""
         x          x
     *   |          |
     |   |          |   *
      \ /           |   |
       X     --->   |   |
      / \\           |   |
     |   |          x  g()
     | t2(g(),x)
t1(g(),x)
""")

r1_u1 = lambda t1,t2,g: lambda x,y: t1(g(),x)
r1_u2 = lambda t1,t2,g: lambda x,y: x

r1_v1 = lambda t1,t2,g: lambda x,y: t2(g(),x)
r1_v2 = lambda t1,t2,g: lambda x,y: g()

runSolver(t1_def, t2_def, t1a_def, t2a_def, g_def,
  goalLex([t1,t2,g], [x,y], [t1a,t2a,g], [xa,ya], [(r1_u1,r1_u2), (r1_v1,r1_v2)]))



print("""
     x                  x
     |   *              |
     |   |          *   |
      \ /           |   |
       X     --->   |   |
      / \\           |   |
     |   |         g()  x
     | t2(x, g())
t1(x, g())
""")

r2_u1 = lambda t1,t2,g: lambda x,y: t1(x, g())
r2_u2 = lambda t1,t2,g: lambda x,y: g()

r2_v1 = lambda t1,t2,g: lambda x,y: t2(x, g())
r2_v2 = lambda t1,t2,g: lambda x,y: x

runSolver(t1_def, t2_def, t1a_def, t2a_def, g_def,
  goalLex([t1,t2,g], [x,y], [t1a,t2a,g], [xa,ya], [(r2_u1,r2_u2), (r2_v1,r2_v2)]))



print("""
     x   y          x   y
     |   |          |   |
      \ /           |   |
       X     --->   |   |
      / \\           |   |
      \ /           |   |
       X            |   |
      / \\           x   y
     |   |
     | t2(t1(x,y), t2(x,y))
t1(t1(x,y), t2(x,y))
""")

r3_u1 = lambda t1,t2: lambda x,y: t1(t1(x,y), t2(x,y))
r3_u2 = lambda t1,t2: lambda x,y: x

r3_v1 = lambda t1,t2: lambda x,y: t2(t1(x,y), t2(x,y))
r3_v2 = lambda t1,t2: lambda x,y: y

runSolver(t1_def, t2_def, t1a_def, t2a_def,
  goalLex([t1,t2], [x,y], [t1a,t2a], [xa,ya], [(r3_u1,r3_u2), (r3_v1,r3_v2)]))



print("""
         x          x
     *   |          |
     |   |          |
      \ /          <*>
       |     --->   |
       |            |
   h(g(),x)        i(x)
""")

r4_u1 = lambda g,h,i: lambda x: h(g(),x)
r4_u2 = lambda g,h,i: lambda x: i(x)

runSolver(g_def, h_def, i_def,
  goalLex([g,h,i], [x], [g,h,i], [xa], [(r4_u1,r4_u2)]))



print("""
     x              x
     |   *          |
     |   |          |
      \ /           |
       |     --->   |
       |            |
   h(x,g())         x
""")

r5_u1 = lambda g,h: lambda x: h(x,g())
r5_u2 = lambda g,h: lambda x: x

runSolver(g_def, h_def,
  goalLex([g,h], [x], [g,h], [xa], [(r5_u1,r5_u2)]))



print("""
     x   y          x   y
     |   |          |   |
      \ /            \ /
       X              |
      / \\             |
     |   |   --->     |
      \ /             |
       |            h(x,y)
h(t1(x,y),t2(x,y))
""")

r6_u1 = lambda t1,t2,h: lambda x,y: h(t1(x,y),t2(x,y))
r6_u2 = lambda t1,t2,h: lambda x,y: h(x,y)

runSolver(t1_def, t2_def, t1a_def, t2a_def, h_def,
  goalLex([t1,t2,h], [x,y], [t1a,t2a,h], [xa,ya], [(r6_u1,r6_u2)]))



print("""
     x   y   z                      x   y   z
     |   |   |                      |   |   |
      \ /    |                      |    \ /
       |     |          --->        |     |
        \\   /                        \\   /
         \ /                          \ /
          |                            |
     h(h(x,y),z)                  h(x,h(y,z))
""")

r7_u1 = lambda h: lambda x,y,z: h(h(x,y),z)
r7_u2 = lambda h: lambda x,y,z: h(x,h(y,z))

runSolver(h_def,
  goalLex([h], [x,y,z], [h], [xa,ya,za], [(r7_u1,r7_u2)]))



print("""
     x          x
     |          |
    <*>         |
     |   --->  <*>
    <*>         |
     |          |
  i(i(x))     i(x)
""")

r8_u1 = lambda i: lambda x: i(i(x))
r8_u2 = lambda i: lambda x: i(x)

runSolver(i_def,
  goalLex([i], [x], [i], [xa], [(r8_u1,r8_u2)]))



print("""
     x   y          x   y
     |   |          |   |
     |  <*>          \ /
     |   |    --->    |
      \ /             |
       |              |
   h(x,i(y))       h(x,y)
""")

r9_u1 = lambda h,i: lambda x,y: h(x,i(y))
r9_u2 = lambda h,i: lambda x,y: h(x,y)

runSolver(h_def, i_def,
  goalLex([h,i], [x,y], [h,i], [xa,ya], [(r9_u1,r9_u2)]))



print("""
     x   y          x   y
     |   |          |   |
    <*>  |           \ /
     |   |    --->    |
      \ /            <*>
       |              |
   h(i(x),y)       i(h(x,y))
""")

r10_u1 = lambda h,i: lambda x,y: h(i(x),y)
r10_u2 = lambda h,i: lambda x,y: i(h(x,y))

runSolver(h_def, i_def,
  goalLex([h,i], [x,y], [h,i], [xa,ya], [(r10_u1,r10_u2)]))



print("""
     *        *
     |  --->  |
    <*>       |
     |       g()
  i(g())
""")

r11_u1 = lambda g,i: lambda: i(g())
r11_u2 = lambda g,i: lambda: g()

runSolver(g_def, i_def,
  goalLex([g,i], [], [g,i], [], [(r11_u1,r11_u2)]))



print("""
     x   y                 x   y
     |   |                 |   |
    <*>  |                  \ / 
      \ /                    X
       X         --->       / \\
      / \\                  |  <*>
     |   |                 |   |
     | t2(i(x),y)          | i(t2(x,y))
t1(i(x),y)              t1(x,y)
""")


r12_u1 = lambda t1,t2,i: lambda x,y: t1(i(x),y)
r12_u2 = lambda t1,t2,i: lambda x,y: t1(x,y)

r12_v1 = lambda t1,t2,i: lambda x,y: t2(i(x),y)
r12_v2 = lambda t1,t2,i: lambda x,y: i(t2(x,y))

runSolver(t1_def, t2_def, t1a_def, t2a_def, i_def, 
  goalLex([t1,t2,i], [x,y], [t1a,t2a,i], [xa,ya], [(r12_u1,r12_u2), (r12_v1,r12_v2)]))



print("""
     x   y                 x   y
     |   |                 |   |
     |  <*>                 \ / 
      \ /                    X
       X         --->       / \\
      / \\                 <*>  |
     |   |                 |   |
     | t2(x,i(y))          | t2(x,y)
t1(x,i(y))              i(t1(x,y))
""")

r13_u1 = lambda t1,t2,i: lambda x,y: t1(x,i(y))
r13_u2 = lambda t1,t2,i: lambda x,y: i(t1(x,y))

r13_v1 = lambda t1,t2,i: lambda x,y: t2(x,i(y))
r13_v2 = lambda t1,t2,i: lambda x,y: t2(x,y)

runSolver(t1_def, t2_def, t1a_def, t2a_def, i_def, 
  goalLex([t1,t2,i], [x,y], [t1a,t2a,i], [xa,ya], [(r13_u1,r13_u2), (r13_v1,r13_v2)]))



print("""
     x   y   z                      x   y   z
     |   |   |                      |   |   |
      \ /    |                      |    \ /
       X     |          --->        |     X
      / \\   /                        \\   / \\
     |    X                            X    |
      \ /   \\                        /   \ /
       X     |                      |     X
      / \\    |                      |    / \\
     |   | t2(t2(x,y),z)            |   | t2(t2(x,t1(y,z)),t2(y,z))
     | t2(t1(x,y),t1(t2(x,y),z))    | t1(t2(x,t1(y,z)),t2(y,z))
t1(t1(x,y),t1(t2(x,y),z))          t1(x,t1(y,z))   
""")

r14_u1 = lambda t1,t2: lambda x,y,z: t1(t1(x,y),t1(t2(x,y),z))
r14_u2 = lambda t1,t2: lambda x,y,z: t1(x,t1(y,z))

r14_v1 = lambda t1,t2: lambda x,y,z: t2(t1(x,y),t1(t2(x,y),z))
r14_v2 = lambda t1,t2: lambda x,y,z: t1(t2(x,t1(y,z)),t2(y,z))

r14_w1 = lambda t1,t2: lambda x,y,z: t2(t2(x,y),z) 
r14_w2 = lambda t1,t2: lambda x,y,z: t2(t2(x,t1(y,z)),t2(y,z))

runSolver(t1_def, t2_def, t1a_def, t2a_def, 
  goalLex([t1,t2], [x,y,z], [t1a,t2a], [xa,ya,za], [(r14_u1,r14_u2), (r14_v1,r14_v2), (r14_w1,r14_w2)]))



print("""
     x   y   z                      x   y   z
     |   |   |                      |   |   |
      \ /    |                      |    \ /
       X     |          --->        |     |
      / \\   /                        \\   /
     |   \ /                          \ /
     |    |                            |
     |   /                             |
      \ /                              |   
       |                               |
h(t1(x,y),h(t2(x,y),z))           h(x,h(y,z))    
""")

r15_u1 = lambda t1,t2,h: lambda x,y,z: h(t1(x,y),h(t2(x,y),z))
r15_u2 = lambda t1,t2,h: lambda x,y,z: h(x,h(y,z))

runSolver(t1_def, t2_def, t1a_def, t2a_def, h_def,
  goalLex([t1,t2,h], [x,y,z], [t1a,t2a,h], [xa,ya,za], [(r15_u1,r15_u2)]))



print("""
     x   y   z                      x   y   z
     |   |   |                      |   |   |
      \ /    |                      |    \ /
       X     |          --->        |     X
      / \\   /                        \\   / \\
     |   \ /                          \ /   |
     |    |                            |    |
      \  /                             |    |
        X                              |    |
       / \\                             |    |
      |   |                            |    |
      | t2(t1(x,y),h(t2(x,y),z))       | t2(y,z)
t1(t1(x,y),h(t2(x,y),z))           h(x,t1(y,z))
""")

r16_u1 = lambda t1,t2,h: lambda x,y,z: t1(t1(x,y),h(t2(x,y),z))
r16_u2 = lambda t1,t2,h: lambda x,y,z: h(x,t1(y,z))

r16_v1 = lambda t1,t2,h: lambda x,y,z: t2(t1(x,y),h(t2(x,y),z))
r16_v2 = lambda t1,t2,h: lambda x,y,z: t2(y,z)

runSolver(t1_def, t2_def, t1a_def, t2a_def, h_def,
  goalLex([t1,t2,h], [x,y,z], [t1a,t2a,h], [xa,ya,za], [(r16_u1,r16_u2), (r16_v1,r16_v2)]))



print("""
     x   y   z                      x   y   z
     |   |   |                      |   |   |
      \ /    |                      |    \ /
       |     |          --->        |     X
        \\   /                        \\   / \\
         \ /                          \ /   |
          X                            X    |
         / \\                          / \\   |
        |   |                        |   \ /
        | t2(h(x,y),z)               |    |
   t1(h(x,y),z)                      | h(t2(x,t1(y,z)),t2(y,z))
                               t1(x,t1(y,z))
""")

r17_u1 = lambda t1,t2,h: lambda x,y,z: t1(h(x,y),z)
r17_u2 = lambda t1,t2,h: lambda x,y,z: t1(x,t1(y,z))

r17_v1 = lambda t1,t2,h: lambda x,y,z: t2(h(x,y),z)
r17_v2 = lambda t1,t2,h: lambda x,y,z: h(t2(x,t1(y,z)),t2(y,z))

runSolver(t1_def, t2_def, t1a_def, t2a_def, h_def,
  goalLex([t1,t2,h], [x,y,z], [t1a,t2a,h], [xa,ya,za], [(r17_u1,r17_u2), (r17_v1,r17_v2)]))



print("""
     x   y   z                      x   y   z
     |   |   |                      |   |   |
      \ /    |                      |    \ /
       X     |          --->        |     |
      / \\   /                        \\   /
     |   \ /                          \ /
     |    X                            X
     |   / \\                          / \\
      \ /   |                        |   |
       | t2(t2(x,y),z)               | t2(x,h(y,z))
h(t1(x,y),t1(t2(x,y),z))        t1(x,h(y,z))    
""")

r18_u1 = lambda t1,t2,h: lambda x,y,z: h(t1(x,y),t1(t2(x,y),z))
r18_u2 = lambda t1,t2,h: lambda x,y,z: t1(x,h(y,z))

r18_v1 = lambda t1,t2,h: lambda x,y,z: t2(t2(x,y),z)
r18_v2 = lambda t1,t2,h: lambda x,y,z: t2(x,h(y,z))

runSolver(t1_def, t2_def, t1a_def, t2a_def, h_def,
  goalLex([t1,t2,h], [x,y,z], [t1a,t2a,h], [xa,ya,za], [(r18_u1,r18_u2), (r18_v1,r18_v2)]))