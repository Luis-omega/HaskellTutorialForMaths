------
title: Learn Haskell by using Python, immutable python  
date: 2021-11-19
tags: python2Haskell python Haskell inmutable
------


This series intent to be a way to learn the basics about Haskell by 
programming on python in a restricted way, so we could understand
why some decisions on Haskell were made (or why them makes sense).

At the end of this series we would have implemented in python 
a parser combinator library that is pretty inefficient for python code
but that resembles the **parsec** library (or maybe the **megaparserc** one).

The prerequisite is then to have python skills to understand 
the presented material and don't be scared of recursion code.
We won't teach Haskell syntax here unless we need it, but in
general all needed concept are explained.

 Immutable things
============================

So, first thing of all, we begin by forcing ourselves to work on immutable
subset of python. As you could know, things in Haskell are immutable by 
defect just as python **str** and **int** are immutable. This means 
that a lot of thing done here area already done whenever you use 
python **str** or **int**.

To refresh ourselves consider the code for add a **int** inside **x** to 
other one in **y**  and store in **x**

~~~{.python}

x = x + y

~~~
  
When we do **x=x+y** the value of **y** and **x** are find by the interpreter
then added and finally the value pointed by **x** is updated for a new value.

But this also means that the numbers wasn't changed, what change is the 
value pointed by **x** not the number themself. 

If numbers where able to change (and supported by the syntax), we could perhaps  write something like 

~~~{.python}

1=2+2

~~~

And then have **1==4** as **True**. Of course allowing this could be a source of bugs and 
other things as this behaviour is unexpected by most programmers. So, making 
python **int** immutable is a great thing. 

In the Haskell world, all changes to variables is by defect forbidden (you could 
still write mutable code) and discouraged. This means that previous code :


~~~{.python}

x = x + y

~~~

Would be rejected by Haskell as we try to change the value pointed by **x**.  

So, our first restriction would be that, we won't use code that change variables.

This means that a lot of things in python would stop working for us, and as that
we would need to adapt ourselves to this new python. Of course there are some 
things that we won't be able to fix, like garbage collection. As we won't 
change variables, all previous stored values by global context would be still valid 
in the remain of the programs, that means that all global declared things won't be 
garbage collected. This is a major problem if we stick to python but it won't be 
if we just use it for tutorial. So, the advice is **wont use this tutorial 
code in your python code**, the code is only illustrative and is a very bad idea
to follow the mutability idea out of tutorial except for specific task.

  
 Iteration
============================


First thing we would fix is iteration. 


So, how iteration is done in python? 

A pretty basic (and python specific) iteration code is 

~~~{.python}

acc = 0
for ind in range(1,101)
  acc+=ind 

~~~
  
This code breaks our **immutability** rule in two places.

Most notorious one is the mutation of **acc** to store
the accumulation of numbers. If we remove them

~~~{.python}

for ind in range(1,101)
  continue

~~~
  
We still face one mutation less obvious, **ind** is taking values on
**range(1,101)** !. 


So, how could we make and equivalent loop without mutation?

The answer is **recursive calls**. Doing recursion in python is bad idea
as python isn't optimized for that (and imposes a ridiculous constrain on recursion deep)
but is what we would need to gain again iteration power.

~~~{.python}

def useless_loop(previous_loop_value:int)->None:
  if previous_loop_value <101:
    new_value=previous_loop_value +1
    return useless_loop(new_value)
  return 

def loop_start()->None:
  useless_loop(1)

loop_start()

~~~
  
So this new way does (at least as we are concerned right now) the same things.
Both of them take all values from 1 to 100 and do nothing with that.


Now if we want to write the original loop summing the first 100 positive integers,
we could rewrite it like :

~~~{.python}

def loop_body(previous_loop_value:int, acc:int)->None:
  if previous_loop_value <101:
    new_value=previous_loop_value +1
    new_acc = acc+new_value
    return loop_body(new_value,new_acc)
  return acc

def loop_start()->None:
  return loop_body(1)

loop_start()

~~~

And as the good programmer you are, you probably has think of modify it to a
more general function acting a little like the python **range** function.



~~~{.python}

def range_body(
  ,step:int
  ,max_value:int
  ,previous_iter_value:int
  ,body_function:Callable[[int, a], a]
  ,previous_iter_result:a
  )->a:

  if previous_iter_value + step <max_value:
    new_value=previous_iter_value +step
    new_iteration_result = f(new_value, previous_iter_result)
    return range_body(step, max_value, new_value, body_function, new_iteration_result)
  return new_iteration_result

def new_range(
  begin:int
  ,step:int
  ,max_value:int
  ,body_function:Callable[[int, a] a]
  ,start:a
  )->a:

  return range_body(step, max_value, begin, body_function, start)

def add_two(x:int,y:int)->int:
  return x+y

new_range(0,101,1, add_two, 0)

~~~

There's a lot of things happening here

- I'm using python type annotation syntax. Right now there's a lot of old 
  python code that won't use them and maybe you haven't see them before.
  In Haskell we would need to put type annotations from time to time, but
  we can discard most of them if we want. Is (at least to me) a pretty 
  bad practice to not put at least the annotation to top level functions.
  So, I will put type annotations in all examples. If you want 
  to know about them you can consult [here](https://docs.python.org/3/library/typing.html).

- The annotation for range_body is wrong. Is wrong as we could diverge the computation
  by calling it with a negative value for step. So, this is a consequence of the fact that 
  our **new_range** function is bad implemented or at least it has some corner cases 
  that the original **range** covers and ours won't cover. It's an exercise to fix it!

For clarity let's rewrite all of this as a regular mutable loop:


~~~{.python}

def new_range(start,stop,step,body_function):
  acc = 0
  for iter_value in range(start,stop,step):
    acc = body_function(iter_value,acc)
  return acc 

new_range(0,101,1, add_two)

~~~
  
So, body_function is what we usually put inside a loop and it depends on our previous value of 
iteration variable and the previous value computed by the loop. 

Then we have two patterns mixed here, the one we did first to get a way to iterate over 
the values between **start** and **stop** but doing nothing with it, and the way to
use the value to get another one, reflected here by the **body_function**.

In Haskell both of them are commonly separated and some people call them **data generation** 
and **data consumption**. Of course those therms aren't unique to FP (functional programming)
but the separation tend to be made in those.

A common way to consume a value that resembles our **new_range** function is by using 
a **fold** function.

In plain therms the **fold** function could look as 

~~~{.python}

def fold(self,transform_and_add:Callable[[b,a],a], start:b, foldableThing: t a)->a:
    pass

~~~
  
Here **t a ** could be thinking as **List a**, so in that particular case we could 
implement fold as


~~~{.python}

def fold(self,transform_and_add:Callable[[b,a],b], start:b, foldableThing: List a)->b:
    acc = start 
    for i in foldableThing:
      acc = transform_and_add(i,acc) 
    return acc 

~~~

So **fold** is a way to resume some structure using a function.

Before we could translate this example to our restricted python subset, 
we need to upfront the problem of representing list.


 List 
============================

The default implementation of python list is full of functions that 
uses the mutability of python (as almost any construction in python),
so, our aim here is to build an alternative to regular python list.


There are lots of ways, by example, we could proceed by using tuples,
so instead of :

~~~{.python}

a = [1,2,3]

~~~

we could represent it like :

~~~{.python}

a = (1,2,3)

~~~
  

As python tuples are immutable and we have done the compromise of not 
mutate anything, we could thing of tuples as fully immutable, 
that is, we can't in any form change a tuple if we stick to our compromise.

But instead of follow this way I prefer for us to take a step further in 
Haskell (and FP) direction.

We begin by defining a super-class  for our list type :


~~~{.python}

class List(Generic[T]):
  pass

~~~
  
Then we add two class that can construct our List,
first the one that builds a empty list.

~~~{.python}

class Empty(List, Generic[T]):
  pass

~~~
  
This still has little code, as it only allow us to build a **empty**
list of type **T**. Next one allow us to store items 


~~~{.python}

class Cons(List, Generic[T]):
  head:T
  tail:List[T]

  def __init__(self, new_elem:T, old_list:List[T])->List[T]:
    self.head = new_elem
    self.tail = old_list

~~~
  

Traditionally the function equivalent for **append** in python is called **cons**,
it takes a list and a element to add to list and build a new list.

You could be asking, Why encode list like this?

Well in Haskell code and in most languages with sum types you could equivalently 


~~~{.haskell}

data List t = 
    Empty 
    | Cons t (List t) 

~~~
  

And the tree elements being defined by that code has a meaning similar of those 
defined by our classes.

**List** is a class whose intend use is in the form of calls to **isinstance**,
that way we could discriminate things that are **List** from those that aren't.
In Haskell his position means that **List** is a new type an that **Empty**
and **Cons** are functions that could be use to construct **List**.
So, to preserve Haskell meaning, we must add a new rule, we won't ever instance
the class **List** directly, we could accomplish this by some python mechanism 
but we would be pleased just with the convention.

In this way our **Empty** and **Cons** class are the only way to get thing 
that could be found to be a **List**, just as in Haskell.

A big gain that we have in Haskell is that this mechanism allow us to write 
something like :

~~~{.haskell}

case list of 
  Empty -> something1...
  Cons head tail -> something2...

~~~
  

Whit a python resemblance to 


~~~{.python}
if isinstance(list,List):
  if isinstance(list, Empty):
    something1...
  else :
    head = list.head
    tail = list.tail
    something2...
else :
  raise Unexpected(f"Expected list type, we got {type(list)}")
~~~
  
Difference in size is clear, but this would be a unjust way to compare python and 
Haskell, as python isn't made to write this kind of code while Haskell expect to 
see it in almost any function.

Other differences:

- Haskell by static check of types, can determine that **list** in fact is 
  expected to have type **List**, so, it could check if we are missing some
  cases of either **Empty** or **Cons**  and warring us about that.
  In the python case, if we break our rule of only create **List** by 
  using **Empty** and **Cons** our inner **if** could fail in multiple ways.
  So, as Haskell can enforce us to only use **Empty** and **Const**
  to make things of type **List**, then it could be sure that all the
  possible cases are those and check for us.

- Haskell pattern match binds to variables **head** and **tail** the 
  components used in the creation of a **List**. To get this effect 
  we need to introduce the **head=list.head** and **tail=list.tail**
  in our python code. Of course we can just use **list.head**
  and **list.tail** for it, but I wanted to be clear about this 
  point here.


- As we said two times in first point, Haskell is able to check
  that variable **list** is in fact a **list**. In python we can't be
  sure until we call **isinstance** but this also means that a bug
  in our code could make our **list** variable of some other type.
  That's why I choose to use an exception here, as python 
  type errors occurs at runtime.

Other things to note is that in Haskell forgot to handle a case to build a **List**
would raise a warring but not a exception, at least no until we try to use 
the part of code.

~~~{.haskell}

case list of 
  Cons a b -> someting...

~~~
  

Could be translated as 


~~~{.python}

if isinstance(list,List):
  if isinstance(list, Cons):
    head = list.head
    tail = list.tail
    something...
  else :
    raise MissingCase("You forgot to generate code for the Empty case of list at line n")
else :
  raise Unexpected(f"Expected list type, we got {type(list)}")

~~~
  

Again we chose a exception here as Haskell full code looks like 


~~~{.haskell}

case list of 
  Cons a b -> someting...
  _ -> error "Missing pattern at line n"
~~~

So, Haskell would warn about this at compilation and then would
proceed to add and error for all the missed cases. This way,
function would be called as a regular one and would exploit on our 
faces if we pass some **Empty** list. Then the most accurate way 
to represent it in python is by the exception.


This is one of the strong points of Haskell and others languages
with strong, static type check and sum types. They could 
warn us about this missing cases but still would processed if we want.

So, how does a list looks in this representation?

In Haskell :

~~~{.haskell}

a = [1,2,3]

~~~

is just a short way to write 
  

~~~{.haskell}

a = Cons (Cons 1 (Cons 2 (Cons 3 Empty)) )

~~~

Here we see the Haskell syntax for function call, instead of 
**f(x,y,z,w)** we write **f x y z w**, and Haskell compiler introduce 
parentheses to make it **(((f x) y)z) w**

So, Haskell has in essence only functions that take one value 
as argument, in this case **f x** returns a function again 
an is that function the one in witch **y** is passed as argument.

We wont go deeper about this here, but you can check this 
by searching for [currying](https://en.wikipedia.org/wiki/Currying).


A last thing about this pattern is the way **Empty** is used here.

In the underlying theory, **Empty** is a function of type :

~~~{.haskell}

Empty t :: unit -> List t

~~~
  

Here the **unit** type is something that has exactly one element.
A real equivalent definition for our **Empty** class would consist
of make a singleton class called **Unit** and use it to make
our **Empty**


~~~{.python}

class Empty(List, Generic[T]):
  def __init__(self, unit:Unit)->List[T]:
      return

~~~
  

But regularly we don't need to worry about this and Haskell 
allow us to omit he **Unit** argument of **Empty** as it could only
be **unit**. So, I choose to drop that part in the definition
of **Empty**.

All that together to get our python example of **[1,2,3]**

~~~{.python}

a = Cons(1,Cons(2,Cons(3,Empty)))

~~~
  


Now, a thing we haven't talk is why we use **T** in our python 
definition and **t** in Haskell definition.
This constrain means that we could only construct list of one type.
That forbids us to write list like **[1,"asdf",(1,2)]**.
We could recover a little by introducing new types that could store 
this mixed values, but we won't do that in this entry.


Last thing to compare before return to iteration, is that 
Haskell list has elements append at left, while python has them append at right.
Is a minor difference here but in certain context of formal verification
it could made a big difference.


  Iterating over List
============================

Now that we have **List** we could refactor our approach to write a new 
**range** function. 

To this, we would visit a old version of python, python 2.
In python 2 the range function generates a full list of things, like 

~~~{.python}

def old_range(start:int,end:int,step:int)->List[int]:
  acc = []
  count = start
  while count <end:
    acc.append(count)
    count = count + step
  
  return acc
~~~
  

Of course, again, this isn't a full implementation of **old_range** but it's all
we need.

Then our aim is to do the same here to gain our range function:

~~~{.python}

def range2_loop(
  count:int
  ,step:int
  ,end:int
  ,current_list:List[int]
  )->List[int]:

  if count < end :
    new_list = Cons(count, current_list)
    return range2_loop(count+step, end, new_list)
  else :
    return current_list

def new_range2(
  start:int
  ,end:int
  ,step:int
  )->List[int]:
   
  start_list = Empty
  return range2_loop(start,step,end, start_list)

~~~
  
Again, this is incomplete as it won't handle negative numbers or other cases, but
is enough for the main point.

With list as base we could write our **fold** function for lists as :

~~~{.python}

def foldList(
  start_elem:T2
  ,transform_add_function:Callable[[T2, T1],T2]
  ,list:List[T1]
  )->List[T2]:
  if isinstance(list,Empty):
    return start_elem
  else 
    new_value = transform_add_function(start_elem, list.head)
    return foldList(new_value, transform_add_function, list.tail)

~~~
  

We omit the check of **list** being a **List** and the check that all values 
are instance of **Empty** or **Cons**. We would continue to do this 
all the time but be warred that those steps could be needed 
to avoid mistakes.


With both of our functions implemented, we could write or program to 
sum the first **100** integers as :

~~~{.python}

foldList(0,add_two,new_range2(0,1001,1))

~~~
  
With this we take our original approach and refactor it to get a much clean
code (or at least more understandable code to me).

A big advantage of this encoding is that **foldList** is enough general
to handle all the list we could made instead of just the one generated by
**new_range2**. 

And we got all of that without explicitly mutating things!

 Hidden mutation
============================

Of course we have perform some mutations under the hood.

We can't store space in memory without changing the memory, so 
all instance creation would mutate the memory. But the key here is :
**We don't explicitly mutate anything**. So if there's a fail 
given to mutation, is a fail made by python runtime himself and not 
by the logic of our program. This means that we could be sure that 
as long as python runtime is right implemented, our program would 
perform as expected. In Haskell we have exactly this warranty, 
there are mutations happening under the hood but we don't need to
know them, we just need to trust Haskell runtime (and code generation)
. This also means that those parts must have an intensive revision 
to avoid bugs. 

In exchange we could code without being worried that 
some thing are being mutated by accident. Of course we could 
still mess around our logic and do something like **mutate things**
without intend it, but we would come back to that latter.


Let's complete our **List** functions by implement some other functions.

 Complete List module
============================

What if instead of fully consume a list we want to transform a list of one 
type to another?, by example, if we have a list of chars and want it 
to become a list of ints, what should we do?

There's where **map** function comes to help us


~~~{.python}

def mapList(transform:Callable[[a,b], b], list:List[a])->List[b]:
  if isinstance(list,Empty):
    return Empty
  else :
    transformed_head = transform(list.head)
    transformed_tail = mapList(transformed, list.tail)
    return Cons(transformed_head, transformed_tail)

~~~
  

So, to our example 

~~~{.python}

def charList2intList(list:List[str])->List[str]:
  return mapList(ord,list)

~~~
  
Of course again our types are bad as we use **str** while we wanted to use **char**,
but as python doesn't have **char** type, most simple hack is to use **str**. Then 
whenever we use a **list** with more than one char per item, we would get a 
exception. We could prevent this by defining a type **Char** that is 
warrantied to store just a **char**

~~~{.python}


class Char:
  def __init__(self, c:str):
    if len(str)==1:
      self.char = c
    else :
      raise NonChar("We tried to make a char instance with more or less than one char")

~~~
  

This approach still raises an exception, but this exception would be raised
when we try to instance **Char** and this could happen in a different point of program.
But generally the exception of **Char** would be in a place closer to some bug.

With this class done we need a new function that allow us to split **str** in **List Char**

~~~{.python}

def str2charList(s:str)->List[str]:
  if len(s)=0 :
    return Empty
  else :
    head = s[0]
    tail = s[1:]
    transformed_tail = str2charList(tail)
    return Cons(head, transformed_tail)

~~~
  

Now our previous function can be implemented as :

~~~{.python}

def charList2intList(list:List[Char])->List[int]:
  return mapList(ord,list) 

~~~
  

Putting all together to write a function from **str** to **List int** :

~~~{.python}

def str2intList(s:str)->List[int]:
  char_list = str2charList(s)
  int_list = charList2intList(char_list)
  return int_list

~~~
  
With all that done, we can now manipulate **str** as **List Char**,
that means, we could implement the build-in functions of **str**
in terms of **List Char**. This could be a waste as 
python already has immutable strings, but we have shown that 
we could. 

The remain traditional function to work with list is **filter**.
That function, allow us to keep just the elements of list that 
match some condition.


~~~{.python}

def filterList(predicate:Callable[[a],bool], list:List[a])-> List[a]:
  if isinstance(list, Empty):
    return Empty
  else :
    tail_filter = filterList(predicate, list.tail) 
    if predicate(list.head):
      return Cons(list.head, tail_filter )
    else :
      return tail_filter
~~~
  
 Why immutable?
============================

A major reason to maintain immutability is to have [referential transparency](https://en.wikipedia.org/wiki/Referential_transparency).

This means that if we know that function **f** would return some value **k** always, since
we won't allow mutations to happen inside **f**, we could 
replace all the calls to **f** by just **k** without fear about our program.

Or if we are tracking what function has unexpectedly changed a global value, 
we could skip all immutable parts as those are guaranteed to not change the value. 

In python the former is a weak reason as interpreter generally can't make the 
substitution (and probably won't try to check if some code is immutable). But 
the later is a great reason to have immutable things.

By comparison in Haskell this kind of things allow the compiler to 
make strong transformations to our code and still do the same thing at end.
Those part has it's roots in **equational reasoning** and is out of scope right now.


Haskell isn't the only language with those benefits from **referential transparency**,
the benefits goes even beyond FP languages and extends to imperative ones like C.
If compiler is able to check that some function **f** works with our immutable
rules, then it could just as in Haskell make strong changes in code. 
So, imperative compilers tend to have a part dedicated to identify those class of 
functions.

To end the section, a function that follow the immutability rules are generally called
[**pure functions**](https://en.wikipedia.org/wiki/Pure_function)


  Unexpected thing of recursivity
============================

In python we know that recursivity as those we use in our functions, is a bad idea
as python isn't able to handle it with grace. What is unexpected is that 
Haskell won't handle it much better in some sense. 

People that already work in other languages that encourages recursivity, tend to 
rely on [tail calls](https://en.wikipedia.org/wiki/Tail_call) to optimize code
and guarantee bounded stack. Haskell won't perform tail call optimizations as expected.
At least, given the **lazy** nature of Haskell, we can't expect naive tail calls to 
work as in other languages. More on this in other entries.


Lesson here is that our python code has **tail calls** not because we 
expect python nor Haskell to optimize them, just for a matter of taste with other
languages and ease of write (and understand). So don't go to Haskell thinking
**I'm gonna conquer the language using tail calls** 


That's all for this entry. I hope that you have a better understanding of 
how things work under the hood in Haskell or at least a little about why it 
looks as weird as it does compared to imperative languages with 
unrestricted mutation.

