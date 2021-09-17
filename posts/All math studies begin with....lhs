------
title: All math studies begin with... 
date: 2021-09-11
tags: Tutorial, TypeTheory, Maths
------

Most programming languages tutorials begin on the lines of a program like :


\begin{code}
main = putStrLn "Hellow world!"
\end{code}

  
Since we already understand math, we could begin without that sort of thing, of course we
finally would come to that, but right now, we could (and we would!) discuss Haskell 
in another view, the **I'm already a mathematician that don't work with category theory**. 


In the beginning there were nothing... or not?
---

Just as we begin in maths with some *basic things* to build the currently studied theory, 
we need some *ground* to begin.
If we go too deep, this *base* could be just as in math, logic. 
Of course going that deep would be time consuming, so, instead we would do as in math and 
claim 

    We have the power of make propositions over things, then we could add axioms to work

and then make some fixes to our logic in the way to learn haskell.

Once we have some **primitive logic** (in fact a **meta language**), we need objects to work.
In math we start over the lines of :

    The sets exists

Or eventually equivalent in **ZF** theory :

    The empty set exists

For us this kind of **axiom** (and just in our informal presentation), is :

    Types exist and there could or not exist elements of a given type.

So, for our Haskell study, types are like sets are for math. That is a long way to
advice you to don't ask (right now) What's a type? And just follow the flow.

In fact types could have set representation and probably we would only work with types
that could be made sets, but in general they can't be sets[^scott] ,
at least not in a too expressive logic.

[^scott]: see things related to [scott continuity](https://en.wikipedia.org/wiki/Scott_continuity)

At the same time we introduce sets, we usually add notation to refer to sets, over the same lines

Notation is syntax here, more or less...
---

In Haskell the way to claim **Some** is a *type* that has an inhabitant **Some1** (an element of, in set terms), is:


\begin{code}
data Some = Some1
\end{code}


Haskell only allow us to make types that begin with upper letters, we would look at the reason behind  this 
in the future.

The **Some1** is called a *constructor* for  the type **Some**.

More definitions:

Let **T** be a **Type**, then for a given **x** we could say 

    x is of type T

and for that we write :

    x:T

So things can have a type, nice. 

The ugly thing here is that Haskell uses the syntax 


Some1::Some

  
to say **Some1** has type **Some**.

That's since Haskell uses **:** alone to another thing. This means that we would use
**x:T** whenever we aren't talking of Haskell code, and we use **x::T** in Haskell code.

Another thing to note in this example is that **Some1** has two meanings in 


data Some = Some1


One of them is :
    
    Some1 is an element of type Some

The other one is :

    Some1 is a constructor for the Some type that takes no argument.

In math terms:
  
    We abuse the notation to write Some1 to both thing, a constant function without arguments and an element of Some.

This distinction will become important in the future.

Now the math tradition of set theory would continue with ways to make sets from already defined sets.

This is a bit tricky since we have two fundamental ways to make a set, we could make a subset of
an already defined set or we could use two or more sets in one in some way.
The problem with this approach is  the part with subsets. We regularly won't work with subtypes in the 
way we do in set theory since defining what a **Subtype** of a **Type** is, is hard or impossible depending on the context. 

Constructors
============================

That say, we could compose **Types** to make new types in a nice way :


\begin{code}
data  TwoSomes = GivemeTwoSomes Some Some 
\end{code}

  
This read as :
  
    TwoSomes is a type that could be made from two things of type Some by using the constructor (function) GivemeTwoSomes.

So, we claim  **GivemeTwoSomes** is a function, what's it's domain and image? 

In math terms :

  $GivemeTwoSomes: Some \times Some \rightarrow TwoSomes$

So, you give to **GivemeTwoSomes** two elements of *type* **Some** and **GivemeTwoSomes** would return you an element of type **TwoSomes**. That's a 
good reason to refer to **GivemeTwoSomes** as a constructor for **TwoSomes**, especially since we don't know right now other way to
make an element of type **TwoSomes**.

Let's examine **GivemeTwoSomes** type in Haskell :


GivemeTwoSomes:: Some -> (Some -> TwoSomes)


Is almost the same! 

We could come close to this in math by using 

  $TwoSomes^{Some \times Some } \cong  (TwoSomes^{Some})^{Some}$

  
The functions from $Some \times Some$ to $TwoSomes$ are isomorphic to functions from $Some$ to functions from $Some$ to $TwoSomes$.

So, Haskell uses the later way to treat functions, usually referring to this as [currying](https://en.wikipedia.org/wiki/Currying).
This means that we only have functions of one element in Haskell, but those functions can return another functions.
Of course in practice we can't just use functions of one element, so Haskell syntax allow us to write both:  


GivemeTwoSomes:: Some -> Some -> TwoSomes
GivemeTwoSomes:: Some -> (Some -> TwoSomes)


with the same meaning.

In the general case :


T :: a -> b -> (c -> d) -> f -> g 
T :: a ->(b ->((c -> d) ->( f -> g)))


Usually this is expressed by saying :

    The (->) associates to the right 

In just the same way we could have a non associative operation **+** and we choose to drop parens in

  $a+(b+(c+d)+(f+g))$

As 

  $a+b+(c+d)+f+g$

In fact **->** is a sort of operator between **Types**, but more on that other day.


There is still other two things to mention on **Constructors**


Types with more constructors.
---------------------

Since we are making new types by defining a function that takes other types and get us our newtype, nothing stop us to use more than one function.


data SomeSomes =  GiveMeFourSomes Some Some Some Some
              | GiveMeFiveSomes Some Some Some Some Some
              | GiveMeSevenSomes Some Some Some Some Some Some Some

  
Here the **|** is used to separate the different constructors, ins fact we could write :



data SomeSomes =  GiveMeFourSomes Some Some Some Some | GiveMeFiveSomes Some Some Some Some Some | GiveMeSevenSomes Some Some Some Some Some Some Some



Or more aesthetics


\begin{code}
data SomeSomes =  
     GiveMeFourSomes Some Some Some Some
     | GiveMeFiveSomes Some Some Some Some Some
     | GiveMeSevenSomes Some Some Some Some Some Some Some
\end{code}



The only requirement here is to put all the constructors for **SomeSomes** more at the right than the first column. This allows us to write


data SomeSomes =  
       GiveMeFourSomes Some Some Some Some
             | GiveMeFiveSomes Some Some Some Some Some
  | GiveMeSevenSomes Some Some Some Some Some Some Some


But please, for Hilbert sake, don't do that!


As you saw, the real restriction here, is how much tired we could become of write **Some** in the right side of the constructor, or 
to add more constructors.

Types with more than one type
---

Of course, we aren't limited to write only **Some** type, so :



\begin{code}
data AnotherType = AnotherConstructor 
data AnotherType2 = 
    AnotherConstructor2 
    | AnotherConstructor2_sub
data AComposeType = Compose AnotherType AnotherType2
\end{code}

  

Here we need to talk about the relation between **constructors** of the same **type**.

Both constructors **AnotherConstructor2** and **AnotherConstructor2_sub** means separated constants (constants functions), for 
**AnotherType**. This means that in general, different constructors of the same type can't produce the same element!
So even if both constructors takes the same type of arguments, and we use the same things for every type, the constructed element 
is always different.

Another example :


\begin{code}
data R = R1
data L = 
      L1 R
      | L2 R 
      | L3 R
\end{code}

  

In math terms we could write it like:

  $L := \{('L1',r),('L2',r),('L3',r) | r \in R\}$

Or more generally, we talk about [disjoint union](https://en.wikipedia.org/wiki/Disjoint_union), but here is called [sum types](https://en.wikipedia.org/wiki/Tagged_union), although in other programming languages this can be found as [tagged union](https://en.wikipedia.org/wiki/Tagged_union).

So we could write **L** in a way like

  $L := R+R+R$

Where **+** denotes sum of the types.

This assumption from Haskell allows us to ask for every **x:T**, What constructor for **T** was used in the creation of **x**?

That question would lead us to the :

Case expression and first functions
---

So, until now we only have defined types, but we haven't write a function that don't define a type, is time to fix it.


First some types



\begin{code}
data CaseType = 
        First
      | Second 

data ReturnType = Return CaseType
\end{code}


Our task would be to write a function that take a **CaseType** an if it was made from **First** return **Return(Second)** otherwise return **Return(First)**. 
In math :

  $f : CaseType \rightarrow ReturnType$

  $f(First) = Return(Second)$

  $f(Second) = Return(First)$

An here comes (a ugly form for this simple case) a way to implement it in Haskell



f :: CaseType -> ReturnType
f x = case x of 
        First -> Return Second
        Second -> Return First

  
There's a lot to talk here.

- The type for **f**, **f:: Case...**  is almost optional. Haskell has something called [type inference](https://en.wikipedia.org/wiki/Type_inference) that allow us most of the time to omit the type for **f**. As code become complex, **type inference** becomes undecidable. That means that we can't have a general way to compute always a right type for things, so, some times Haskell would ask us to put the **type** by hand. As a mathematician I think you really know the power of reasoning about how to define and use functions by having functions domain and range, so is quite superfluous to warn you to always or almost always put the types for this kind of functions.

- Then comes the **f x = ... **. As we said before, in Haskell we have functions of only one argument, so we can drop the parens **(** and **)** in functions and just write **f x** instead of **f(x)**. More on this latter.
  
- Now the **case expression**. **case** is a way Haskell bring us the right to ask how things were made. So instead of the **x** we could put any **expression** and Haskell would allow us to disintegrate  the **expression** in parts. The way to tell Haskell *I only want things made this way*, is by putting something called **pattern match** at the begin of the following lines. In our case, the **pattern match** is just the name of our constructor. This way we tell Haskell *If x was made from the First constructor, then use Return constructor an Second constructor to return an element of type Return*. The second line is the same but for **Second**, **Return**, **First**.


Something beautiful is that we could write :


\begin{code}
f First = Return Second
f Second = Return First
\end{code}


So, Haskell syntax allows us to make the **case/pattern match** at the left side of the **=**. This is almost the same as the mathematical way to write the function.


What happens to this if we have bigger cases?


\begin{code}
data Color = Red | Blue| Yellow
data ColorPairs = Pairs Color Color
data EqualColors = Yes | No 
\end{code}


We would define a function **compare** that takes a pair and returns a **Yes** if both colors are equal, otherwise **No**.

In math :

$compare((x,y))= Yes \quad \text{if} \quad x = y$

$compare((x,y))= No \quad \text{if} \quad x \neq y$
  


\begin{code}
compare (Pairs x y) = 
  case y of 
    Red -> case x of 
            Red -> Yes
            Blue-> No
            Yellow-> No
    Blue -> case x of 
              Blue -> Yes  
              _ -> No
    Yellow -> 
      case x of 
        Yellow -> Yes
        _ -> No
\end{code}

  
This is quite large, but follows some rules.

  
  
- The cases are more at the right than the **case** declaration.
- Right side of a case can start in the line or the next line but it needs to be more at right than the case.
- **\_** stands for **any case that hasn't been taken account**


In **(Pairs x y)**, the parents are needed since, if you remember


Pairs :: Color -> Color -> ColorPairs 
Pairs :: Color -> (Color -> ColorPairs)


And we have 


compare :: ColorPairs -> EqualColors


So, adding parents to the next expression


compare Pairs x y
((compare Pairs) x) y


While we want 


compare ((Pairs x) y)

 
Why the parents goes to the left? 

If you think of it, is just because **(->)** goes to the right. By using this convention we could omit parents in things like: 


f :: a -> b -> c -> d
f a1 :: b -> c -> d
f a1 b1 :: c -> d
f a1 b1 c1 ::  d

  

Now simplifying as before, and calling the function **compare2**:



\begin{code}
compare2 (Pairs Red Red) = Yes
compare2 (Pairs Yellow Yellow) = Yes
compare2 (Pairs Blue Blue) = Yes
compare2 (Pairs _ _ ) = No
\end{code}

  
What a bout a function that takes two **Colors** and return **EqualColors** the same way as **compare** and **compare2**?




\begin{code}
compareColors Red Red = Yes
compareColors Yellow Yellow = Yes
compareColors  Blue Blue = Yes
compareColors  _ _  = No
\end{code}


Or the one liner :


\begin{code}
compareColors2 x y = compare2 (Pairs x y)
\end{code}

  
Did you remember that **Pairs** is a function right? What if i show you the composition operator "**.**"?



\begin{code}
compareColors3 x y = (compare2 . Pairs x) y 
\end{code}


Here we are legitimate composing **compare2** and **Pairs x** since:


x ::Color
Pairs :: Color -> (Color  -> ColorPairs)
Pairs x :: Color  -> ColorPAirs
compare2 :: ColorPairs -> EqualColors
compare2 . (Pairs x) :: Color -> EqualColors
compare2 . Pairs x :: Color -> EqualColors

 
We could omit the **()** since we have **.**  here, more on that other day.

A thing to note is that **.** has an space before and after the functions it is composing, these spaces could or couldn't  be needed, it depends on the contexts since **f.h** could have other meanings in Haskell. To avoid problems, we always write **f . h** with the space for composition. In fact, this is one of the things that I could say are *ugly* of Haskell, and probably most mathematicians can understand this (or not, you are free).

So, what about another short version of this **compareColors**?


\begin{code}
compareColors4 x = compare2 . Pairs x  
\end{code}


This time we omit second argument for **compareColors4** since Haskell can make the previous check of types for **compare2 . Pairs x** and **compareColors4** always has been a function from **Color** to a function that takes a **Color** an returns a **EqualColors** .


Now as last thing, we need to talk about the next code:


\begin{code}
instance Show Some where
  show Some1 = "Some1"

instance Show TwoSomes where
 show (GivemeTwoSomes _ _) = "GivemeTwoSomes Some1 Some1"

instance Show SomeSomes where
  show (GiveMeFourSomes _ _ _ _) = "GiveMeFourSomes Some Some Some Some"
  show (GiveMeFiveSomes _ _ _ _ _) = "GiveMeFiveSomes Some Some Some Some Some"
  show (GiveMeSevenSomes _ _ _ _ _ _ _) = "GiveMeSevenSomes Some Some Some Some Some Some Some"

instance Show AnotherType where 
  show AnotherConstructor  = "AnotherConstructor"

instance Show  AnotherType2 where
  show  AnotherConstructor2  = "AnotherConstructor2"
  show  AnotherConstructor2_sub = "AnotherConstructor2_sub"

instance Show AComposeType where
  show (Compose x y ) = "Compose " ++ show x ++" "++show y

instance Show R where 
  show R1 = "R1"

instance Show L where 
  show (L1 x) = "L1 "++ show x
  show (L2 x) = "L2 "++ show x
  show (L3 x) = "L3 "++ show x

instance Show CaseType where
  show First = "First"
  show Second = "Second"

instance Show ReturnType where
  show (Return x) = "Return " ++ show x
\end{code}

 
This code allow us to have a nice print if you import this file to ghci. As you can see, this defined some **show** functions, generally Haskell won't allow us to write more than one function with the same name, but this **instance** syntax is special. We won't discuss the instance syntax here since we need more things to be made first, so, don't think a lot on this code. 

