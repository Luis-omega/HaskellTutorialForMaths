------
title: Recursive types and being lazy.
date: 2021-09-13
tags: 
------

Past time we were almost there, we have a way to make types, a way to know how types where made and a way to write functions about them.
If you think on it, we are at a point like ZF where we can make sets from other sets, but we still lack the infinity axiom. 
We don't have the same structures (union, intersection, cartesian product), but we have a set of tools that allows us to talk about  types, functions, functions composition, types composition.

As with ZF, we would be motivated to have infinite elements to build at least the $\mathbb{N}$ (naturals).

If you remember, one could proceed by Peano's axioms or by inductive sets. Let's use Peano's way.

~~~{.haskell}
\begin{code}
  data Nat = 
          Z
        | S Nat
\end{code}
~~~
  
Let's read this as functions and types again

~~~{.haskell}
Nat::Type
Z:: Nat
S:: Nat -> Nat
~~~

So, we're claiming :

  $\exists Nat, \quad \exists Z:Nat, \quad \exists S:Nat->Nat$
  
And by the nature of Haskell we know that if $n:Nat$ then $(S\, n):Nat$ and $\forall n:Nat,\,\, n \neq S\, n$ and $\forall n:Nat, \,\, S n \neq Z$.
In fact we are only lacking the *There is and equality over Nat*, to get a thing that follows the Peano's axioms, if we ignore the fact that 
computer can't store and infinite collection of numbers, but just as us, can encode the expression of something infinite in a finite mean.

From a math point of view, we need a little more to be convinced from this definition, allow me to show the equation :

$X = X + (X->X)$

Here **X** is a type and **a + b** represent the sum type for **a** and **b** types. This equation represents the Haskell **data** definition for **Nat**. We could slightly modify to make a function.


$f X = X + (X->X)$

So, **f** is a function that takes a type **X** and returns a type, then we could define **Nat** by means of :

$f Nat = Nat$

That is, we could see **Nat** as one fixed point for **f**. As fixed point could be not unique, we need to take another step and ask for **the least fixed point**. To be able to talk about that, we need to impose some order over the values of **f**, that means, we need to put some order on **types**. We won't got that far here,  so, we would just think in this equation and the fact that we could formalize the recursivity by it.

Now to get the remain Peano's axioms, we need to define the equality over **Nat** and "proof" the needed properties. Of course Haskell won't ask for all this, but as we are mathematicians we would.


So, what's the way to define equality? For arbitrary types **X** this is even undecidable in **Haskell**, but for this type **Nat** we could define a equality by the obvious way:

    Define a function equNat: Nat -> Nat -> Some type that means equal or unequal 

So, first we need a type that allow us to express the end values of **equNat**, that type must have just two states **they were equal** and **they aren't equal**. We usually call this type **Bool** but as Haskell has his own type **Bool**, we can't name or type **Bool**, so lets call it **Bolean**

~~~{.haskell}
\begin{code}
  data Boolean = BTrue | BFalse
\end{code}
~~~
  
Now for our equality function :

~~~{.haskell}
\begin{code}
  equNat :: Nat -> Nat -> Boolean  
  equNat Z Z = BTrue
  equNat Z _ = BFalse
  equNat _ Z = BFalse
  equNat (S n) (S m) = equNat n m
\end{code}
~~~
  

This must be obvious, two **Nats** are equal of both are **Z** or comes from the same term by the use of **S**.

In this case, we use pattern match in both terms to cover all the cases. Additionally we recursive defined the function by appealing to previous values.

So, we really could proof by induction that this functions is well defined and ends on every two **Nat**.

Even better, we could proof, this function allow us to have naturals by Peano's axioms.

Now that we have the basics cover, we could define the **+** function.

~~~{.haskell}
\begin{code}
  add :: Nat -> Nat -> Nat
  add Z m = m
  add (S n) m = S (add n m)
\end{code}
~~~
  
With those functions as base, we could talk about a particular thing about Haskell called **laziness**. 

Until now, we haven't talk about a crucial thing, How Haskell computes something like **Add (S Z) Z**? 
We have relied in our math intuition to understand how this could be accomplished but this need some care, since math intuition about this isn't equal to Haskell evaluation way.

~~~{.haskell}
\begin{code}
  instance Show Nat where
    show Z = "Z"
    show (S n) = "(S "++show n++")"

  instance Show Boolean where
    show BTrue = "True"
    show BFalse = "False"

\end{code}
~~~
  
