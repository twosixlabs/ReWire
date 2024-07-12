# Monad Wrangling 101

ReWire is a monadic language, meaning that it is organized in terms of various monads (which ones, we'll get to shortly). There are about a zillion tutorials on monads out there, and most of them are just *terrible*. This is a shame since the idea of a monad itself is really beautiful and, if you know how to use them correctly, they're a really important part of functional programming practice. And, furthermore, they are a really important part of programming language semantics, too, and consequently an important part of formal methods properly understood. 

What this section does is introduce the monad idea through a sequence of simple language interpreters. As we add features to the language, we have to change the monad we use to define the new interpreter. We will see four interpreters whose core is a language of simple arithmetic expressions. 

To see all of the monads discussed in this tutorial defined in one convenient Haskell file, download this: 
[MonadWrangling.hs](MonadWrangling.hs). These monad and monad transformer definitions are in the style of earlier versions of GHC, which were immensely easier to understand than the current mess.
