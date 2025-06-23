+++
title = "Term to Type"
date = "2025-06-09"
description = "How I went from term level to type level programming"
+++

# Abstract

When searching `type level programming` on your favorite search engine you will most likely find resources that shows the same code like *peano numbers* and *length indexed vectors* but at times I wished they could show some other examples or use cases. And while some do like Rebecca Skinners article [An Introduction to Type Level Programming](https://rebeccaskinner.net/posts/2021-08-25-introduction-to-type-level-programming.html) I seem to get lost when I try to implement something or fail to understand why I can't do certain things.

Although this post starts out very easy and simple there will be some advanced futures of Haskell mentioned here so a good to advanced knowledge is expected as I won't go into what extensions are being used but are inferred.

This is mostly my personal brain dump but I share this if anyone else finds this helpful.

Most functions and other definitions will not have an implementation unless it's important to understand a specific concept.

# The Problem

Given a character name we want to make sure it follows a specific rule where the rules are as follow:

- Does not exceed the maximum character limit (maximum 19 characters).
- Must use a minimum of 3 characters (a space counts as one).
- Must use two or more words (implies at least one space).
- Must use only Latin letters (digits are not allowed).

And this is trivially done with one or more functions depending on what you prefer or use case.

```haskell
isValidCharacterName :: Text -> Bool
isValidCharacterName = undefined
```
Here we return True if it's a valid and False otherwise, simple and to the point.

```haskell
createValidCharacterName :: Text -> Maybe Text
createValidCharacterName = undefined
```
While here we lift the text up to a `Maybe` type and possibly change the input like removing leading and trailing spaces for example. Not complicated at all.

This may be sufficient for many applications, but I wanted to push further and explore how to make the solution even safer at the type level.

The first thing is that nothing is preventing me from changing the text after I have validated it and use a bad character name to function like
```haskell
someFaultyTower :: Text -> IO ()
someFaultyTower = undefined

cantStopMe :: Text -> IO ()
cantStopMe name = if isValidCharacterName name
	then someFaultyTower ""
	else someFaultyTower name
```
In the true branch I can still call the function with whatever I want or change the name to something that hasn't been validated, likewise I can just call the supposedly good function with the invalid name. The function might want to store the name in a database or display it, but it does expect a valid name.

Secondly likewise with the other function returning a `Maybe` type we can just use the functor class to change the value to something invalid for example.
```haskell
toppledTower :: Text -> Maybe Text
toppledTower name = const "" <$> createValidCharacterName name
```
If we are given a valid name we can still just map over it to whatever text we want, albeit given an invalid name this will still return a `Nothing` value.

So while we do validate a `Text` in itself doesn't hold any information whether this is a valid character name. So what can we do about it?
```haskell
newtype CharacterName = CharacterName {unCharacterName :: Text} deriving (Show)

createCharacterName :: Text -> Maybe CharacterName
createCharacterName = undefined
```
Here we create a newtype that represents a text value. And to make sure it can't be instantiated with a bad value make sure to only export the type and `createCharacterName`. `createCharacterName` is then responsible to check for the invariants and return it in a `Maybe` type or `Nothing` otherwise. Now we can use these for different purposes like have a list of character names `[CharacterName]` or a map between Int and character name `Map Int CharacterName`. As long as we can only get a valid `CharacterName` from `createCharacterName` we know that it is a valid character name.

# Problem Extension

What if we want some different naming rules depending on a value. Say we only allow these rules:

- American Europe
	- Does not exceed the maximum character limit (maximum 19 characters).
	- Must use a minimum of 3 characters (a space counts as one).
	- Must use two or more words (implies at least one space).
	- Must use only Latin letters (digits are not allowed).
- Japanese
	-  Does not exceed the maximum character limit (maximum 16 characters).
	- Must use a minimum of 4 characters (each Japanese character counts as 2 characters).
	- Must use at least 1 Japanese character (Latin letters are optional).
	- Must not contain any spaces.
	- Must use only Japanese characters and Latin letters (digits are not allowed).
- Chinese
	- Does not exceed the maximum character limit (maximum 16 characters).
	- Must use a minimum of 3 characters (each Chinese character counts as 2 characters).
	- Must use at least 1 Chinese character (Latin letters are optional).
	- Must not contain any spaces.
	- Must use only Chinese characters and Latin letters (digits are not allowed).

Here we can expand on some of our functions quite easily.
```haskell
data NameRule = AmericanEurope | Japanese | Chinese

createCharacterName :: NameRule -> Text -> Maybe CharacterName
createCharacterName = undefined
```
So we first define a sum type that holds our types of naming rules conventions and the function would then choose and validate the name based on the `NameRule` given.

So we have now reached another tier of safety and we can still have the same types like before like lists of character names or an associated map. But we do loose for what type of rule was used for this name. We can't define a function like `CharacterName -> NameRule` that would be dependent on the first parameter. To solve this we can create a simple record.
```haskell
data CharacterName = CharacterName {_characterNameRule :: NameRule, _characterName :: Text}
```
And we still get the same benefit as before but with some extra information. But I was experimenting with type level so lets push the name ruling to the type level instead.
```haskell
newtype CharacterName (ruling :: NameRule) = CharacterName {unCharacterName :: Text}
```
But this type does not conform to the `createCharacterName` we had before as it returns a simple `CharacterName`, so what can we do? We can try to instantiate the type based on the NameRule.
```haskell
createCharacterName :: NameRule -> Text -> Maybe (CharacterName ruling)
createCharacterName nameRule name =
	case nameRule of
	    AmericanEurope -> if validAmericanEurope name then Just (CharacterName @AmericanEurope name) else Nothing
	    Japanese -> if validJapanese name then Just (CharacterName @Japanese name) else Nothing
	    Chinese -> if validChinese name then Just (CharacterName @Chinese name) else Nothing
```
But this fails to compile because we are saying that we return a `CharacterName` for all *ruling*. This is called universal quantification. The caller of the function decides what *ruling* is when called. For example a function could look like this.
```haskell
manuelQuePasa :: Text -> Maybe (CharacterName AmericanEurope)
manuelQuePasa = createCharacterName AmericanEurope
```
This would seem to be fine but to a compiler there are options to call this function. For example someone might have typed it wrong and call the function as `createCharacterName Japanese` and then the types `CharacterName AmericanEurope` and `CharacterName Japanese` no longer matches. So we have to do some more trickery.
```haskell
data SomeCharacterName = forall ruling. SomeCharacterName (CharacterName ruling)

createCharacterName :: NameRule -> Text -> Maybe SomeCharacterName
createCharacterName nameRule name =
  case nameRule of
    AmericanEurope -> if validAmericanEurope name then Just (SomeCharacterName . CharacterName @AmericanEurope $ name) else Nothing
    Japanese -> if validJapanese name then Just (SomeCharacterName . CharacterName @Japanese $ name) else Nothing
    Chinese -> if validChinese name then Just (SomeCharacterName . CharacterName @Japanese $ name) else Nothing
```
Here we use a trick called existential quantification to store the type in the value (not really, simplified explanation) to have a simpler type. Now we can return this new type based on name ruling. And in the function we return the right `CharacterName` wrapped inside a `SomeCharacterName`. **uh oh, did you catch it?** In the last branch we are returning a Japanese name instead of the expected Chinese one.

# Singletons

To solve the bug introduced previously we must make sure we can't create the wrong types. And to do that we are going to use something called singletons. ***Don't panic!*** It's not a bad word in this case. 

```haskell
data SNameRule (ruling :: NameRule) where
  SAmericanEurope :: SNameRule AmericanEurope
  SJapanese :: SNameRule Japanese
  SChinese :: SNameRule Chinese

class Sing ruling where
  sing :: SNameRule ruling
  
instance Sing AmericanEurope where
  sing = SAmericanEurope
  
instance Sing Japanese where
  sing = SJapanese

instance Sing Chinese where
  sing = SChinese
```
Here we create a GADT that constructs the appropriate type based on the constructor used. Again, GADT's has been covered greatly in other resources and I'm just touching on the subject here. The class and instances is based on the GADT saying that for any `NameRule` type we get an appropriate type of `SNameRule ruling`. All this can be omitted if you use the [*singletons*](https://hackage.haskell.org/package/singletons) package and replace the above with `genSingletons [''NameRule]` instead.

```haskell
createCharacterName :: (Sing ruling) => Text -> Maybe (CharacterName ruling)
createCharacterName name =
  case sing @ruling of
    SAmericanEurope -> if validAmericanEurope name then Just (CharacterName name) else Nothing
    SJapanese -> if validJapanese name then Just (CharacterName name) else Nothing
    SChinese -> if validChinese name then Just (CharacterName name) else Nothing
```
Note that we no longer need to specify the type when constructing a `CharacterName`. This is because when matching on our `SNameRule` the `ruling` in both the constraint `Sing ruling` and the return type `CharacterName ruling` has to be the same. And since `SChinese` is constructing a `SNameRule Chinese` `ruling` is equal to `Chinese`. If you want a deeper dive into this checkout out [Introduction to Singletons](https://blog.jle.im/entry/introduction-to-singletons-1.html) for a great tutorial.

That is all I had to say for today, I plan on taking this further in an upcoming post, but for now I've covered what I set out to do. Thanks for your time.
