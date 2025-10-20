---
date: 2025-10-20
title: Rust 1.88 let_chains compared to Haskell
---

A while ago rust released version 1.88 and I read it and thought it was similar to how haskell
worked and thought I'd compare these two.

In the post they posted [here](https://blog.rust-lang.org/2025/06/26/Rust-1.88.0/) they have the following
code example.

```rust
if let Channel::Stable(v) = release_info()
    && let Semver { major, minor, .. } = v
    && major == 1
    && minor == 88
{
    println!("`let_chains` was stabilized in this version");
}
```

and this is how it could look like in haskell

::: {.dn}

### Some prep data

```haskell
{- LANGUAGE ViewPatterns #-}

data Semver = Semver { major :: Int, minor :: Int, bugfix :: Int }
    deriving (Show)

data Channel = Unstable Semver | Stable Semver
    deriving (Show)
```

:::

```haskell
releaseInfo :: Channel
releaseInfo = Stable $ Semver 1 88 0

test :: IO ()
test = case releaseInfo of
    (Stable (Semver 1 88 _)) -> putStrLn "Rusts `let_chains` was stabilized in this version"
    _ -> pure ()
```

Haskell is more expressive
when they need to express pure vs side effect functions. The old releaseInfo was static and
able to be used in any pure code. But most of the time the release version might come from
a database or another source of truth. So it could look like this.

```haskell
releaseInfoIO :: IO Channel
releaseInfoIO = pure . Stable $ Semver 1 88 0 -- or get from a db or similar

rustLetChain :: Channel -> IO ()
rustLetChain (Stable (Semver 1 88 _)) = putStrLn "Rusts `let_chains` was stabilized in this version"
rustLetChain _ = pure ()

fakeMain :: IO ()
fakeMain = do
    v <- releaseInfoIO
    rustLetChain v
```
