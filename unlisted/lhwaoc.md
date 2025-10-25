# Learn Haskell with Advent of Code

## Who is this for?

I am aiming this series for those that are new to Haskell but have atleast read
on syntax or a book or two but want work on some examples. Advent of Code is a site that
give various problems to solve in any way you wish and makes a perfect candidate to
ease in new Haskellers into some of the libraries functions.

## How the series is planned

The series only solves the first five days of Advent of Code on each year as those are
usually the easiest. The rest may be for another series when for when you have become
more familiar with Haskell.

## Can I download and run your examples?

Yes. There are two ways you could run them.

### Nix way

Using nix with flakes you can run these commands.

```sh
nix shell nixpkgs#darcs --command darcs clone hub.darcs.net:wgaffa/lhwaoc
nix run ./lhwaoc#2015-01
```

That will clone the code to the directory *lhwaoc*. And the second runs a specific year and day
problem. Note that it may crash if your puzzle input is could not be opened or read.

### With GHC

Download the Markdown file from [Github](https://github.com/wgaffa/wgaffa.github.io/tree/main/posts/lhwaoc).
You also need to have *markdown-unlit* library available to GHC. Also make sure the markdown file ends in
the file extension *.lhs* and not *.md*.

Then you can run `ghc -pgmL markdown-unlit --make file.lhs` and it will compile.

# Current List of posts for this series
