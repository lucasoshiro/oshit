# Oshit

<p align="center">
<img src="https://raw.githubusercontent.com/lucasoshiro/oshit/logo/logo.svg" width="200px"></img>
</p>



Oshiro's git: VCS written in Haskell that tries to be compatible with git.

This is **not safe to use**, and if you use it in serious repositories you
probably will say: "Oshit!". This is only meant for learning how git works and
how hard it is.

## Building from source

This project can be built from source using GNU Make, Stack, or Cabal. When
building with GNU Make, you'll have to manage Haskell dependencies on your own.
The Makefile was written with Arch / Manjaro in mind, so dynamic linking is
used, as haskell packages on those distros no longer ship archives for static
linking.

### Stack / Cabal

With Stack installed, the build process is as simple as:

```sh
$ stack build
```

If you're using Cabal directly instead, make sure you have a compatible version
of GHC installed and run the usual:

```sh
$ cabal build
```

Installing is possible with both through their `install` commands.

### GNU Make

As mentioned before, you're on your own here when it comes to managing
dependencies. If you're on Arch or Manjaro, your mileage may vary, but these
should prove sufficient:

- `ghc`
- `make`
- `haskell-zlib`
- `haskell-cryptohash`
- `haskell-base16-bytestring`
- `haskell-unix-compat`
- `haskell-split`

Install them with the usual `sudo pacman -S`.

#### Notes on Debian and Ubuntu

The oficial versions of the packages needed to build and run Oshit can be
outdated! Be careful! Try to install them through other sources, such as cabal
or stack!

I'm providing here a Dockerfile based on Arch, if you prefer.

## Running

### Local

**Only in the root of your project**, run `[oshit path]/oshit [<command>] [<args>]`

### Using Docker

We provide a Dockerfile, which may be safer and contains all the dependencies
pre-installed. On the root of the project, run 

~~~bash 
docker build -t oshit .
~~~

for build the `oshit` Docker image. For executing it, run:

~~~bash
docker run -v "[oshit path]:/oshit" -e OSHIT_AUTHOR=[author] -e OSHIT_EMAIL=[email] -it oshit
~~~ 

## Commands (so far)

### Porcelain commands

- `log`: similar to `git log`. You can provide a hash, or call without parameters.
- `commit`: similar to `git commit`. It will call the default text editor for the commit message
- `add`: similar to `git add`. You **must** provide a file!
- `branch`: similar to `git branch`. Without arguments, show branches. With an argument, create a branch
- `reflog`: similar to `git reflog`. Without parameters, show HEAD reflog. Otherwise, show reflog for the branch
- `cherry-pick`: similar to `git cherry-pick`

### Plumbing

- `commit-from-tree`: similiar to `git commit-tree`. Creates a commit for a tree.
- `create-blob`: similar to `git hash-object -w --stdin`. Creates a blob object.
- `show-obj`: similar to `git cat-file -p`. Show object contents
- `add-blob`: similar to `git update-info --add --cacheinfo 100644`. Adds a blob to the index. A hash and a filename **must** be provided.
- `tree-from-index`: similar to `git write-tree`. Creates a tree based on the index contents.
- `update-branch`: similar to `git update-ref`, but only for branches.
- `update-head`: similar to `git update-ref HEAD`
- `list-tree`: similar to `git ls-tree`
- `merge-tree`: performs a three way merge using three trees, and returns the merged tree
- `merge-commit`: performs a three way merge using three commits, and returns a merge commit
