# Oshit

Oshiro's git: VCS written in Haskell that tries to be compatible with git.

This is **not safe to use**, and if you use it in serious repositories you
probably will say: "Oshit!". This is only meant for learning how git works and
how hard it is.

## Dependencies

- `ghc`
- `make`

and the following haskell libs:

- `haskell-zlib`
- `cryptohash`
- `base16-bytestring`
- `unix-compat`
- `split`

### Installing dependencies on Arch/Manjaro

~~~bash
sudo pacman -S ghc make haskell-zlib haskell-cryptohash haskell-base16-bytestring haskell-unix-compat haskell-split
~~~

### Installing dependencies on Mac OS
~~~
brew install ghc cabal-install make
cabal install zlib cryptohash base16-bytestring unix-compat split
~~~

### Notes on Debian and Ubuntu
The oficial versions of the packages needed to build and run Oshit can be
outdated! Be careful! Try to install them through other sources, such as cabal
or stack!

I'm providing here a Dockerfile based on Arch, if you prefer.

## Compiling

Just run `make` in the root.

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
