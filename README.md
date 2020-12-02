# Oshit

Oshiro's git: VCS written in Haskell that tries to be compatible with git.

This is **not safe to use**, and if you use it in serious repositories you
probably will say: "Oshit!". This is only meant for learning how git works and
how hard it is.

## Dependencies

__TODO: I don't remember them...__

## Compiling

Just run `make` in the root.

## Running

**Only in the root of your project**, run `[oshit path]/oshit [<command>] [<args>]`


## Commands (so far)

- `commit-from-tree`: similiar to `git commit-tree`
- `create-blob`: similar to `git hash-object -w --stdin`
- `show-obj`: similar to `git cat-file -p`
- `stage-blob`: similar to `git update-info --add --cacheinfo 100644`
- `tree-from-stage`: similar to `git write-tree`
