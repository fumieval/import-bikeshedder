import-bikeshedder
======

Import sorting tool for Haskell sources

Usage
----

`$ import-bikeshedder [command] -- [paths]` generates diffs.

commands:

* `shuffle` Shuffle imports.
* `sort` Sort imports in an alphabetical manner.
* `sortby /path/to/grouping-rules` Sort imports using the given set of grouping rules.

An example of a grouping rule file:
```
-1
    Prelude
1
    My.Project.Prefix
2
    Debug
```

Example
----

```
$ import-bikeshedder sortby .hs-import-groups -- $(git diff origin --name-only src/) | git apply
```
