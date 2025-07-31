# GitNet

First purpose is automating `RELEASE_NOTES.md` generation with the flexibility I need for my mono repos like Partas.Solid.Primitives, and other wrappers which are piled into a singular repo.  I can then expand on this over time with more functionality.

## Requirements

- [ ] Parse Commits
  - [x] Parse Conventional Commits
  - [x] Parse unconventional commits
  - [ ] Collate critical information into a record
- [ ] Find Projects
  - [ ] Parse project source/compiled files
  - [ ] SemVer based on changes to these items

## Spec

SemVer spec is used for parsing versions from tags, releases et al. With the following appended support for scoped tagging to indicate a release for a particular tree:

### Scoped Epoch

There is already some semblance of acceptance towards using either a 4th numerical or alphabetical sign to indicate an epoch.

I will begin by adding an extra qualifier for tags in a binding monorepo to disambiguate semvers of different bindings in tags.

```
// Where SCOPE is an optional alphabetical identifier bounded by ( parenthesis )
// Where EPOCH is an optional alphabetical character 
// Where the existence of either epoch or scope is bounded by < angle brackets >
// followed by 0 or more spaces
[<[EPOCH][(SCOPE)]>] [v][MAJOR].[MINOR].[PATCH][-PRERELEASE][+METADATA]
```

In this way parsing is not difficult. Major is determined by parsing the first numeric character.
