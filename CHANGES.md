# Changes / Release Notes

## 2.2.0.0

- Added the ability to "drill down" into parameters using projection
  functions. E.g. `:user.username`.

## 2.1.0.0

- Added TH derivation for `SqlRow` typeclasses.

## 2.0.0.0

- Added typed full-row results, using the new `FromSqlRow` typeclass
- Unfortunately, this requires a breaking change to the query
  syntax: scalar results now require parentheses to distinguish
  them from full-row results.

## 1.0.0.0

- Added the ability to fetch scalar results (first column only)
- Made "rowcount" syntax explicit, to disambiguate scalar return
  types from row count return types
