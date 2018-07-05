{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE CPP #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE FlexibleInstances #-}
{-|
Module: Database.YeshQL
Description: Turn SQL queries into type-safe functions.
Copyright: (c) 2015-2017 Tobias Dammers
Maintainer: Tobias Dammers <tdammers@gmail.com>
Stability: experimental
License: MIT

Unlike existing libraries such as Esqueleto or Persistent, YeshQL does not try
to provide full SQL abstraction with added type safety; instead, it gives you
some simple tools to write the SQL queries yourself and bind them to (typed)
functions.

= Usage

The main workhorses are 'yesh1' (to define one query) and 'yesh' (to define
multiple queries).

Both 'yesh' and 'yesh1' can be used as TemplateHaskell functions directly, or
as quasi-quoters, and they can generate declarations or expressions depending
on the context in which they are used.

== Creating Declarations

When used at the top level, or inside a @where@ block, the 'yesh' and 'yesh1'
quasi-quoters will declare one or more functions, according to the query names
given in the query definition. Example:

@
[yesh1|
    -- name:insertUser :: (Integer)
    -- :name :: String
    INSERT INTO users (name) VALUES (:name) RETURNING id |]
@

...will create a top-level function of type:

@
    insertUser :: IConnection conn => String -> conn -> IO (Maybe Integer)
@

Using plain TH, it can also be written as:

@
yesh1 $ unlines
    [ "-- name:insertUser :: (Integer)"
    , "-- :name :: String"
    , "INSERT INTO users (name) VALUES (:name) RETURNING id"
    ]
@

== Syntax

Because SQL itself does not *quite* provide enough information to generate a
fully typed Haskell function, we extend SQL syntax a bit.

Here's what a typical YeshQL definition looks like:

@
[yesh|
    -- name:insertUser :: (Integer)
    -- :name :: String
    INSERT INTO users (name) VALUES (:name) RETURNING id
    ;;;
    -- name:deleteUser :: rowcount Integer
    -- :id :: Integer
    DELETE FROM users WHERE id = :id
    ;;;
    -- name:getUser :: (Integer, String)
    -- :id :: Integer
    SELECT id, name FROM users WHERE id = :id
    ;;;
    -- name:getUserEx :: [(Integer, String)]
    -- :id :: Integer
    -- :filename :: String
    SELECT id, name FROM users WHERE name = :filename OR id = :id
    |]
@

Note that queries are separated by _triple_ semicolons; this is done in order
to allow semicolons to appear inside queries.

On top of standard SQL syntax, YeshQL query definitions are preceded by some
extra information in order to generate well-typed HDBC queries. All that
information is written in SQL line comments (@-- ...@), such that a valid
YeshQL definition is also valid SQL by itself (with the exception of
parameters, which follow the pattern @:paramName@).

Let's break it down:

@
    -- name:insertUser :: (Integer)
@

This line tells YeshQL to generate an object called @insertUser@, which should
be a function of type @IConnection conn => conn -> {...} -> IO (Maybe Integer)@
(where the @{...}@ part depends on query parameters, see below).

The declared return type can be one of the following:

- (); the generated function will ignore any and all results from the query
  and always return ().
- The keyword 'rowcount', followed by an integer scalar, e.g.
  'Integer' or 'Int'; the generated function will return a row count from
  @INSERT@ / @UPDATE@ / ... statements, or 0 from @SELECT@ statements.
- A tuple, where all elements implement 'FromSql'; the function will return
  the result set from a @SELECT@ query as a 'Maybe' of such tuples, or always
  'Nothing' for other query types. For example, @:: (String, Int)@ produces
  a function whose type ends in @conn -> IO (Maybe (String, Int))@. Null-tuples
  are marshalled to '()', ignoring result sets; one-tuples (written as @(a)@)
  are marshalled to scalars.
- A naked type, i.e., just a type name, without parentheses. The type must
  implement 'FromSqlRow'; the return type will be a 'Maybe' of that type. E.g.,
  @(:: User)@ will produce a function signature ending in
  @conn -> IO (Maybe User)@.
- A list of tuples or naked types, written using square brackets (@[@ ... @]@),
  returning a list of mapped rows instead of a 'Maybe'.

Note that, unlike Haskell, YeshQL distinguishes @(Foo)@ from @Foo@: the former
takes the first column from a result row and maps it using 'FromSql', while the
latter takes the entire result row and maps it using 'FromSqlRow'.

@
    -- :paramName :: Type
@

Declares a Haskell type for a parameter. The parameter @:paramName@ can then
be referenced zero or more times in the query itself, and will appear in the
generated function signature in the order of declaration. So in the above
example, the last query definition:

@
    -- name:getUserEx :: (Integer, String)
    -- :id :: Integer
    -- :filename :: String
    SELECT id, name FROM users WHERE name = :filename OR id = :id;
@

...will produce the function:

@
getUserEx :: IConnection conn => Integer -> String -> conn -> IO [(Integer, String)]
getUserEx id filename conn =
    -- ... generated implementation left out
@

On top of referencing parameters directly, you can also "drill down" with a
projection function, using @.@ syntax similar to property access in, say,
JavaScript. The intended use case is passing record types as arguments to
the query function, and then dereferencing them inside the query, like so:

@
    -- name:updateUser :: rowcount Int
    -- :user :: User
    UPDATE users
    SET username  = :user.name
    WHERE id = :user.userID
@

Note that the part after the @.@ is a plain Haskell function that must be in
scope wherever the query is spliced.

Also note that projection functions can be chained, and are not limited to
record field accessors.

== Loading Queries From External Files

The 'yeshFile' and 'yesh1File' flavors take a file name instead of SQL
definition strings. Using these, you can put your SQL in external files rather
than clutter your code with long quasi-quotation blocks.

== DDL Queries

Normally, you will have one query per function, and that query takes some
parameters, and returns a result set or a row count. For DDL queries, however,
we aren't interested in the results, and we often want to execute multiple
queries with just one function call, e.g. to set up an entire database schema
using multiple @CREATE TABLE@ statements.

By adding the @\@ddl@ annotation to a query definition, YeshQL will change the
following things:

- The return type of that query, regardless of what you declare, will be '()'.
  It is recommended to never declare an explicit return type other than '()'
  for DDL queries, as future versions may report this as an error.
- The query cannot accept any parameters.
- The query may consist of multiple individual SQL queries,
  semicolon-separated. The combined query is sent to the HDBC backend as-is.
- Instead of 'run', YeshQL will use 'runRaw' in the code it generates.

In practice, this means that the type of a DDL function thus generated will
always be @'IConnection' conn => conn -> 'IO' ()@.

=== Example:

@
[yesh1|
    -- name:makeDatabaseSchema
    -- @ddl
    CREATE TABLE users (id INTEGER, username TEXT);
    CREATE TABLE pages (id INTEGER, title TEXT, slug TEXT, body TEXT);
    |]
@

== Other Functions That YeshQL Generates

On top of the obvious query functions, a top-level YeshQL quasiquotation
introduces two more definitions per query: a 'String' variable prefixed
@describe-@, which contains the SQL query as passed to HDBC (similar to the
@DESCRIBE@ feature in some RDBMS systems), and another 'String' variable
prefixed @doc-@, which contains all the free-form comments that precede the SQL
query in the query definition.

So for example, this quasiquotation:

@
[yesh1|
    -- name:getUser :: User
    -- :userID :: Integer
    -- Gets one user by the "id" column.
    SELECT id, username FROM users WHERE id = :userID LIMIT 1 |]
@

...would produce the following three top-level definitions:

@
getUser :: IConnection conn => Integer -> conn -> IO (Maybe User)
getUser userID conn = ...

describeGetUser :: String
describeGetUser = \"SELECT id, username FROM users WHERE id = ? LIMIT 1\"

docGetUser :: String
docGetUser = \"Gets one user by the \\\"id\\\" column.\"
@

 -}
module Database.YeshQL
(
-- * Quasi-quoters that take strings
  Yesh (..)
-- * Quasi-quoters that take filenames
, YeshFile (..)
-- * Query parsers
, parseQuery
, parseQueries
-- * AST
, ParsedQuery (..)
)
where

import Database.YeshQL.Core
import Database.YeshQL.Backend
import Database.YeshQL.Parser
import Database.YeshQL.HDBC
import Database.YeshQL.HDBC.SqlRow.Class
