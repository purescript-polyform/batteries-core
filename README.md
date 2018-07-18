# purescript-polyform-validators

Set of useful validators which are build on top of `Validation` from purescript-polyform.

## Usage

`Validation` type from polyform has Applicative and Category instance. Using these two interfaces and provided validators you can quite easily write whole validation chain: from ajax request to resulting object. This validation flow will aggregate in typed manner all encoutered errors at given failing step.

Let's look at real example - here we are making a request to shutterstock API and validating its response. `Data.Record.Fold.collect` is like sequence but over a record (so it takes a record of `Validation`s per field and returns `Validation` which result is record):

``` purescript
type SearchResult image =
  { page ∷ Int
  , perPage ∷  Int
  , totalCount ∷ Int
  , searchId ∷ String
  , photos ∷ Array image
  }

searchResult
  :: forall err m
   . Monad m
  => Validation m
      (Array (Variant (JsError err)))
      Json
      (SearchResult Image)
searchResult = collect
  { page: field "page" int
  , perPage: field "per_page" int
  , totalCount: field "total_count" int
  , searchId: field "search_id" string
  , photos: field "data" $ arrayOf image
  }
```

In above example `image` is another `Validation` from `Json` into `Image`.

Now using `affjaxJson` validator from this library we can chain these two validators to get fully validated request:

``` purescript
searchValidation
  :: forall ext err
   . Validation
      ( Aff( ajax :: AJAX| ext))
      (Array(Variant(SearchErrorRow err)))
      (AffjaxRequest Unit)
      (SearchResult Image)
searchValidation = searchResult <<< affjaxJson
```

### More Examples

For more usage examples please refer these simple libraries: [purescript-shutterstock](https://github.com/lambdaterms/purescript-shutterstock), [purescript-pexels](https://github.com/lambdaterms/purescript-pexels) or [purescript-nasa-images](https://github.com/lambdaterms/purescript-nasa-images).
