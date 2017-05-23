# Formatting

[![Build Status](https://travis-ci.org/krisajenkins/purescript-formatting.svg?branch=master)](https://travis-ci.org/krisajenkins/purescript-formatting)

A type-safe string formatting library, based on [Chris Done's Haskell version](https://hackage.haskell.org/package/formatting).

It fulfils the need for string-interpolation or a `printf` function,
without sacrificing type safety or requiring any special
language-level support.

It also composes well, making it easy to build up complex, reusable formatters.

## Documentation

Module documentation is published on Pursuit: [http://pursuit.purescript.org/packages/purescript-formatting](http://pursuit.purescript.org/packages/purescript-formatting)

## Credits

This package is a port of [Chris Done's Formatting][formatting] library for
Haskell.

[formatting]: http://chrisdone.com/posts/formatting

## Contributing

More specialised formatters (like "print to a certain number of
decimal places") would be welcome.

For more advanced help, I have a suspicion that `before` and `after`
have a proper home in some category somewhere, and I'd like to find
out where it is. Also, I suspect we can relax the composition
restraint from `Format String` to `forall m. Semigroupoid m => Format m`.
If you know how, I'd love your input.

## See Also

A port of this library is also [available for Elm](http://package.elm-lang.org/packages/krisajenkins/formatting/latest).

## License

Copyright © 2017 Kris Jenkins

Distributed under the MIT license.
