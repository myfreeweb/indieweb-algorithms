# indieweb-algorithms [![Hackage](https://img.shields.io/hackage/v/indieweb-algorithms.svg?style=flat)](https://hackage.haskell.org/package/indieweb-algorithms) [![Build Status](https://img.shields.io/travis/myfreeweb/indieweb-algorithms.svg?style=flat)](https://travis-ci.org/myfreeweb/indieweb-algorithms) [![unlicense](https://img.shields.io/badge/un-license-green.svg?style=flat)](http://unlicense.org)

A collection of implementations of [IndieWeb]- and [Microformats 2]-related algorithms (based on [microformats2-parser] and [http-link-header]):

- finding all microformats of a given type (while retaining the path to them), ie. flattening the tree
- discovering **[authorship](http://indiewebcamp.com/authorship)** of an `h-entry`
- discovering [Webmention](http://indiewebcamp.com/Webmention)/[Micropub](http://indiewebcamp.com/Micropub)/[IndieAuth](http://indiewebcamp.com/IndieAuth)/etc. **endpoints** (HTTP `Link` header, `a` and `link` tags with the `rel` attribute)
- parsing tweets on Twitter.com into microformats (like [php-mf2-shim](https://github.com/indieweb/php-mf2-shim))

[IndieWeb]: https://indiewebcamp.com
[Microformats 2]: http://microformats.org/wiki/microformats2
[microformats2-parser]: https://github.com/myfreeweb/microformats2-parser
[http-link-header]: https://github.com/myfreeweb/http-link-header

## Usage

### Endpoints

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Link
import Data.Default
import Data.Maybe
import Data.Microformats2.Parser
import Data.IndieWeb.Endpoints

discoverEndpoints [ "micropub" ] (parseMf2 def $ documentRoot $ parseLBS "<link rel=micropub href='http://example.com/micropub2'>...") (fromMaybe [] $ parseLinkHeader "<http://example.com/micropub>; rel=\"micropub\"")
```

## Development

Use [stack] to build.  
Use ghci to run tests quickly with `:test` (see the `.ghci` file).

```bash
$ stack build

$ stack test && rm tests.tix

$ stack ghci --ghc-options="-fno-hpc"
```

[stack]: https://github.com/commercialhaskell/stack

## Contributing

Please feel free to submit pull requests!
Bugfixes and simple non-breaking improvements will be accepted without any questions :-)

By participating in this project you agree to follow the [Contributor Code of Conduct](http://contributor-covenant.org/version/1/2/0/).

[The list of contributors is available on GitHub](https://github.com/myfreeweb/indieweb-algorithms/graphs/contributors).

## License

This is free and unencumbered software released into the public domain.  
For more information, please refer to the `UNLICENSE` file or [unlicense.org](http://unlicense.org).
