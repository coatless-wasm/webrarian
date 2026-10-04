# The webR JavaScript client in the viewer bundle

`inst/viewer/exlibris-r.js` (exlibris v0.1.2, commit `e4824c8cc923ef82e7ada0292952abbbecbd2367`)
contains the webR JavaScript client, `webr` 0.6.0 from
[r-wasm/webr](https://github.com/r-wasm/webr). webR's `LICENSE.md` puts the
JavaScript client under the MIT license, reproduced below; its GNU GPL v3 text
covers the webR distribution binaries, the WebAssembly build of R and the GPL
software compiled into it.

This package does not contain those binaries. `bind()` copies them into every
site unless `build.bundle-engine` is `false` (an offline site, `build.offline:
true`, always has them), and `collection_mirror()` into every mirror; each such
site records them, with the webR and R source links, in its `LICENSES/webR.md`.
A site built with `build.bundle-engine: false` loads them from
<https://webr.r-wasm.org/> instead.

## The MIT Licence

Copyright (c) 2023 webR authors

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
