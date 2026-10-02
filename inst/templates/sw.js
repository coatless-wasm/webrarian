/*
 * webrarian service worker
 * ========================
 *
 * Emitted into the built site by `webrarian::bind()` only when `_webrarian.yml`
 * opts in:
 *
 *     build:
 *       service-worker: true
 *
 * Why it exists
 * -------------
 * A webrarian site ships a multi-megabyte webR runtime (the engine under
 * webr/v<version>/) plus the vendored viewer bundle. On a host that can set
 * response headers, the `_headers` file that `bind()` writes marks those as
 * immutable and the browser HTTP cache does the work. GitHub Pages cannot set
 * custom headers at all, so this worker is the only way a returning visitor
 * there avoids re-downloading the runtime. It speeds up repeat visits; it
 * does not make the site work offline (webR's lazy file system also issues
 * HEAD requests, which this worker leaves to the network). When the option is
 * turned off, `bind()` ships a retirement worker (sw-retire.js) in its place.
 *
 * Cross-origin isolation - read before editing
 * --------------------------------------------
 * webR needs SharedArrayBuffer, which browsers only expose to a
 * cross-origin-isolated page: the document must arrive with
 * `Cross-Origin-Opener-Policy: same-origin` and
 * `Cross-Origin-Embedder-Policy: require-corp`, and its subresources must be
 * CORP-compatible. A service worker is the classic way to destroy that by
 * accident, because a Response the worker *synthesizes* (`new Response(...)`)
 * carries none of those headers.
 *
 * This worker therefore never constructs a Response. Every reply is either
 *
 *   - the untouched Response returned by `fetch()`, or
 *   - a Response replayed verbatim out of the Cache Storage API,
 *
 * and Cache Storage preserves the headers a response was stored with - COOP,
 * COEP, CORP and Content-Type included - so a replayed engine asset is
 * byte-for-byte and header-for-header what the network served. When neither is
 * available the fetch rejection propagates so the browser reports it, rather
 * than being papered over with a synthetic error page that would drop
 * isolation for the document.
 *
 * Requests the worker does not recognize get no `respondWith()` at all, which
 * takes the worker out of the loop entirely and lets the browser fetch them
 * exactly as it would with no worker installed.
 */

// Substituted by bind(); unique per build so a redeploy cannot be served from
// the previous build's cache.
const BUILD_ID = "{{build_id}}";

// Cache Storage is keyed per *origin*, so two webrarian sites under one origin
// (say two GitHub Pages projects on the same user site) share it. Scoping the
// prefix to this registration keeps activate() from evicting the other site's
// entries.
const CACHE_PREFIX = "webrarian:" + self.registration.scope + ":";
const CACHE_NAME = CACHE_PREFIX + BUILD_ID;

// Cache-first. Only URLs whose bytes can never change: the engine under
// webr/v<version>/ (a new webr.version is a new directory) and the viewer
// bundle named after its content hash. `/vfs-files/` (the user's files) and
// `/repo/` (packages that can be rebuilt at the same version) are
// deliberately NOT here - see NETWORK_FIRST.
const CACHE_FIRST = [
  /\/webr\/v[^/]+\/.+$/,
  /\/exlibris-r\.[0-9a-f]{8}\.(?:js|css)$/,
  // The package library image (build.library-image), also named after its content hash.
  /\/library\/library-[0-9a-f]{8}\.tgz$/
];

// Network-first, falling back to the cached copy only when the network fails.
// These change on every `bind()` at a stable URL, so a redeploy has to win.
// Navigations are handled here too (see the fetch handler) because index.html
// carries the inlined viewer config.
const NETWORK_FIRST = [
  /\/index\.html$/,
  /\/vfs-files\/.+$/,
  /\/repo\/.+$/
];

function matchesAny(patterns, path) {
  for (let i = 0; i < patterns.length; i++) {
    if (patterns[i].test(path)) return true;
  }
  return false;
}

// Store a response for later replay. Deliberately tolerant: caching is an
// optimization, never a correctness requirement, so every failure is swallowed
// and the live response is still returned to the page.
async function putInCache(request, response) {
  // - status 200 only: cache.put() rejects 206 partial responses outright.
  // - type "basic" only: an opaque cross-origin response cannot be used under
  //   COEP: require-corp, so caching one would be worse than useless.
  if (!response || response.status !== 200 || response.type !== "basic") return;
  try {
    const cache = await caches.open(CACHE_NAME);
    await cache.put(request, response);
  } catch (err) {
    // Quota exhausted, storage disabled, private mode, ... - ignore.
  }
}

async function cacheFirst(event, request) {
  const cached = await caches.match(request, { cacheName: CACHE_NAME });
  if (cached) return cached;

  // `cache: "no-cache"`: fill this build's cache from the origin (a 304 when
  // unchanged), never from the browser HTTP cache, which may still hold the
  // previous build's copy.
  const response = await fetch(request, { cache: "no-cache" });
  // clone() before the body is handed to the page - a body can only be read
  // once. waitUntil keeps the worker alive until the copy is written; it is
  // called before respondWith()'s promise settles, so the event is still
  // active.
  keepAlive(event, putInCache(request, response.clone()));
  return response;
}

async function networkFirst(event, request) {
  try {
    const response = await fetch(request);
    keepAlive(event, putInCache(request, response.clone()));
    return response;
  } catch (err) {
    const cached = await caches.match(request, { cacheName: CACHE_NAME });
    if (cached) return cached;
    // No cached copy: let the original network failure surface. Returning a
    // synthetic Response here would hand the document a reply with no
    // COOP/COEP and silently break SharedArrayBuffer.
    throw err;
  }
}

function keepAlive(event, promise) {
  try {
    event.waitUntil(promise);
  } catch (err) {
    // waitUntil() throws if the event is no longer active; the write still
    // proceeds on a best-effort basis.
  }
}

self.addEventListener("install", () => {
  // A new bind() means a new BUILD_ID and an empty cache; there is nothing to
  // gain from waiting for every old tab to close first.
  self.skipWaiting();
});

self.addEventListener("activate", (event) => {
  event.waitUntil((async () => {
    const names = await caches.keys();
    await Promise.all(names.map((name) => {
      // Only ever delete *this* deployment's older builds.
      if (name.startsWith(CACHE_PREFIX) && name !== CACHE_NAME) {
        return caches.delete(name);
      }
      return Promise.resolve();
    }));
    await self.clients.claim();
  })());
});

self.addEventListener("fetch", (event) => {
  const request = event.request;

  // Anything that is not a plain same-origin GET goes straight to the network
  // with the worker uninvolved. Range requests in particular must not be
  // intercepted: they produce 206s that Cache Storage refuses to store.
  if (request.method !== "GET") return;
  if (request.headers.has("range")) return;

  let url;
  try {
    url = new URL(request.url);
  } catch (err) {
    return;
  }
  if (url.origin !== self.location.origin) return;

  const path = url.pathname;

  if (request.mode === "navigate" || matchesAny(NETWORK_FIRST, path)) {
    event.respondWith(networkFirst(event, request));
    return;
  }

  if (matchesAny(CACHE_FIRST, path)) {
    event.respondWith(cacheFirst(event, request));
  }

  // Everything else: no respondWith(), so the browser fetches it normally.
});
