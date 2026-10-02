/*
 * webrarian service worker: retirement
 * ====================================
 *
 * Emitted as `sw.js` by `webrarian::bind()` whenever `build: service-worker`
 * is off, and by `webrarian::collection_mirror()`, whose mirrors never have a
 * worker. A visitor whose browser never registered a worker never fetches
 * this file. A visitor whose browser still runs the worker from an earlier
 * build fetches it on its next update check; it then deletes the caches that
 * worker created for this site, unregisters itself, and reloads the open
 * pages so they are served straight from the network again.
 *
 * Like the caching worker, it never constructs a Response, and it has no
 * fetch handler at all: requests go to the network untouched.
 */

const CACHE_PREFIX = "webrarian:" + self.registration.scope + ":";

self.addEventListener("install", () => {
  self.skipWaiting();
});

self.addEventListener("activate", (event) => {
  event.waitUntil((async () => {
    const names = await caches.keys();
    await Promise.all(names.map((name) => {
      if (name.startsWith(CACHE_PREFIX)) {
        return caches.delete(name);
      }
      return Promise.resolve();
    }));
    await self.registration.unregister();
    const windows = await self.clients.matchAll({ type: "window" });
    windows.forEach((client) => client.navigate(client.url));
  })());
});
