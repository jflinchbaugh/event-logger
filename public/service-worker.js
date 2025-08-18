const urlParams = new URLSearchParams(self.location.search);
const BUILD_ID = urlParams.get('v') || 'v0'; // Default to v0 if not found
const CACHE_NAME = `event-logger-cache-${BUILD_ID}`;

const urlsToCache = [
  'index.html',
  'css/style.css',
  'js/main.js',
  'event-logger.png'
];

self.addEventListener('install', event => {
  event.waitUntil(
    caches.open(CACHE_NAME)
      .then(cache => {
        return cache.addAll(urlsToCache);
      })
  );
});

self.addEventListener('fetch', event => {
  console.log('requested ' + event.request);
  event.respondWith(
    caches.match(event.request)
      .then(response => {
        if (response) {
          return response;
        }
        return fetch(event.request);
      })
  );
});

self.addEventListener('activate', event => {
  const cacheWhitelist = [CACHE_NAME];
  event.waitUntil(
    caches.keys().then(cacheNames => {
      return Promise.all(
        cacheNames.map(cacheName => {
          if (cacheWhitelist.indexOf(cacheName) === -1) {
            return caches.delete(cacheName);
          }
        })
      );
    })
  );
});
