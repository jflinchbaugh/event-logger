## Run the App for Dev and Tests in Browser
```
$ npx shadow-cljs watch frontend test-browser
```

- Open a browser to http://localhost:3021/ to view the test results
- Open a browser to http://localhost:3000/index.html
- Everything else through http://localhost:3000/
  will proxy to http://localhost:8000/ to find the storage server
  and avoid CORS errors.

## Compile the App for Production
```
$ npx shadow-cljs compile frontend
```

## In Emacs
`cider-jack-in-cljs` -> `shadow`  -> `:frontend`

It'll give you a cljs repl in emacs
and launch the browser view of the app.

## Tests in Browser
- `npx shadow-cljs watch test-browser`
