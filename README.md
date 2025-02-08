## Run the App for Dev
```
$ npx shadow-cljs watch frontend
```

## Compile the App for Production
```
$ npx shadow-cljs compile frontend
```

## In Emacs
`cider-jack-in-cljs` -> `shadow`  -. `:frontend`

It'll give you a cljs repl in emacs
and launch the browser view of the app.

## Tests in Browser
- `npx shadow-cljs watch test-browser`
- open a browser to http://localhost:3021/
