# Event Logger

## Underlying Tech
- [Shadow-CLJS](http://shadow-cljs.org/) as the build tool / compiler
- [Reagent](https://github.com/reagent-project/reagent) (CLJS wrapper around [React](https://reactjs.org/)) for building your user interface

## Getting Started

### Install Dependencies

Note: This step creates a `node_modules` folder with all the dependencies in your project folder. You can use either `yarn` or `npm` as your package manager.

```
npm install
```

### Start the App

```
npm start
```

### Open Your Browser

Then open http://localhost:3000/ in your browser to see your app.

## Available Scripts

### Start App

This will compile the app in development mode, and watch for any changes in your code.
Open [http://localhost:3000](http://localhost:3000) to view the app in the browser.

```
npm start
```

This operation creates a `.shadow-cljs` folder in your project folder.

### Build Release Version of App

This compiles the app in production mode, using `:advanced` compilation settings. The finished build (under `public/js`) will be in the `public` folder, which can be deployed.

```
npm run build
```

This operation creates a `.shadow-cljs` folder in your project folder.

### Debug Release Version of App

Sometimes you may run into some release issues due to `:advanced` compilation. The following command may help you track down the causes:

```
npm run debug-build
```

### Show Detailed Build Report of Release Version of App

A detailed build report will be shown in your CLI, and a `report.html` file will be created in your project folder.

```
npm run report
```

### Serve Your App Build Locally

This will serve your finished build (from doing a production build via `yarn build` or `npm run build`, or from doing a development build via `yarn devbuild` or `npm run devbuild`) on [http://localhost:5000](http://localhost:5000) in your browser.

```
npm run serve
```

### Build Development Version of App

This compiles the app in developent mode once and then exits. The finished build will be in the `public` folder. This command does _not_ watch your code for changes (use `yarn start` or `npm start` to build the app in development mode _and_ watch for changes).

```
npm run dev-build
```

This operation creates a `.shadow-cljs` folder in your project folder.

### Connect to a Build-specific Browser REPL

From a different Terminal, connect to a browser REPL for your specific build (only available while `yarn start` or `npm start` is running, that is, Shadow-CLJS is “watching” your code for changes). Note also that your build must be running in the browser (`localhost:3000`).

See [this section](https://shadow-cljs.github.io/docs/UsersGuide.html#build-repl) of the Shadow-CLJS documentation for more details.

```
npm run repl
```

### Connect to a Blank Browser REPL

This starts a blank CLJS REPL and will open an associated browser window where the code will execute. It is not connected to any specific build target. Since it is not connected to any build it does not do any automatic rebuilding of code when your files change and does not provide hot-reload. If you close the browser window the REPL will stop working.

See [this section](https://shadow-cljs.github.io/docs/UsersGuide.html#browser-repl) of the Shadow-CLJS documentation for more details.

```
npm run browser-repl
```

### Connect to a Clojure REPL

A Clojure REPL is also provided in addition to the provided ClojureScript REPLs. This is can be used to control the shadow-cljs process and run all other build commands through it. You can start with a Clojure REPL and then upgrade it to a CLJS REPL at any point (and switch back).

See [this section](https://shadow-cljs.github.io/docs/UsersGuide.html#_clojure_repl) of the Shadow-CLJS documentation for more details.

```
npm run clojure-repl
```

### Compile tests and watch for changes

(Run in a separate Terminal.) This runs the tests and watches them for changes, re-running when a change is detected.

```
npm test
```

Note: Creates an `out` folder in your project folder, containing a `node-tests.js` file.

### Compile tests and run them once

(Run in a separate Terminal.) This runs the tests once and then exits. This command does _not_ watch your tests for changes (use `npm test` to run tests _and_ watch for changes).

```
npm run test-once
```

Note: Creates an `out` folder in your project folder.

### Remove Generated JS Code (“Clean”)

Remove (“clean”) the `public/js` folder and contents generated during compilation.

```
npm run clean
```

### Remove All Generated Code and Dependencies (“Nuke”)

Remove all (“nuke”) of the following:

- `public/js` folder and contents
- `.shadow-cljs` folder and contents
- `node_modules` folder and contents
- `package-lock.json` file (or `yarn.lock` file, if you specified the `yarn` option for your package manager)
- `out` folder and contents (containing tests)
- `report.html` file showing release build details

```
npm run nuke
```

Note that after this operation you will need to run `npm install` again before starting the app, to re-install the dependencies.

### Run a Shadow-CLJS Server

Shadow-CLJS can be fairly slow to start. To improve this Shadow-CLJS can run in “server mode” which means that a dedicated process is started which all other commands can use to execute a lot faster since they won’t have to start a new JVM/Clojure instance.

You can run the process in the foreground in a dedicated Terminal. Use CTRL+C to terminate the server.

```
npm run shadow-cljs-server
```

### Emacs Can Jack-In Just Fine

