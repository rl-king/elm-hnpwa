# Elm Hacker News Progressive Web App
A [HNPWA](https://hnpwa.com) implementation written in [Elm 0.18](http://elm-lang.org)

<p align="center">
    <a href="https://elm-hnpwa.firebaseapp.com" rel="noopener" target="_blank">
        <img alt="Site screenshot" src="elm-mobile.png" height="400">
    <br>
        Elm HNPWA
    </a> /
    <a href="https://elm-hnpwa-debug.firebaseapp.com/" rel="noopener" target="_blank">
        Elm HNPWA with time-traveling debugger
    </a>
</p>

## Perfomance
- Lighthouse: 91/100
- Interactive (Emerging Markets): 4.3s
- Interactive (Faster 3G): 3.5s

## Features
- Completely written in Elm 0.18
- Cache requested resources in session
- Service worker for offline use

## Develop
Install Elm, Elm-format and Webpack dependencies
```
npm install
```
Start Webpack dev server with HMR

```
npm start
```
Run Webpack build

```
npm run build
```

## Todo
- SSR (Waiting for Elm 0.19)
