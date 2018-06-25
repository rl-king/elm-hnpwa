# Elm Hacker News Progressive Web App
A [HNPWA](https://hnpwa.com) implementation written in [Elm 0.19](http://elm-lang.org)

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
- Completely written in Elm 0.19
- Cache requested resources in session
- Service worker for offline use

## Develop
Install Elm, Elm-format and other dependencies
```
make deps
```
Start dev server 
```
make watch
```
Build, compile optimized Elm, generate Service Worker and minify assets
```
make build
```
