const app = Elm.Main.init({
  flags: {
    window: {
      width: window.innerWidth,
      height: window.innerHeight
    },
    now: Date.now()
  }
})

// Disable right click menu
document.addEventListener('contextmenu', event => event.preventDefault())