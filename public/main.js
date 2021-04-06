const app = Elm.Main.init({
  flags: {
    window: {
      width: window.innerWidth,
      height: window.innerHeight
    }
  }
})

// Disable right click menu
document.addEventListener('contextmenu', event => event.preventDefault())

// Handle ports
const audio = new Audio('/loop.ogg')
audio.volume = 0.025
app.ports && app.ports.outgoing && app.ports.outgoing.subscribe(msg => (({
  "play": () => audio.play(),
  "pause": () => audio.pause()
})[msg] || (() => {}))())