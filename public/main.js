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
const audio = {
  music: new Audio('/audio/music/blankworld.ogg'),
  dhruv: {
    howsItGoin: new Audio('/audio/clips/hows_it_going.ogg')
  }
}
audio.music.volume = 0
audio.music.loop = true
audio.dhruv.howsItGoin.volume = 0
app.ports && app.ports.outgoing && app.ports.outgoing.subscribe(msg => (({
  "play": () => audio.music.play(),
  "pause": () => audio.music.pause(),
  "talk": () => audio.dhruv.howsItGoin.play()
})[msg] || (() => {}))())