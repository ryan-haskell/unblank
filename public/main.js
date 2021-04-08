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
const clip = (filename) => {
  const audio = new Audio(`/audio/clips/${filename}`)
  audio.volume = 0.5
  return audio
}
const audio = {
  music: new Audio('/audio/music/black_and_white.ogg'),
  villagers: [
    clip('lady/lady-1.mp3'),
    clip('lady/lady-2.mp3'),
    clip('lady/lady-3.mp3'),
    clip('lady/lady-4.mp3')
  ]
}
let playingClip = false

const pickRandom = (list = []) => list[parseInt(Math.random()*list.length)]

audio.music.volume = 0.01
audio.music.loop = true
app.ports && app.ports.outgoing && app.ports.outgoing.subscribe(msg => (({
  "play": () => audio.music.play(),
  "pause": () => audio.music.pause(),
  "talk": () => playingClip ? null : (
    playingClip = true,
    setTimeout(_ => { playingClip = false }, 1000),
    pickRandom(audio.villagers).play()
  )
})[msg] || (() => {}))())