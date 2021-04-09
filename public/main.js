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
  tracks: [
    new Audio('/audio/music/track_0.ogg'),
    new Audio('/audio/music/track_1.ogg'),
    new Audio('/audio/music/track_2.ogg'),
  ],
  villagers: [
    clip('lady/lady-1.mp3'),
    clip('lady/lady-2.mp3'),
    clip('lady/lady-3.mp3'),
    clip('lady/lady-4.mp3')
  ],
  dhruv: [
    clip('hows_it_going.ogg')
  ],
  kelch: {
    playing: false,
    taunts: [
      clip('kelch/NotSoFast.mp3'),
      clip('kelch/WhySoSlow.mp3'),
      clip('kelch/YoullHaveToBeQuicker.mp3'),
      clip('kelch/YouMissed.mp3'),
      clip('kelch/YourGoingDown.mp3'),
    ],
    dead: clip('kelch/YouKilledMe.mp3')
  }
}

// Background music

let currentTrack = 0
const volume = { music: 0.25 }
audio.tracks.forEach((t, i) => {
  t.loop = true;
  t.volume = (i === currentTrack) ? volume.music : 0
})

const play = () => audio.tracks.forEach(t => t.play())
const pause = () => audio.tracks.forEach(t => t.pause())
const fadeIn = (new_) => {
    const newTrack = audio.tracks[new_]
    currentTrack = new_
    const delta = 0.003
    const fade = _ => {
      newTrack.volume = Math.min(volume.music, newTrack.volume + delta)
      if (newTrack.volume < volume.music) { window.requestAnimationFrame(fade) }
    }
    window.requestAnimationFrame(fade)
}
// Voice clips
let playingClip = false
const pickRandom = (list = []) => list[parseInt(Math.random()*list.length)]

const playClip = (clip) => playingClip ? null : (
  playingClip = true,
  setTimeout(_ => { playingClip = false }, 1000),
  pickRandom(clip).play()
)

app.ports && app.ports.outgoing && app.ports.outgoing.subscribe(msg => (({
  "play": () => play(),
  "pause": () => pause(),
  "next": () => fadeIn(currentTrack + 1),
  "dhruv": () => playClip(audio.dhruv),
  "kelchTaunt": () => playBossTaunt(audio.kelch),
  "kelchKilled": () => playBossKilled(audio.kelch),
  "talk": () => playClip(audio.villagers)
})[msg] || (() => {}))())

const playBossTaunt = (boss) => {
  if (boss.playing) return
  boss.playing = true
  const taunt = pickRandom(boss.taunts)
  taunt.play()
  setTimeout(_ => { boss.playing = false }, 10000)
}

const playBossKilled = (boss) => {
  boss.taunts.forEach(t => t.pause())
  boss.dead.play()
}