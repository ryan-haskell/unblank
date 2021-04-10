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
const clip = (filename, vol = 0.75) => {
  const audio = new Audio(`/audio/clips/${filename}`)
  audio.volume = vol
  return audio
}

const audio = {
  tracks: [{
      track: new Audio('/audio/music/track_0.ogg'),
      volume: 0.1
    }, {
      track: new Audio('/audio/music/track_1.ogg'),
      volume: 0.05
    }, {
      track: new Audio('/audio/music/track_2.ogg'),
      volume: 0.1
    }
  ],
  villagers: [
    clip('lady/lady-1.mp3'),
    clip('lady/lady-2.mp3'),
    clip('lady/lady-3.mp3'),
    clip('lady/lady-4.mp3')
  ],
  dhruv: {
    pre: [
      clip('dhruv/dhruv_0.ogg'),
      clip('dhruv/dhruv_1.ogg'),
      clip('dhruv/dhruv_2.ogg'),
    ],
    post: [
      clip('dhruv/dhruv_post_0.ogg'),
      clip('dhruv/dhruv_post_1.ogg'),
      clip('dhruv/dhruv_post_2.ogg'),
    ]
  },
  scott: {
    pre: [
      clip('scott/Scott-1a.mp3'),
      clip('scott/Scott-1b.mp3'),
      clip('scott/Scott-2a.mp3'),
      clip('scott/Scott-2b.mp3'),
    ],
    post: [
      clip('scott/Scott-3.mp3'),
      clip('scott/Scott-4a.mp3'),
      clip('scott/Scott-4b.mp3'),
    ]
  },
  boss: {
    taunt: [
      clip('boss/boss_1.ogg'),
      clip('boss/boss_2.ogg'),
      clip('boss/boss_3.ogg'),
      clip('boss/boss_4.ogg'),
      clip('boss/boss_5.ogg'),
      clip('boss/boss_6.ogg'),
    ]
  },
  kelch: {
    playing: false,
    taunts: [
      clip('kelch/NotSoFast.mp3', 1),
      clip('kelch/WhySoSlow.mp3', 1),
      clip('kelch/YoullHaveToBeQuicker.mp3', 1),
      clip('kelch/YouMissed.mp3', 1),
      clip('kelch/YourGoingDown.mp3', 1),
    ],
    dead: clip('kelch/YouKilledMe.mp3', 1)
  },
  nick: {
    playing: false,
    taunts: [
      clip('nick/taunt_0.mp3', 1),
      clip('nick/taunt_1.mp3', 1),
      clip('nick/taunt_2.mp3', 1)
    ],
    dead: clip('nick/ded.mp3', 1)
  }
}

// Background music

let currentTrack = 0
audio.tracks.forEach((t, i) => {
  t.track.loop = true;
  t.track.volume = (i === currentTrack) ? t.volume : 0
})

const play = () => audio.tracks.forEach(t => t.track.play())
const pause = () => audio.tracks.forEach(t => t.track.pause())
const fadeIn = (new_) => {
    const newTrack = audio.tracks[new_]
    currentTrack = new_
    const delta = 0.001
    const fade = _ => {
      newTrack.track.volume = Math.min(newTrack.volume, newTrack.track.volume + delta)
      if (newTrack.track.volume < newTrack.volume) { window.requestAnimationFrame(fade) }
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
  "dhruv": () => playClip(audio.dhruv.pre),
  "dhruvPost": () => playClip(audio.dhruv.post),
  "scottPre": () => playClip(audio.scott.pre),
  "scottPost": () => playClip(audio.scott.post),
  "kelchTaunt": () => playBossTaunt(audio.kelch, 10000),
  "kelchKilled": () => playBossKilled(audio.kelch),
  "nickTaunt": () => playBossTaunt(audio.nick),
  "nickKilled": () => playBossKilled(audio.nick),
  "talk": () => playClip(audio.villagers)
})[msg] || (() => {}))())

const playBossTaunt = (boss, delay = 3000) => {
  if (boss.playing) return
  boss.playing = true
  const taunt = pickRandom(boss.taunts)
  taunt.play()
  setTimeout(_ => { boss.playing = false }, delay)
}

const playBossKilled = (boss) => {
  boss.taunts.forEach(t => t.pause())
  boss.dead.play()
}
