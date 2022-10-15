package net.soundmining

import net.soundmining.Generative.{pickItems, randomRange}
import net.soundmining.modular.ModularSynth.{lineControl, relativePercControl, staticControl}
import net.soundmining.modular.SynthPlayer
import net.soundmining.synth.SuperColliderClient.loadDir
import net.soundmining.synth.{Instrument, Patch, PatchPlayback, SuperColliderClient, SuperColliderReceiver}
import java.awt.{Dimension, Graphics}
import javax.swing.{JFrame, JPanel, JScrollPane, SwingUtilities, WindowConstants}
import scala.collection.mutable
import scala.util.Random

object AmbientMusic5 {
  implicit val client: SuperColliderClient = SuperColliderClient()
  val SYNTH_DIR = "/Users/danielstahl/Documents/Projects/soundmining-modular/src/main/sc/synths"
  val synthPlayer = SynthPlayer(soundPlays = Map.empty, numberOfOutputBuses = 2, bufferedPlayback = false)

  var patchPlayback: PatchPlayback = PatchPlayback(patch = FmHarmony2Patch, client = client)
  val superColliderReceiver: SuperColliderReceiver = SuperColliderReceiver(patchPlayback)

  implicit val random: Random = new Random()

  def init(): Unit = {
    println("Starting up SuperCollider client")
    client.start()
    Instrument.setupNodes(client)
    client.send(loadDir(SYNTH_DIR))
    synthPlayer.init()
    superColliderReceiver.start()
  }

  object FmHarmony2Patch extends Patch {

    val fundamentalNote = "e3"
    val firstPartialNote = "fiss4"

    val fundamental = Note.noteToHertz(fundamentalNote)
    val firstPartial = Note.noteToHertz(firstPartialNote)

    val fact = Spectrum.makeFact(fundamental, firstPartial)
    val spectrum = Spectrum.makeSpectrum2(fundamental, fact, 50)

    val invertedFact = fundamental / firstPartial
    val lowerSpectrum = Spectrum.makeSpectrum2(fundamental / math.pow(fact, 7), invertedFact, 50)

    override def noteHandle(start: Double, key: Int, velocity: Int, device: String): Unit = {
      val note = key % 12
      val octave = (key / 12) - 1
      val amp = velocity / 127.0

      println(s"Spectrum ${spectrum.zipWithIndex}")

      // 0, 4 5, 10 11

      octave match {
        case 1 =>
          playSubBass(start, note, amp)
        case 2 =>
          playLow(start, note, amp)
        case 3 =>
          playMiddle(start, note, amp)
        case 4 =>
          playMiddleHigh(start, note, amp)
        case 5 =>
          playHigh(start, note, amp)
      }
    }

    def playPiece(start: Double = 0, reset: Boolean = true): Unit = {
      val uiBuilder: Seq[(String, mutable.Buffer[UiNote])] = Seq(
        ("High", mutable.Buffer()),
        ("Middle high", mutable.Buffer()),
        ("Middle", mutable.Buffer()),
        ("Middle low", mutable.Buffer()),
        ("Low", mutable.Buffer()),
        ("Sub", mutable.Buffer()))

      def addUi(track: String, start: Double, peak: Double, duration: Double, note: Int): Unit =
        uiBuilder.find(uib => uib._1 == track).map(_._2.append(UiNote(start, peak, duration, note)))

      if(reset) client.resetClock()

      def low1(start: Double): Unit = {
        var nextStart = start
        0 until 13 foreach {
          i =>
            val amp = randomRange(0.5, 0.75)
            val playDuration = playLow(nextStart, 0, amp)
            addUi("Low", nextStart, 0.5, playDuration, 0)

            if(i == 1) {
              middle1(nextStart + (playDuration * randomRange(0.90, 1.1)))
            } else if(i == 2) {
              sub1(nextStart + (playDuration * randomRange(0.90, 1.1)))
            } else if (i == 4) {
              secondThemeLow1(nextStart + (playDuration * randomRange(0.90, 1.1)))
            } else if (i == 8) {
              secondThemeMiddle1(nextStart + (playDuration * randomRange(0.90, 1.1)))
            } else if(i == 12) {
              secondSub1(nextStart + (playDuration * randomRange(0.66, 0.75)))
            }

            nextStart = nextStart + (playDuration * randomRange(0.66, 0.75))
        }
      }

      def sub1(start: Double): Unit = {
        var nextStart = start
        0 until 5 foreach {
          _ =>
            val amp = randomRange(0.5, 0.75)
            val playDuration = playSubBass(nextStart, 0, amp)
            addUi("Sub", nextStart, 0.5, playDuration, 0)
            nextStart = nextStart + (playDuration * randomRange(0.66, 0.75))
        }
      }

      def secondSub1(start: Double): Unit = {
        var nextStart = start
        val amp = randomRange(0.5, 0.75)
        val playDuration = playSubBass(nextStart, 0, amp)
        addUi("Sub", nextStart, 0.5, playDuration, 0)
        nextStart = nextStart + (playDuration * randomRange(0.66, 0.75))
      }

      def middle1(start: Double): Unit = {
        var nextStart = start
        0 until 13 foreach {
          i =>
            val amp = randomRange(0.5, 0.75)
            val playDuration = playMiddle(nextStart, 0, amp)
            addUi("Middle", nextStart, 0.5, playDuration, 0)

            if(i == 2) {
              middleHigh1(nextStart + (playDuration * randomRange(0.90, 1.1)))
            } else if(i == 11) {
              low2(nextStart + (playDuration * randomRange(0.75, 0.9)))
            }
            nextStart = nextStart + (playDuration * randomRange(0.66, 0.75))
        }
      }

      def secondThemeLow1(start: Double): Unit = {
        var nextStart = start
        val amp = randomRange(0.5, 0.75)
        var playDuration = playLow(nextStart, 8, amp)
        addUi("Low", nextStart, 0.5, playDuration, 8)
        nextStart = nextStart + (playDuration * randomRange(0.66, 0.75))
        playDuration = playLow(nextStart, 10, amp)
        addUi("Low", nextStart, 0.5, playDuration, 10)
      }

      def secondThemeMiddle1(start: Double): Unit = {
        var nextStart = start
        val amp = randomRange(0.5, 0.75)
        var playDuration1 = playLow(nextStart, 8, amp)
        addUi("Low", nextStart, 0.5, playDuration1, 8)
        var playDuration2 = playMiddle(nextStart, 8, amp)
        addUi("Middle", nextStart, 0.5, playDuration2, 8)

        nextStart = nextStart + (math.max(playDuration1, playDuration2) * randomRange(0.66, 0.75))
        playDuration1 = playLow(nextStart, 10, amp)
        addUi("Low", nextStart, 0.5, playDuration1, 10)
        playDuration2 = playMiddle(nextStart, 10, amp)
        addUi("Middle", nextStart, 0.5, playDuration2, 10)
      }

      def middleHigh1(start: Double): Unit = {
        var nextStart = start
        0 until 8 foreach {
          i =>
            val amp = randomRange(0.5, 0.75)
            val playDuration = playMiddleHigh(nextStart, 0, amp)
            addUi("Middle high", nextStart, 0.5, playDuration, 0)

            if (i == 1) {
              high1(nextStart + (playDuration * randomRange(0.9, 1.1)))
            }
            nextStart = nextStart + (playDuration * randomRange(0.66, 0.75))
        }
      }

      def high1(start: Double): Unit = {
        var nextStart = start
        0 until 5 foreach {
          _ =>
            val amp = randomRange(0.5, 0.75)
            val playDuration = playHigh(nextStart, 0, amp)
            addUi("High", nextStart, 0.5, playDuration, 0)
            nextStart = nextStart + (playDuration * randomRange(0.66, 0.75))
        }
      }

      //// Second ////

      def middle2(start: Double): Unit = {
        var nextStart = start
        0 until 8 foreach {
          i =>
            val amp = randomRange(0.5, 0.75)
            val playDuration = playMiddle(nextStart, 1, amp)
            addUi("Middle", nextStart, 0.5, playDuration, 1)

            if(i == 1) {
              middleHigh2(nextStart + (playDuration * randomRange(0.90, 1.1)))
            } else if(i == 2) {
              secondThemeMiddleHigh2(nextStart + (playDuration * randomRange(0.90, 1.1)))
            }
            nextStart = nextStart + (playDuration * randomRange(0.66, 0.75))
        }
      }

      def secondThemeMiddleHigh2(start: Double): Unit = {
        var nextStart = start
        val amp = randomRange(0.5, 0.75)
        var playDuration1 = playLow(nextStart, 8, amp)
        addUi("Low", nextStart, 0.5, playDuration1, 8)
        var playDuration2 = playMiddle(nextStart, 8, amp)
        addUi("Middle", nextStart, 0.5, playDuration2, 8)
        var playDuration3 = playMiddleHigh(nextStart, 8, amp)
        addUi("Middle high", nextStart, 0.5, playDuration3, 8)

        nextStart = nextStart + (Seq(playDuration1, playDuration2, playDuration3).max * randomRange(0.66, 0.75))
        playDuration1 = playLow(nextStart, 10, amp)
        addUi("Low", nextStart, 0.5, playDuration1, 10)
        playDuration2 = playMiddle(nextStart, 10, amp)
        addUi("Middle", nextStart, 0.5, playDuration2, 10)
        playDuration3 = playMiddleHigh(nextStart, 10, amp)
        addUi("Middle high", nextStart, 0.5, playDuration3, 10)
      }


      def secondThemeHigh2(start: Double): Unit = {
        var nextStart = start
        val amp = randomRange(0.5, 0.75)
        var playDuration1 = playLow(nextStart, 8, amp)
        addUi("Low", nextStart, 0.5, playDuration1, 8)
        var playDuration2 = playMiddle(nextStart, 8, amp)
        addUi("Middle", nextStart, 0.5, playDuration2, 8)
        var playDuration3 = playMiddleHigh(nextStart, 8, amp)
        addUi("Middle high", nextStart, 0.5, playDuration3, 8)
        var playDuration4 = playHigh(nextStart, 8, amp)
        addUi("High", nextStart, 0.5, playDuration4, 8)

        nextStart = nextStart + (Seq(playDuration1, playDuration2, playDuration3, playDuration4).max * randomRange(0.66, 0.75))
        playDuration1 = playLow(nextStart, 10, amp)
        addUi("Low", nextStart, 0.5, playDuration1, 10)
        playDuration2 = playMiddle(nextStart, 10, amp)
        addUi("Middle", nextStart, 0.5, playDuration2, 10)
        playDuration3 = playMiddleHigh(nextStart, 10, amp)
        addUi("Middle high", nextStart, 0.5, playDuration3, 10)
        playDuration4 = playHigh(nextStart, 10, amp)
        addUi("High", nextStart, 0.5, playDuration4, 10)

        val maxPlayDuration = Seq(playDuration1, playDuration2, playDuration3, playDuration4).max
        middleHigh3(nextStart + (maxPlayDuration * randomRange(0.90, 1.1)))
      }

      def middleHigh2(start: Double): Unit = {
        var nextStart = start
        0 until 8 foreach {
          i =>
            val amp = randomRange(0.5, 0.75)
            val playDuration = playMiddleHigh(nextStart, 1, amp)
            addUi("Middle high", nextStart, 0.5, playDuration, 1)
            if (i == 1) {
              high2(nextStart + (playDuration * randomRange(0.90, 1.1)))
            }
            nextStart = nextStart + (playDuration * randomRange(0.66, 0.75))
        }
      }

      def high2(start: Double): Unit = {
        var nextStart = start
        0 until 5 foreach {
          i =>
            val amp = randomRange(0.5, 0.75)
            val playDuration = playHigh(nextStart, 1, amp)
            addUi("High", nextStart, 0.5, playDuration, 1)
            if (i == 1) {
              secondThemeHigh2(nextStart + (playDuration * randomRange(0.90, 1.1)))
            }
            nextStart = nextStart + (playDuration * randomRange(0.66, 0.75))
        }
      }

      def low2(start: Double): Unit = {
        var nextStart = start
        0 until 13 foreach {
          i =>
            val amp = randomRange(0.5, 0.75)
            val playDuration = playLow(nextStart, 1, amp)
            addUi("Low", nextStart, 0.5, playDuration, 1)

            if (i == 1) {
              middle2(nextStart + (playDuration * randomRange(0.90, 1.1)))
            } else if(i == 2) {
              sub2(nextStart + (playDuration * randomRange(0.90, 1.1)))
            } else if(i == 12) {
              secondSub2(nextStart + (playDuration * randomRange(0.66, 0.75)))
            }
            nextStart = nextStart + (playDuration * randomRange(0.66, 0.75))
        }
      }

      def secondSub2(start: Double): Unit = {
        var nextStart = start
        val amp = randomRange(0.5, 0.75)
        val playDuration = playSubBass(nextStart, 1, amp)
        addUi("Sub", nextStart, 0.5, playDuration, 1)
        nextStart = nextStart + (playDuration * randomRange(0.66, 0.75))
      }

      def sub2(start: Double): Unit = {
        var nextStart = start
        0 until 5 foreach {
          _ =>
            val amp = randomRange(0.5, 0.75)
            val playDuration = playSubBass(nextStart, 1, amp)
            addUi("Sub", nextStart, 0.5, playDuration, 1)
            nextStart = nextStart + (playDuration * randomRange(0.66, 0.75))
        }
      }

      //// Third /////
      // middle high, low, middle, sub, high

      def middleHigh3(start: Double): Unit = {
        var nextStart = start
        0 until 8 foreach {
          i =>
            val amp = randomRange(0.5, 0.75)
            val playDuration = playMiddleHigh(nextStart, 0, amp)
            addUi("Middle high", nextStart, 0.5, playDuration, 0)
            if (i == 1) {
              low3(nextStart + (playDuration * randomRange(0.90, 1.1)))
            }
            nextStart = nextStart + (playDuration * randomRange(0.66, 0.75))
        }
      }

      def low3(start: Double): Unit = {
        var nextStart = start
        0 until 13 foreach {
          i =>
            val amp = randomRange(0.5, 0.75)
            val playDuration = playLow(nextStart, 0, amp)
            addUi("Low", nextStart, 0.5, playDuration, 0)
            if (i == 1) {
              middle3(nextStart + (playDuration * randomRange(0.90, 1.1)))
            }
            nextStart = nextStart + (playDuration * randomRange(0.66, 0.75))
        }
      }

      def middle3(start: Double): Unit = {
        var nextStart = start
        0 until 8 foreach {
          i =>
            val amp = randomRange(0.5, 0.75)
            val playDuration = playMiddle(nextStart, 0, amp)
            addUi("Middle", nextStart, 0.5, playDuration, 0)
            if (i == 0) {
              sub3(nextStart + (playDuration * randomRange(0.90, 1.1)))
            }
            nextStart = nextStart + (playDuration * randomRange(0.66, 0.75))
        }
      }

      def sub3(start: Double): Unit = {
        var nextStart = start
        0 until 5 foreach {
          i =>
            val amp = randomRange(0.5, 0.75)
            val playDuration = playSubBass(nextStart, 0, amp)
            addUi("Sub", nextStart, 0.5, playDuration, 0)

            if (i == 0) {
              secondThemeSub3(nextStart + (playDuration * randomRange(0.90, 1.1)))
            } else if (i == 1) {
              high3(nextStart + (playDuration * randomRange(0.90, 1.1)))
            }
            nextStart = nextStart + (playDuration * randomRange(0.66, 0.75))
        }
      }

      def secondThemeSub3(start: Double): Unit = {
        var nextStart = start
        val amp = randomRange(0.5, 0.75)
        var playDuration0 = playSubBass(nextStart, 8, amp)
        addUi("Sub", nextStart, 0.5, playDuration0, 8)
        var playDuration1 = playLow(nextStart, 8, amp)
        addUi("Low", nextStart, 0.5, playDuration1, 8)
        var playDuration2 = playMiddle(nextStart, 8, amp)
        addUi("Middle", nextStart, 0.5, playDuration2, 8)
        var playDuration3 = playMiddleHigh(nextStart, 8, amp)
        addUi("Middle high", nextStart, 0.5, playDuration3, 8)
        var playDuration4 = playHigh(nextStart, 8, amp)
        addUi("High", nextStart, 0.5, playDuration4, 8)

        nextStart = nextStart + (Seq(playDuration0, playDuration1, playDuration2, playDuration3, playDuration4).max * randomRange(0.66, 0.75))
        playDuration0 = playSubBass(nextStart, 10, amp)
        addUi("Sub", nextStart, 0.5, playDuration0, 10)
        playDuration1 = playLow(nextStart, 10, amp)
        addUi("Low", nextStart, 0.5, playDuration1, 10)
        playDuration2 = playMiddle(nextStart, 10, amp)
        addUi("Middle", nextStart, 0.5, playDuration2, 10)
        playDuration3 = playMiddleHigh(nextStart, 10, amp)
        addUi("Middle high", nextStart, 0.5, playDuration3, 10)
        playDuration4 = playHigh(nextStart, 10, amp)
        addUi("High", nextStart, 0.5, playDuration4, 10)

        nextStart = nextStart + (Seq(playDuration0, playDuration1, playDuration2, playDuration3, playDuration4).max * randomRange(0.66, 0.75))
      }

      def high3(start: Double): Unit = {
        var nextStart = start
        0 until 5 foreach {
          i =>
            val amp = randomRange(0.5, 0.75)
            val playDuration = playHigh(nextStart, 0, amp)
            addUi("High", nextStart, 0.5, playDuration, 0)
            if(i == 1) {
              secondThemeLast3(nextStart + (playDuration * randomRange(0.90, 1.1)))
            }
            nextStart = nextStart + (playDuration * randomRange(0.66, 0.75))
        }
      }

      def secondThemeLast3(start: Double): Unit = {
        var nextStart = start
        val amp = randomRange(0.5, 0.75)
        var playDuration0 = playLow(nextStart, 8, amp)
        addUi("Low", nextStart, 0.5, playDuration0, 8)
        var playDuration2 = playMiddle(nextStart, 8, amp)
        addUi("Middle", nextStart, 0.5, playDuration2, 8)
        var playDuration4 = playHigh(nextStart, 8, amp)
        addUi("High", nextStart, 0.5, playDuration4, 8)

        nextStart = nextStart + (Seq(playDuration0, playDuration2, playDuration4).max * randomRange(0.66, 0.75))
        playDuration0 = playLow(nextStart, 10, amp)
        addUi("Low", nextStart, 0.5, playDuration0, 10)
        playDuration2 = playMiddle(nextStart, 10, amp)
        addUi("Middle", nextStart, 0.5, playDuration2, 10)
        playDuration4 = playHigh(nextStart, 10, amp)
        addUi("High", nextStart, 0.5, playDuration4, 10)

        nextStart = nextStart + (Seq(playDuration0, playDuration2, playDuration4).max * randomRange(0.66, 0.75))

        val lowDuration = playLow(nextStart, 0, randomRange(0.5, 0.75))
        addUi("Low", nextStart, 0.5, lowDuration, 0)
      }

      low1(start)
      val uim = UiModel(uiBuilder.map {
        case (name, buffer) => (name, buffer.toSeq)
      })
      displayUiModel(uim)
    }

    case class UiNote(start: Double, peak: Double, duration: Double, note: Int)

    case class UiModel(tracks: Seq[(String, Seq[UiNote])]) {
      def getDuration: Double = {
        val foo: Seq[Double] = tracks.map {
          case (_, notes) => notes.map(note => note.start + note.duration).maxOption.getOrElse(0)
        }
        foo.max
      }
    }

    object PieceCanvas {
      val TRACK_HEIGHT = 100
      val NOTE_SCALE_FACTOR = 20
      val HEIGHT_INDENT = 80
    }

    case class PieceCanvas(uiModel: UiModel) extends JPanel {
      import PieceCanvas._

      override def getPreferredSize = {
        val width = (100 + (uiModel.getDuration * PieceCanvas.NOTE_SCALE_FACTOR)).toInt
        val height = (100 * uiModel.tracks.size) + 80
        new Dimension(width, height)
      }

      override def paintComponent(g: Graphics): Unit = {
        super.paintComponent(g)
        val rulers = (uiModel.getDuration / 10.0).toInt
        (0 until rulers).map(_ * 10).foreach {
          rule =>
            g.drawString(
              s"$rule",
              100 + (rule * NOTE_SCALE_FACTOR),
              20)

            g.drawLine(
              100 + (rule * NOTE_SCALE_FACTOR),
              30,
              100 + (rule * NOTE_SCALE_FACTOR),
              35
            )
        }

        uiModel.tracks.zipWithIndex.foreach {
          case ((trackName, notes), trackIndex) =>
            g.drawString(trackName, 20, (trackIndex * TRACK_HEIGHT) + HEIGHT_INDENT)
            notes.foreach {
              case UiNote(start: Double, peak: Double, duration: Double, note: Int) =>
                g.drawLine(
                  (100 + (start * NOTE_SCALE_FACTOR)).toInt,
                  (trackIndex * TRACK_HEIGHT) - (note * 5) + HEIGHT_INDENT,
                  (100 + ((start + (duration * peak)) * NOTE_SCALE_FACTOR)).toInt,
                  (trackIndex * TRACK_HEIGHT) - (note * 5) + HEIGHT_INDENT - 5)
                g.drawLine(
                  (100 + ((start + (duration * peak)) * NOTE_SCALE_FACTOR)).toInt,
                  (trackIndex * TRACK_HEIGHT) - (note * 5) + HEIGHT_INDENT - 5,
                  (100 + ((start + duration) * NOTE_SCALE_FACTOR)).toInt,
                  (trackIndex * TRACK_HEIGHT) - (note * 5) + HEIGHT_INDENT)
            }
        }
      }
    }

    def displayUiModel(uiModel: UiModel): Unit = {
      val duration = uiModel.getDuration
      val width = (100 + (duration * PieceCanvas.NOTE_SCALE_FACTOR)).toInt
      val height = (100 * uiModel.tracks.size) + 80
      println(s"duration $duration width $width")
      println(s"tracks ${uiModel.tracks.size} height $height")
      SwingUtilities.invokeLater(
        () => {
          val frame = new JFrame()
          frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
          val canvas = PieceCanvas(uiModel)
          canvas.setSize(width, height)
          val panel = new JScrollPane(canvas)
          panel.setAutoscrolls(true)

          frame.getContentPane.add(panel)
          frame.setSize(math.min(4000, width), height)
          frame.pack()
          frame.setVisible(true)
        }
      )

    }

    def playSubBass(start: Double, note: Int, amp: Double): Double = {
      val duration = randomRange(8, 21)
      val attackTime = randomRange(0.33, 0.66)
      synthPlayer()
        .sineFm(staticControl(lowerSpectrum(note)),
          staticControl(lowerSpectrum(note) * fact),
          staticControl(randomRange(10, 20)),
          relativePercControl(0.001, amp, attackTime, Left(Seq(0, 0))))
        .pan(staticControl(randomRange(-0.4, 0.4)))
        .playWithDuration(start, duration)

      duration
    }

    def playLow(start: Double, note: Int, amp: Double): Double = {
      pickItems(Seq(0, 1, 2, 3), 2).map {
        case 0 =>
          withLowpass(start, note, amp, spectrum.head)
        case low =>
          withBandpass(start, note, amp, spectrum(low), spectrum(low + 1))
      }.max
    }

    def playMiddle(start: Double, note: Int, amp: Double): Double = {
      pickItems(Seq(4, 5, 6, 7, 8), 3).map {
        low =>
          withBandpass(start, note, amp, spectrum(low), spectrum(low + 1))
      }.max
    }

    def playMiddleHigh(start: Double, note: Int, amp: Double): Double = {
      pickItems(Seq(10, 11, 12, 13, 14, 15, 16, 17), 3).map {
        low =>
          withBandpass(start, note, amp, spectrum(low), spectrum(low + 1))
      }.max
    }

    def playHigh(start: Double, note: Int, amp: Double): Double = {
      pickItems(Seq(19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 36, 37), 3).map {
        low =>
          withBandpass(start, note, amp, spectrum(low), spectrum(low + 1))
      }.max
    }

    def withBandpass(start: Double, note: Int, amp: Double, low: Double, high: Double): Double = {
      val modStart = randomRange(1000, 40000)
      val modPeak = randomRange(1000, 40000)

      val carrier = spectrum(note)
      val modulator = carrier * fact
      val attackTime = randomRange(0.33, 0.66)
      val panStart = randomRange(-0.9, 0.9)
      val panEnd = randomRange(-0.9, 0.9)

      //val duration = randomRange(5, 13)
      val duration = randomRange(8, 13)

      synthPlayer()
        .sawFm(staticControl(carrier),
          staticControl(modulator),
          lineControl(modStart, modPeak),
          relativePercControl(0.001, amp, attackTime, Left(Seq(0, 0))))
        .highPass(staticControl(low))
        .lowPass(staticControl(high))
        .pan(lineControl(panStart, panEnd))
        .playWithDuration(start, duration)

      synthPlayer()
        .sineFm(staticControl(carrier),
          staticControl(modulator),
          lineControl(modPeak, modStart),
          relativePercControl(0.001, amp, attackTime, Left(Seq(0, 0))))
        .highPass(staticControl(low))
        .lowPass(staticControl(high))
        .pan(lineControl(panEnd, panStart))
        .playWithDuration(start, duration)

      duration
    }

    def withLowpass(start: Double, note: Int, amp: Double, lowPass: Double): Double = {
      val modStart = randomRange(1000, 40000)
      val modPeak = randomRange(1000, 40000)

      val carrier = spectrum(note)
      val modulator = carrier * fact
      val attackTime = randomRange(0.33, 0.66)
      val panStart = randomRange(-0.9, 0.9)
      val panEnd = randomRange(-0.9, 0.9)

      //val duration = randomRange(5, 13)
      val duration = randomRange(8, 13)

      synthPlayer()
        .sawFm(staticControl(carrier),
          staticControl(modulator),
          lineControl(modStart, modPeak),
          relativePercControl(0.001, amp, attackTime, Left(Seq(0, 0))))
        .lowPass(staticControl(lowPass))
        .pan(lineControl(panStart, panEnd))
        .playWithDuration(start, duration)

      synthPlayer()
        .sineFm(staticControl(carrier),
          staticControl(modulator),
          lineControl(modPeak, modStart),
          relativePercControl(0.001, amp, attackTime, Left(Seq(0, 0))))
        .lowPass(staticControl(lowPass))
        .pan(lineControl(panEnd, panStart))
        .playWithDuration(start, duration)

      duration
    }
  }

  /**
   * First iteration. e3 and fiss4
   * Theme c and giss aiss
   * mix sineFm and sawFm
   * line with high mod amount
   * line with pan
   */
  object FmHarmony1Patch extends Patch {
    override def noteHandle(start: Double, key: Int, velocity: Int, device: String): Unit = {
      val note = key % 12
      val octave = (key / 12) - 1
      val amp = velocity / 127.0

      // c, c# 164.8138003156881 giss 1806.2591720621701 aiss 2216.620514998791
      // double with c and ciss on the lower instrument

      val fundamentalNote = "e3"
      val firstPartialNote = "fiss4"

      val fundamental = Note.noteToHertz(fundamentalNote)
      val firstPartial = Note.noteToHertz(firstPartialNote)

      val fact = Spectrum.makeFact(fundamental, firstPartial)
      val spectrum = Spectrum.makeSpectrum2(fundamental, fact, 50)

      val invertedFact = fundamental / firstPartial
      val lowerSpectrum = Spectrum.makeSpectrum2(fundamental / math.pow(fact, 7), invertedFact, 50)
      println(s"Lower Spectrum $lowerSpectrum")

      println(s"Spectrum $spectrum")

      val modStart = randomRange(1000, 40000)
      val modPeak = randomRange(1000, 40000)

      val carrier = spectrum(note)
      val modulator = carrier * fact
      val attackTime = randomRange(0.33, 0.66)
      val panStart = randomRange(-0.9, 0.9)
      val panEnd = randomRange(-0.9, 0.9)

      octave match {
        case 2 =>
          val duration = randomRange(8, 21)
          synthPlayer()
            .sineFm(staticControl(lowerSpectrum(note)),
              staticControl(lowerSpectrum(note) * fact),
              staticControl(randomRange(10, 20)),
              relativePercControl(0.001, amp, attackTime, Left(Seq(0, 0))))
            .pan(staticControl(randomRange(-0.4, 0.4)))
            .playWithDuration(start, duration)
        case 3 =>
          val duration = randomRange(5, 13)
          println(s"Fm carrier $carrier modulator $modulator mod amount $modStart -> $modPeak fundamental $fundamental $fundamentalNote first partial $firstPartial $firstPartialNote fact $fact")
          synthPlayer()
            .sawFm(staticControl(spectrum(note)),
              staticControl(spectrum(note) * fact),
              lineControl(modStart, modPeak),
              relativePercControl(0.001, amp, attackTime, Left(Seq(0, 0))))
            .pan(lineControl(panStart, panEnd))
            .playWithDuration(start, duration)

          synthPlayer()
            .sineFm(staticControl(spectrum(note)),
              staticControl(spectrum(note) * fact),
              lineControl(modPeak, modStart),
              relativePercControl(0.001, amp, attackTime, Left(Seq(0, 0))))
            .pan(lineControl(panEnd, panStart))
            .playWithDuration(start, duration)
      }
    }
  }

  def stop(): Unit = {
    println("Stopping SuperCollider client")
    client.stop()
    this.superColliderReceiver.stop()
  }
}
