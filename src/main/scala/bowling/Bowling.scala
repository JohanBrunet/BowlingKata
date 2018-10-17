package bowling

object Bowling {
    def start() = Game(List())
}

case class Game(frames: List[Frame]) {

    def newFrame(firstRoll: Int, secondRoll: Int): Game = {
        if(firstRoll == 10) this.copy(frames = addFrame((new Frame(10, 0, "strike"))))
        else if((firstRoll + secondRoll) == 10) this.copy(frames = addFrame(new Frame(10, 0, "strike")))
        else this.copy(frames = addFrame(new Frame(firstRoll, secondRoll, "regular")))
    }

    def addFrame(frame: Frame): List[Frame] = this.frames :+ frame

    def computeScore(score: Int, frames: List[Frame]): Int = {
        if(frames.isEmpty) score
        else {
            val currentFrame = frames.head
            currentFrame.frameType match {
                case "strike" => {
                    val nextFrame = frames(1)
                    nextFrame.frameType match {
                        case "strike" => {
                            val nextNextFrame = frames(2)
                            nextNextFrame.frameType match {
                                case "strike" => {
                                    val newScore = score + currentFrame.roll1 
                                                         + nextFrame.roll1 
                                                         + nextNextFrame.roll1
                                    computeScore(newScore, frames.tail)                                 
                                }
                                case "spare" => {
                                    val newScore = score + currentFrame.roll1 + (nextFrame.roll1 + nextNextFrame.roll1)
                                                         + (nextFrame.roll1 + nextNextFrame.roll1 + nextNextFrame.roll2)
                                    computeScore(newScore, frames.takeRight(frames.size - 2)) 
                                }
                                case _ => {
                                    val newScore = score + currentFrame.roll1 + (nextFrame.roll1 + nextNextFrame.roll1)
                                                         + (nextFrame.roll1 + nextNextFrame.roll1 + nextNextFrame.roll2)
                                                         + (nextNextFrame.roll1 + nextNextFrame.roll2)
                                    computeScore(newScore, frames.takeRight(frames.size - 3)) 
                                }
                            }
                        }
                        case "spare" => {
                            val newScore = score + currentFrame.roll1 + nextFrame.roll1 + nextFrame.roll2
                            computeScore(newScore, frames.tail)
                        }
                        case _ => {
                            val newScore = score + currentFrame.roll1 + (nextFrame.roll1 + nextFrame.roll2) * 2
                            computeScore(newScore, frames.takeRight(frames.size - 2))
                        }
                    }
                }
                case "spare" => {
                    val nextFrame = frames(1)
                    val newScore = score + currentFrame.roll1 
                                         + currentFrame.roll2
                                         + nextFrame.roll1 
                    computeScore(newScore, frames.tail)
                }
                case _ => {
                    val newScore = score + currentFrame.roll1 + currentFrame.roll2
                    computeScore(score, frames.tail)
                }
            }
        }
    }
}
