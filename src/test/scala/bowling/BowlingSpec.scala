package bowling

import org.scalatest.{FunSpec, Matchers}

class BowlingSpec extends FunSpec with Matchers {

    //def rollIntoGutter(): Int = 0
    val game : Game = Bowling.start()
    val listFrameNull : List[Frame] = List.fill(10)(Frame(0,0,"regular"))
    val listFrame1 : List[Frame] = List.fill(10)(Frame(1,1,"regular"))

    def addAllFrame(game: Game, list : List[Frame]) : Game = {
        if(list.isEmpty) game
        else addAllFrame(game.copy(frames = game.addFrame(list.head)),list.tail)
    }

    val gameFrameNull = addAllFrame(game, listFrameNull)
    val gameFrame1 = addAllFrame(game, listFrame1)

    describe("Bowling game score") {
        it("should be 0 when all rolls go into the gutter") {
            assert(gameFrameNull.computeScore(0, gameFrameNull.frames) == 0)
        }

        it("should be 20 when all rolls are 1") {
            assert(gameFrame1.computeScore(0, gameFrame1.frames) == 20)
        }
    }
}
