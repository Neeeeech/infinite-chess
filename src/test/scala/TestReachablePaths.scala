// BELOW TESTS REQUIRE REACHABLE NOT BE PROTECTED
class TestReachablePaths extends org.scalatest.funsuite.AnyFunSuite {
	def assertPath(isMatch : Boolean, expectedPath : Piece.PathType, testPath : Piece.PathType) : Unit = {
		if (isMatch)
			assert(testPath === expectedPath)
		else
			assert(testPath === null)
	}
	
	// pre:  piece is in position (2,0) && piece is not a pawn (test pawns separately)
	// post: checks whether the piece works as expected given the Booleans of what it should be able to do
	def testPath(piece : Piece,
				 rightStep : Piece.PathType,
				 frontStep : Piece.PathType,
				 leftLeap  : Piece.PathType,
				 backLeap  : Piece.PathType,
				 frontLeap : Piece.PathType,
				 diagStep  : Piece.PathType,
				 diagLeap  : Piece.PathType,
				 lStep     : Piece.PathType,
				 backDiag  : Piece.PathType
				) : Unit = {
		assert(piece.pos == (2,0))
		assert(piece.reachable(3,0)  === rightStep)
		assert(piece.reachable(2,1)  === frontStep)
		assert(piece.reachable(0,0)  === leftLeap)
		assert(piece.reachable(2,-2) === backLeap)
		assert(piece.reachable(2,2)  === frontLeap)
		assert(piece.reachable(1,1)  === diagStep)
		assert(piece.reachable(4,2)  === diagLeap)
		assert(piece.reachable(4,1)  === lStep)
		assert(piece.reachable(3,-1) === backDiag)
		assert(piece.reachable(5,1)  === null)
	}
	
	test("finding path type - King") {
		val p = new Piece.King((2,0), true)
		testPath(p, Piece.StraightNear, Piece.StraightNear, null, null, null,
				 Piece.DiagNear, null, null, Piece.DiagNear)
	}
	
	test("finding path type - Queen") {
		val p = new Piece.Queen( (2, 0), true )
		testPath( p, Piece.StraightFar, Piece.StraightFar, Piece.StraightFar, Piece.StraightFar, Piece.StraightFar,
			Piece.DiagFar, Piece.DiagFar, null, Piece.DiagFar )
	}
	
	test("finding path type - Rook") {
		val p = new Piece.Rook((2,0), true)
		testPath(p, Piece.StraightFar, Piece.StraightFar, Piece.StraightFar, Piece.StraightFar, Piece.StraightFar,
			null, null, null, null)
	}
	
	test("finding path type - Bishop") {
		val p = new Piece.Bishop((2,0), true)
		testPath(p, null, null, null, null, null,
				 Piece.DiagFar, Piece.DiagFar, null, Piece.DiagFar)
	}
	
	test("finding path type - Knight") {
		val p = new Piece.Knight((2,0), true)
		testPath(p, null, null, null, null, null,
			null, null, Piece.LWalk, null)
	}
	
	test("finding path type - Pawn White") {
		val p = new Piece.Pawn((2,0), true)
		testPath(p, null, Piece.PawnWalk, null, null, Piece.PawnLeap,
			Piece.PawnNom, null, null, null)
		assert(p.reachable((2,-1)) === null) // the backwards step I forgot to add to testPath and cba to add
	}
	
	test("finding path type - Pawn Black") {
		val p = new Piece.Pawn((2,0), false)
		testPath(p, null, null, null, Piece.PawnLeap, null,
			null, null, null, Piece.PawnNom)
		assert(p.reachable((2,-1)) === Piece.PawnWalk) // the backwards step I forgot to add to testPath and cba to add
	}
}
