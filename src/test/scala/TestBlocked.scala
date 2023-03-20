// Assumes reachable works as intended
class TestBlocked extends org.scalatest.funsuite.AnyFunSuite {
	// for each position test:
	// - unblocked entirely
	// - taking a piece at destination
	// - blocked at destination by teammate
	// - blocked by a piece in the middle
	
	private type Coord = (Int, Int)
	
	// Takes a piece, destination, and block_pos of where a blocking piece would be
	def testPos(p : Piece, des : Coord, block_pos : Coord = null) : Unit = {
		val pathType = p.reachable(des)
		if (pathType == null) {
			// if the piece can't make it at all, then we just test that it's blocked and move on
			assert(p.blocked(des, pathType, Array[Piece](p)))
			return
		}
		// pathType != null
		
		// unblocked entirely
		if (p.pawnLike && (pathType == Piece.PawnNom))	// can't move, bc no piece to take
			assert( p.blocked(des, p.reachable(des), Array[Piece](p)))
		else
			assert(!p.blocked(des, p.reachable(des), Array[Piece](p)))
		
		// taking a piece at destination
		if (!p.pawnLike || (pathType == Piece.PawnNom))    // you can actually take a piece
			assert(!p.blocked(des, p.reachable(des), Array[Piece](p, new Piece.Pawn(des, !p.isWhite))))
		else 	// can't take a piece because you're a pawn going straight
			assert( p.blocked(des, p.reachable(des), Array[Piece](p, new Piece.Pawn(des, !p.isWhite))))
			
		
		// blocked at destination by teammate
		assert(p.blocked(des, p.reachable(des), Array[Piece](p, new Piece.Pawn(des,p. isWhite))))
		
		// blocked by a piece in the middle
		if (block_pos != null)
			assert(p.blocked(des, p.reachable(des), Array[Piece](p, new Piece.Pawn(block_pos, p.isWhite))))
	}
	
	// assuming the piece is in (2,0)
	val coords : Array[((Int, Int), (Int, Int))] = Array[((Int, Int), (Int, Int))](
		// the order's kinda weird so that it's consistent with the other test
		// which is in the order in which I came up with it
		// but it should cover all the cases, including pawn directional ones
		((3,0),  null),		// right step
		((2,1),  null),		// forward step
		((0,0),  (1,0)),	// left leap
		((2,-2), (2,-1)),	// backwards leap
		((2,2),  (2,1)),	// forwards leap
		((1,1),  null),		// diag step forward
		((4,2),  (3,1)),	// diag leap
		((4,1),  null),		// l step
		((3,-1), null),		// diag step backward
		((2,-1), null)		// backwards step
	)
	
	// Takes an array of test coordinates and runs testPos on them on the given piece
	def testPiece(p : Piece) : Unit =
		for ((des, block_pos) <- coords)
			testPos(p, des, block_pos)
	
	test("checking possible path - King") 	  { testPiece(new Piece.King((2,0), true)) }
	test("checking possible path - Queen") 	  { testPiece(new Piece.Queen((2,0), true)) }
	test("checking possible path - Rook") 	  { testPiece(new Piece.Rook((2,0), true)) }
	test("checking possible path - Bishop") 	  { testPiece(new Piece.Bishop((2,0), true)) }
	test("checking possible path - Knight") 	  { testPiece(new Piece.Knight((2,0), true)) }
	test("checking possible path - Pawn White") { testPiece(new Piece.Pawn((2,0), true)) }
	test("checking possible path - Pawn Black") { testPiece(new Piece.Pawn((2,0), false)) }
	
}
