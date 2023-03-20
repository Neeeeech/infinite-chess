class Piece( var pos		   : Piece.Coord,// (x,y) - (0,0) is bottom-left corner & positive is right & up
			 val isWhite       : Boolean,	 // white faces up
			 val movesDiag     : Boolean,	 // whether piece moves along diagonals
			 val movesStraight : Boolean,	 // whether piece moves along
			 val limited       : Boolean,	 // whether piece can only move one tile at a time
			 val knightLike    : Boolean,	 // whether piece moves like a knight
			 val pawnLike      : Boolean ) { // whether piece moves like a pawn
	private type Coord = Piece.Coord
	private var moved : Boolean = false 	 // whether pieces has moved yet; for castling, pawns
	
	/* Determines whether a destination coord is reachable based on the piece's move set (doesn't care about board)
	 * TESTED - TestReachablePaths.scala, but you need to unprotect this method first
	 * pre:  pos != des
	 * post: pos == pos_0  &&  returns whether des is reachable and the path type */
	def reachable( des : Coord ) : Piece.PathType = {
		require(pos != des)
		if ( movesDiag && (des._1 - pos._1).abs == (des._2 - pos._2).abs ) {
			// diagonal
			if ( !limited ) {
				// can go very far
				return Piece.DiagFar
			} else if ( ( des._1 - pos._1 ).abs == 1 ) {
				if ( !pawnLike ) {
					// within range but not pawn
					return Piece.DiagNear
				} else if ( ( des._2 - pos._2 == 1 ) == isWhite) {
					// is pawn & going the right direction
					return Piece.PawnNom
				}
			}
		} else if ( movesStraight && (des._1 == pos._1 || des._2 == pos._2) ) {
			// straight
			if ( !limited ) {
				// can go very far
				return Piece.StraightFar
			} else if ( !pawnLike && (( des._1 - pos._1 ).abs == 1 || ( des._2 - pos._2 ).abs == 1 )) {
				// king-like
				return Piece.StraightNear
			} else if ( pawnLike && des._1 == pos._1 && ((des._2 - pos._2) == 1) == isWhite && (des._2 - pos._2).abs == 1) {
				// pawn walk in the right dir.
				return Piece.PawnWalk
			} else if ( pawnLike && (des._2 - pos._2).abs == 2 && ((des._2 - pos._2 == 2) == isWhite && !moved)) {
				// the fancy leap at the start
				return Piece.PawnLeap
			}
		} else if ( knightLike && Piece.knightWalks.contains((des._1 - pos._1, des._2 - pos._2)) ) {
			// knight-like
			return Piece.LWalk
		}
		null
	}
	
	/* Determines whether the path to a destination coord is clear
	 * pre:  pos != des
	 * post: (( pathtype == null && returns true )
	 *     || ( pathtype != null && returns whether there are obstacles in the way ))
	 *    && s = s_0 */
	def blocked( des : Coord, pathtype : Piece.PathType, pieces : Array[Piece] ) : Boolean = {
		require(pos != des)
		val minx = pos._1.min(des._1); val maxx = pos._1.max(des._1)
		val miny = pos._2.min(des._2); val maxy = pos._2.max(des._2)
		pathtype match {
			case Piece.DiagFar =>
				for (p <- pieces) {
					if ((des == p.pos && this.isWhite == p.isWhite) || // blocked by own piece
							((pos._2 - des._2)*(pos._1 - p.pos._1) == (pos._2 - p.pos._2)*(pos._1 - des._1) &&
								minx < p.pos._1 && p.pos._1 < maxx)) // or somewhere in between
						return true
				}
				false
			
			case Piece.StraightFar =>
				for (p <- pieces)
					if ((minx < p.pos._1 && p.pos._1 < maxx && miny == p.pos._2 && p.pos._2 == maxy) ||
					    (minx == p.pos._1 && p.pos._1 == maxx && miny < p.pos._2 && p.pos._2 < maxy) ||
						(des == p.pos && this.isWhite == p.isWhite))
						return true
				false
			
			case Piece.DiagNear | Piece.StraightNear | Piece.LWalk | Piece.PawnWalk =>
				for (p <- pieces) if (des == p.pos && (pawnLike || (isWhite == p.isWhite)))
					return true
				false
			
			case Piece.PawnNom =>
				for (p <- pieces)
					if (p.pos == des && this.isWhite != p.isWhite)
						return false
				true
			
			case Piece.PawnLeap =>
				for (p <- pieces
					 if p != this
					 if p.pos._1 == pos._1
					 if p.pos._2 - pos._2 <= 2)
					return true
				false
			
			case null => true
		}
	}
	
	/* Checks whether the piece can move to des, given the other pieces
	 * pre:  empty
	 * post: returns whether you can move it */
	private def freeToMove( des : Coord, pieces : Array[Piece] ) : Boolean = {
		if ( pos == des || blocked(des, reachable(des), pieces) )
			return false
		// checking if move puts yourself in check; compatible w/ multiple kings
//		for (king <- pieces; if king.isInstanceOf[Piece.King] && king.isWhite == isWhite)
//			for (opp <- pieces; if opp.isWhite != isWhite && opp.freeToMove(king.pos, pieces))
//				return false
		true
	}
	
	def move( des : Coord, pieces : Array[Piece] ) : Boolean = {
		if (!freeToMove(des, pieces))
			return false
		for (i <- pieces.indices) if (pieces(i).pos == des) {
			pieces(i) = null
		}
		pos = des
		true
	}
	
	override def toString: String = s"${this.getClass.getSimpleName}: $pos"
}

object Piece {
	private type Coord = (Int, Int)
	class King(pos : Coord, isWhite : Boolean) extends Piece(pos, isWhite, true, true, true, false, false)
	class Queen(pos : Coord, isWhite : Boolean) extends Piece(pos, isWhite, true, true, false, false, false)
	class Rook(pos : Coord, isWhite : Boolean) extends Piece(pos, isWhite, false, true, false, false, false)
	class Bishop(pos : Coord, isWhite : Boolean) extends Piece(pos, isWhite, true, false, false, false, false)
	class Knight(pos : Coord, isWhite : Boolean) extends Piece(pos, isWhite, false, false, false, true, false)
	class Pawn(pos : Coord, isWhite : Boolean) extends Piece(pos, isWhite, true, true, true, false, true)
	
	protected val knightWalks: Array[ (Int, Int) ] =
		Array((1,2), (2,1), (2,-1), (1,-2), (-1,-2), (-2,-1), (-2,1), (-1,2))
	
	trait PathType
	case object DiagFar extends PathType
	case object DiagNear extends PathType
	case object StraightFar extends PathType
	case object StraightNear extends PathType
	case object LWalk extends PathType
	case object PawnNom extends PathType
	case object PawnWalk extends PathType
	case object PawnLeap extends PathType
}