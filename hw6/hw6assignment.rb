# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
    # class array holding all the pieces and their rotations
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
               Piece::rotations([[-1, 0], [0, 0], [-1, 1], [0, -1], [1, -1]], # squarT
              )]
  # All_My_Pieces.push([[0, 0], [0, 1], [0, 2], [0, 3], [0, 4]])
  # All_My_Pieces.push([[-2, 0], [-1, 0], [0, 0], [1, 0], [2, 0]])
  # All_My_Pieces.push([[0, 0], [0, 1], [1, 1]])

  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end
end

# can derived class affect parent class's fields?
# if so how can I affect it?
# class inherits method definitions but how about fields?
class MyBoard < Board
  # your enhancements here
  def initialize (game)
    super(game)
    @current_block = MyPiece.next_piece(self)
  end
  
end

class MyTetris < Tetris
  # your enhancements here
  # creates a canvas and the board that interacts with it
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings  
    super
    @root.bind('u', proc {
                    @board.rotate_counter_clockwise
                    @board.rotate_counter_clockwise})
  end

end


